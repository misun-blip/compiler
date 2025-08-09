//
// Created by kaixuan on 2025/7/7.
//

#include "CodeGenerator.h"
#include <stdexcept>
#include <algorithm>
#include <set>
#include <functional>

void CodeGenerator::generate(const std::shared_ptr<CompUnitNode>& root) {
    // 初始化标记
    in_continue_statement = false;
    label_counter = 0;

    // 生成数据段
    out << ".data\n\n";

    // 生成代码段
    out << ".text\n";
    out << ".global main\n\n";

    // 生成代码
    root->accept(*this);
}

void CodeGenerator::visit(CompUnitNode& node) {
    for (const auto& func : node.funcDefs) {
        func->accept(*this);
    }
}

// 新增：计算函数体中最大调用实参个数
int CodeGenerator::computeMaxCallArgs(const std::shared_ptr<BlockNode>& block) const {
    int max_args = 0;
    std::function<void(const std::shared_ptr<StmtNode>&)> walk = [&](const std::shared_ptr<StmtNode>& stmt) {
        if (!stmt) return;

        // 处理函数调用语句
        if (const auto call_stmt = std::dynamic_pointer_cast<CallStmtNode>(stmt)) {
            if (const auto func_call = std::dynamic_pointer_cast<FuncCallExprNode>(call_stmt->call)) {
                max_args = std::max(max_args, (int)func_call->args.size());
            }
        }
        // 处理表达式语句中的函数调用
        else if (const auto expr_stmt = std::dynamic_pointer_cast<ExprStmtNode>(stmt)) {
            if (const auto func_call = std::dynamic_pointer_cast<FuncCallExprNode>(expr_stmt->expr)) {
                max_args = std::max(max_args, (int)func_call->args.size());
            }
        }
        // 处理return语句中的函数调用
        else if (const auto return_stmt = std::dynamic_pointer_cast<ReturnStmtNode>(stmt)) {
            if (return_stmt->retExpr) {
                if (const auto func_call = std::dynamic_pointer_cast<FuncCallExprNode>(return_stmt->retExpr)) {
                    max_args = std::max(max_args, (int)func_call->args.size());
                }
            }
        }
        // 递归处理块语句
        else if (const auto block_stmt = std::dynamic_pointer_cast<BlockNode>(stmt)) {
            for (const auto& s : block_stmt->stmts) walk(s);
        }
        // 处理if语句
        else if (const auto if_stmt = std::dynamic_pointer_cast<IfStmtNode>(stmt)) {
            walk(if_stmt->thenStmt);
            walk(if_stmt->elseStmt);
        }
        // 处理while语句
        else if (const auto while_stmt = std::dynamic_pointer_cast<WhileStmtNode>(stmt)) {
            walk(while_stmt->body);
        }
    };

    for (const auto& stmt : block->stmts) {
        walk(stmt);
    }
    return max_args;
}

void CodeGenerator::visit(FuncDefNode& node) {
    current_function = node.id;
    stack_offset = 0;
    temp_offset = 0;

    reg_alloc.reset();
    while (!spill_stack.empty()) {
        spill_stack.pop();
    }

    out << node.id << ":\n";
    sym_table.enterScope();

    // 1. 计算最大调用实参个数（用于预留出参区）
    const int max_call_args = computeMaxCallArgs(node.block);

    // 2. 计算各区域空间
    const int frame_space = 8;  // ra(4B) + fp(4B)
    const int param_space = static_cast<int>(node.params.size()) * 4;
    const int local_space = calculateLocalSpace(node.block);
    int temp_space = calculateTempSpaceNeed(node.block);

    // 预留调用出参区（超过8个的参数用）
    const int outgoing_space = std::max(0, (max_call_args - 8)) * 4;

    // 3. 计算总帧大小并对齐
    int frame_size = param_space + local_space + temp_space + outgoing_space + frame_space;
    alignStack(frame_size);
    current_function_stack_size = frame_size;

    // 4. 生成序言（仅此处调整sp，函数体内不再修改）
    emitPrologue(frame_size);

    // 5. 处理形参（全部存入当前栈帧，符号表记录本地偏移）
    int current_off = 0;
    for (size_t i = 0; i < node.params.size(); ++i) {
        const std::string& param_id = node.params[i]->id;
        const int local_off = current_off;

        if (i < 8) {
            // 前8个参数从寄存器存入栈帧
            if (local_off >= -2048 && local_off < 2048) {
                out << "    sw a" << i << ", " << local_off << "(sp)\n";
            } else {
                std::string addr_reg = allocateRegister();
                out << "    li " << addr_reg << ", " << local_off << "\n";
                out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
                out << "    sw a" << i << ", 0(" << addr_reg << ")\n";
                freeRegister(addr_reg);
            }
        } else {
            // 超过8个的参数从调用者栈帧读取后存入当前栈帧
            const int incoming_off = frame_size + (i - 8) * 4;  // 调用者传入的栈参数偏移
            std::string temp_reg = allocateRegister();

            if (incoming_off >= -2048 && incoming_off < 2048) {
                out << "    lw " << temp_reg << ", " << incoming_off << "(sp)\n";
            } else {
                std::string addr_reg = allocateRegister();
                out << "    li " << addr_reg << ", " << incoming_off << "\n";
                out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
                out << "    lw " << temp_reg << ", 0(" << addr_reg << ")\n";
                freeRegister(addr_reg);
            }

            if (local_off >= -2048 && local_off < 2048) {
                out << "    sw " << temp_reg << ", " << local_off << "(sp)\n";
            } else {
                std::string addr_reg = allocateRegister();
                out << "    li " << addr_reg << ", " << local_off << "\n";
                out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
                out << "    sw " << temp_reg << ", 0(" << addr_reg << ")\n";
                freeRegister(addr_reg);
            }
            freeRegister(temp_reg);
        }

        sym_table.addSymbol(param_id, local_off, true, static_cast<int>(i));
        current_off += 4;
    }

    // 6. 设置各区域边界
    stack_offset = param_space;  // 局部变量起始偏移
    temp_area_start = stack_offset + local_space;  // 临时区起始偏移

    // 关键修复：临时区结束位置应该在出参区之前，留有足够空间
    // 不要让temp_area_end等于current_out_arg_base
    temp_area_end = temp_area_start + temp_space;  // 临时区结束位置

    // 出参区基址（在栈帧的最高地址处）
    current_out_arg_base = frame_size;

    // 确保临时区不会和出参区重叠
    if (temp_area_end > frame_size - outgoing_space - frame_space) {
        // 如果临时区太大，调整它
        temp_area_end = frame_size - outgoing_space - frame_space - 8; // 留8字节安全边界
    }

    // 7. 生成函数体
    node.block->accept(*this);

    // 8. main函数默认返回0
    if (node.id == "main") {
        out << "    li a0, 0\n";
    }

    // 9. 生成尾声标签和尾声
    out << current_function << "_epilogue:\n";
    emitEpilogue(frame_size);

    sym_table.exitScope();
    out << "\n";
}
// 计算局部变量的空间需求
int CodeGenerator::calculateLocalSpace(const std::shared_ptr<BlockNode>& block) {
    int space = 0;
    std::set<std::string> declared_vars;    // 记录声明过的变量

    std::function<void(const std::shared_ptr<BlockNode>&)> calculateSpace =
        [&](const std::shared_ptr<BlockNode>& current_block) {
        for (const auto& stmt : current_block->stmts) {
            if (const auto decl = std::dynamic_pointer_cast<DeclStmtNode>(stmt)) {
                if (declared_vars.find(decl->id) == declared_vars.end()) {  // 变量没有声明过
                    space += 4;                                             // 变量占用4字节
                    declared_vars.insert(decl->id);                         // 记录声明
                }
            }
            else if (const auto if_stmt = std::dynamic_pointer_cast<IfStmtNode>(stmt)) {
                if (auto then_block = std::dynamic_pointer_cast<BlockNode>(if_stmt->thenStmt)) {
                    calculateSpace(then_block);
                }
                if (if_stmt->elseStmt) {
                    if (auto else_block = std::dynamic_pointer_cast<BlockNode>(if_stmt->elseStmt)) {
                        calculateSpace(else_block);
                    }
                }
            }
            else if (const auto while_stmt = std::dynamic_pointer_cast<WhileStmtNode>(stmt)) {
                if (auto body_block = std::dynamic_pointer_cast<BlockNode>(while_stmt->body)) {
                    calculateSpace(body_block);
                }
            }
            else if (auto block_stmt = std::dynamic_pointer_cast<BlockNode>(stmt)) {
                calculateSpace(block_stmt);
            }
        }
        };

    calculateSpace(block);
    return space;
}

void CodeGenerator::alignStack(int& offset) {
    // RISC-V栈需要16字节对齐
    if (offset % 16 != 0) {
        offset = ((offset / 16) + 1) * 16;
    }
}

void CodeGenerator::emitPrologue(const int frame_size) const {
    out << "    # Function prologue\n";

    if (frame_size >= -2048 && frame_size < 2048) {
        out << "    addi sp, sp, -" << frame_size << "\n";
    } else {
        out << "    li t0, " << frame_size << "\n";
        out << "    sub sp, sp, t0\n";
    }

    const int ra_off = frame_size - 4;
    const int fp_off = frame_size - 8;

    if (ra_off >= -2048 && ra_off < 2048) {
        out << "    sw ra, " << ra_off << "(sp)\n";
    } else {
        out << "    li t0, " << ra_off << "\n";
        out << "    add t0, sp, t0\n";
        out << "    sw ra, 0(t0)\n";
    }

    if (fp_off >= -2048 && fp_off < 2048) {
        out << "    sw fp, " << fp_off << "(sp)\n";
    } else {
        out << "    li t0, " << fp_off << "\n";
        out << "    add t0, sp, t0\n";
        out << "    sw fp, 0(t0)\n";
    }

    if (frame_size >= -2048 && frame_size < 2048) {
        out << "    addi fp, sp, " << frame_size << "\n";
    } else {
        out << "    li t0, " << frame_size << "\n";
        out << "    add fp, sp, t0\n";
    }
}

void CodeGenerator::emitEpilogue(const int frame_size) const {
    out << "    # Function epilogue\n";

    const int ra_off = frame_size - 4;
    const int fp_off = frame_size - 8;

    if (ra_off >= -2048 && ra_off < 2048) {
        out << "    lw ra, " << ra_off << "(sp)\n";
    } else {
        out << "    li t0, " << ra_off << "\n";
        out << "    add t0, sp, t0\n";
        out << "    lw ra, 0(t0)\n";
    }

    if (fp_off >= -2048 && fp_off < 2048) {
        out << "    lw fp, " << fp_off << "(sp)\n";
    } else {
        out << "    li t0, " << fp_off << "\n";
        out << "    add t0, sp, t0\n";
        out << "    lw fp, 0(t0)\n";
    }

    if (frame_size >= -2048 && frame_size < 2048) {
        out << "    addi sp, sp, " << frame_size << "\n";
    } else {
        out << "    li t0, " << frame_size << "\n";
        out << "    add sp, sp, t0\n";
    }

    out << "    jr ra\n";
}
std::string CodeGenerator::allocateRegister() {
    // 首先尝试正常分配
    std::string reg = reg_alloc.tryAllocate();
    if (!reg.empty()) {
        return reg;
    }

    // 如果没有空闲寄存器，强制分配
    auto [forced_reg, spill_offset] = reg_alloc.forceAllocate(out, getTempStackOffset());
    if (spill_offset >= 0) {
        // 记录溢出信息
        spill_stack.emplace( forced_reg, spill_offset );
    }
    return forced_reg;
}

void CodeGenerator::freeRegister(const std::string& reg) {
    reg_alloc.free(reg);

    // 检查是否需要恢复溢出的值
    if (!spill_stack.empty() && spill_stack.top().first == reg) {
        auto [spilled_reg, offset] = spill_stack.top();
        spill_stack.pop();

        // 检查偏移量是否在立即数范围内
        if (offset >= -2048 && offset < 2048) {
            out << "    lw " << spilled_reg << ", " << offset << "(sp)  # restore\n";
        }
        else {
            // 需要使用临时寄存器来计算地址
            out << "    addi sp, sp, -8\n";
            out << "    sw t5, 0(sp)  # save t5\n";
            out << "    li t5, " << offset << "\n";
            out << "    add t5, sp, t5\n";
            out << "    addi t5, t5, 8\n";  // 补偿临时调整的sp
            out << "    lw " << spilled_reg << ", 0(t5)  # restore\n";
            out << "    lw t5, 0(sp)  # restore t5\n";
            out << "    addi sp, sp, 8\n";
        }
        // 不要再次标记为使用，因为我们刚刚释放了它
    }
}

void CodeGenerator::visit(BlockNode& node) {
    sym_table.enterScope();

    // 保存当前的临时偏移
    const int saved_temp_offset = temp_offset;

    for (const auto& stmt : node.stmts) {
        stmt->accept(*this);
    }

    // 恢复临时偏移
    temp_offset = saved_temp_offset;

    sym_table.exitScope();
}

void CodeGenerator::visit(StmtNode& node) {
    // 基类不应该被直接访问
    throw std::runtime_error("Should not visit base StmtNode");
}

void CodeGenerator::visit(EmptyStmtNode& node) {
    // 空语句，不生成代码
}

void CodeGenerator::visit(ExprStmtNode& node) {
    const std::string result_reg = generateExpr(node.expr);
    freeRegister(result_reg);
}

void CodeGenerator::visit(AssignStmtNode& node) {
    const std::string result_reg = generateExpr(node.expr);
    const CodeGenSymbol* sym = sym_table.lookupSymbol(node.id);
    if (!sym) {
        throw std::runtime_error("Undefined variable: " + node.id);
    }

    // 检查偏移量是否在立即数范围内
    if (sym->offset >= -2048 && sym->offset < 2048) {
        out << "    sw " << result_reg << ", " << sym->offset << "(sp)\n";
    }
    else {
        // 偏移量太大，需要先计算地址
        const std::string addr_reg = allocateRegister();
        out << "    li " << addr_reg << ", " << sym->offset << "\n";
        out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
        out << "    sw " << result_reg << ", 0(" << addr_reg << ")\n";
        freeRegister(addr_reg);
    }

    freeRegister(result_reg);
}

void CodeGenerator::visit(DeclStmtNode& node) {
    // 分配局部变量
    sym_table.addSymbol(node.id, stack_offset);
    stack_offset += 4;
    // 处理初始化表达式
    if (node.initExpr) {
        const std::string result_reg = generateExpr(node.initExpr);
        const CodeGenSymbol* sym = sym_table.lookupSymbol(node.id);

        // 检查偏移量是否在立即数范围内
        if (sym->offset >= -2048 && sym->offset < 2048) {
            out << "    sw " << result_reg << ", " << sym->offset << "(sp)\n";
        }
        else {
            // 偏移量太大，需要先计算地址
            const std::string addr_reg = allocateRegister();
            out << "    li " << addr_reg << ", " << sym->offset << "\n";
            out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
            out << "    sw " << result_reg << ", 0(" << addr_reg << ")\n";
            freeRegister(addr_reg);
        }

        freeRegister(result_reg);
    }
    // 没有初始化表达式，变量默认为0
    else {
        const CodeGenSymbol* sym = sym_table.lookupSymbol(node.id);

        // 检查偏移量是否在立即数范围内
        if (sym->offset >= -2048 && sym->offset < 2048) {
            out << "    sw x0, " << sym->offset << "(sp)\n";
        }
        else {
            // 偏移量太大，需要先计算地址
            const std::string addr_reg = allocateRegister();
            out << "    li " << addr_reg << ", " << sym->offset << "\n";
            out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
            out << "    sw x0, 0(" << addr_reg << ")\n";
            freeRegister(addr_reg);
        }
    }
}

void CodeGenerator::visit(IfStmtNode& node) {
    const std::string else_label = generateLabel("else_");
    const std::string end_label = generateLabel("end_if_");
    // 计算条件
    generateCond(node.cond, else_label);

    // 保存continue状态
    const bool prev_continue = in_continue_statement;
    in_continue_statement = false;

    node.thenStmt->accept(*this);

    //如果不在continue语句中，添加跳转到end_label
    if (!in_continue_statement) {
        out << "    j " << end_label << "\n";
    }

    // 处理else语句
    out << else_label << ":\n";
    if (node.elseStmt) {
        in_continue_statement = false;
        node.elseStmt->accept(*this);
    }
    out << end_label << ":\n";

    // 恢复continue状态
    in_continue_statement = prev_continue;
}

void CodeGenerator::visit(WhileStmtNode& node) {
    const std::string cond_label = generateLabel("while_cond_");
    const std::string end_label = generateLabel("end_while_");

    break_labels.push_back(end_label);
    continue_labels.push_back(cond_label);

    out << cond_label << ":\n";
    generateCond(node.cond, end_label);

    // 保存continue状态
    const bool prev_continue = in_continue_statement;
    in_continue_statement = false;

    node.body->accept(*this);

    out << "    j " << cond_label << "\n";
    out << end_label << ":\n";

    break_labels.pop_back();
    continue_labels.pop_back();

    // 恢复continue状态
    in_continue_statement = prev_continue;
}

void CodeGenerator::visit(BreakStmtNode& node) {
    if (break_labels.empty()) {
        throw std::runtime_error("Break statement outside loop");
    }
    out << "    j " << break_labels.back() << "\n";
}

void CodeGenerator::visit(ContinueStmtNode& node) {
    if (continue_labels.empty()) {
        throw std::runtime_error("Continue statement outside loop");
    }
    in_continue_statement = true;
    out << "    j " << continue_labels.back() << "\n";
}

void CodeGenerator::visit(ReturnStmtNode& node) {
    if (node.retExpr) {
        const std::string result_reg = generateExpr(node.retExpr);
        if (result_reg != "a0") {
            out << "    mv a0, " << result_reg << "\n";
        }
        freeRegister(result_reg);
    }
    // 跳转到当前函数的尾声标签
    out << "    j " << current_function << "_epilogue\n";
}
std::string CodeGenerator::generateExpr(const std::shared_ptr<ExprNode>& expr, const std::string&) {
    expr->accept(*this);
    return current_expr_result;
}

void CodeGenerator::visit(ExprNode& node) {
    throw std::runtime_error("Should not visit base ExprNode");
}

void CodeGenerator::visit(LOrExprNode& node) {
    const std::string result_reg = allocateRegister();
    const std::string true_label = generateLabel("or_true_");
    const std::string end_label = generateLabel("or_end_");

    // 计算左操作数
    const std::string left_reg = generateExpr(node.left);

    // 如果左操作数为真，短路到true
    out << "    bnez " << left_reg << ", " << true_label << "\n";
    freeRegister(left_reg);

    // 左操作数为假，计算右操作数
    const std::string right_reg = generateExpr(node.right);
    // 将右操作数的值标准化为0或1
    out << "    snez " << result_reg << ", " << right_reg << "\n";
    freeRegister(right_reg);
    out << "    j " << end_label << "\n";

    // 左操作数为真的情况
    out << true_label << ":\n";
    out << "    li " << result_reg << ", 1\n";

    out << end_label << ":\n";
    current_expr_result = result_reg;
}

void CodeGenerator::visit(LAndExprNode& node) {
    const std::string result_reg = allocateRegister();
    const std::string false_label = generateLabel("and_false_");
    const std::string end_label = generateLabel("and_end_");

    // 计算左操作数
    const std::string left_reg = generateExpr(node.left);

    // 如果左操作数为假，短路到false
    out << "    beqz " << left_reg << ", " << false_label << "\n";
    freeRegister(left_reg);

    // 左操作数为真，计算右操作数
    const std::string right_reg = generateExpr(node.right);
    // 结果等于右操作数的值，将右操作数的值标准化为0或1
    out << "    snez " << result_reg << ", " << right_reg << "\n";
    freeRegister(right_reg);
    out << "    j " << end_label << "\n";

    // 左操作数为假的情况
    out << false_label << ":\n";
    out << "    li " << result_reg << ", 0\n";

    out << end_label << ":\n";
    current_expr_result = result_reg;
}

void CodeGenerator::visit(RelExprNode& node) {
    const std::string left_reg = generateExpr(node.left);
    const std::string right_reg = generateExpr(node.right);
    const std::string result_reg = allocateRegister();

    if (node.op == "<") {
        out << "    slt " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
    }
    else if (node.op == ">") {
        out << "    slt " << result_reg << ", " << right_reg << ", " << left_reg << "\n";
    }
    else if (node.op == "<=") {
        out << "    slt " << result_reg << ", " << right_reg << ", " << left_reg << "\n";
        out << "    xori " << result_reg << ", " << result_reg << ", 1\n";
    }
    else if (node.op == ">=") {
        out << "    slt " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
        out << "    xori " << result_reg << ", " << result_reg << ", 1\n";
    }
    else if (node.op == "==") {
        out << "    sub " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
        out << "    seqz " << result_reg << ", " << result_reg << "\n";
    }
    else if (node.op == "!=") {
        out << "    sub " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
        out << "    snez " << result_reg << ", " << result_reg << "\n";
    }

    freeRegister(left_reg);
    freeRegister(right_reg);
    current_expr_result = result_reg;
}

void CodeGenerator::visit(AddExprNode& node) {
    const std::string left_reg = generateExpr(node.left);
    const std::string right_reg = generateExpr(node.right);
    const std::string result_reg = allocateRegister();

    if (node.op == "+") {
        out << "    add " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
    }
    else if (node.op == "-") {
        out << "    sub " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
    }

    freeRegister(left_reg);
    freeRegister(right_reg);
    current_expr_result = result_reg;
}

void CodeGenerator::visit(MulExprNode& node) {
    const std::string left_reg = generateExpr(node.left);
    const std::string right_reg = generateExpr(node.right);
    const std::string result_reg = allocateRegister();

    if (node.op == "*") {
        out << "    mul " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
    }
    else if (node.op == "/") {
        out << "    div " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
    }
    else if (node.op == "%") {
        out << "    rem " << result_reg << ", " << left_reg << ", " << right_reg << "\n";
    }

    freeRegister(left_reg);
    freeRegister(right_reg);
    current_expr_result = result_reg;
}

void CodeGenerator::visit(UnaryExprNode& node) {
    const std::string operand_reg = generateExpr(node.expr);

    if (node.op == "+") {
        current_expr_result = operand_reg;
        return;
    }

    const std::string result_reg = allocateRegister();

    if (node.op == "-") {
        out << "    sub " << result_reg << ", x0, " << operand_reg << "\n";
    }
    else if (node.op == "!") {
        out << "    seqz " << result_reg << ", " << operand_reg << "\n";
    }

    freeRegister(operand_reg);
    current_expr_result = result_reg;
}

void CodeGenerator::visit(PrimaryExprNode& node) {
    // 检查是否是数字（包括负数）
    if (std::all_of(node.value.begin(), node.value.end(), isdigit) ||
        (node.value[0] == '-' && std::all_of(node.value.begin() + 1, node.value.end(), isdigit))) {
        const std::string result_reg = allocateRegister();
        out << "    li " << result_reg << ", " << node.value << "\n";
        current_expr_result = result_reg;
    }
    else if (!node.value.empty() && (std::isalpha(node.value[0]) || node.value[0] == '_') &&
        std::all_of(node.value.begin(), node.value.end(), [](const char c) { return std::isalnum(c) || c == '_'; })) {
        // 变量
        const CodeGenSymbol* sym = sym_table.lookupSymbol(node.value);
        if (!sym) {
            throw std::runtime_error("Undefined variable: " + node.value);
        }
        const std::string result_reg = allocateRegister();

        // 检查偏移量是否在立即数范围内
        if (sym->offset >= -2048 && sym->offset < 2048) {
            out << "    lw " << result_reg << ", " << sym->offset << "(sp)\n";
        }
        else {
            // 偏移量太大，需要先计算地址
            const std::string addr_reg = allocateRegister();
            out << "    li " << addr_reg << ", " << sym->offset << "\n";
            out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
            out << "    lw " << result_reg << ", 0(" << addr_reg << ")\n";
            freeRegister(addr_reg);
        }
        current_expr_result = result_reg;
    }
    else {
        throw std::runtime_error("Invalid primary expression: " + node.value);
    }
}

void CodeGenerator::visit(ParamNode& node) {
    // 参数节点在FuncDefNode中已处理
}

void CodeGenerator::visit(FuncCallExprNode& node) {
    const int n = static_cast<int>(node.args.size());

    // 保存当前使用的临时寄存器
    std::vector<std::string> saved_regs;
    std::vector<int> save_offsets;

    // 修复：确保保存寄存器时使用正确的偏移
    int save_base = temp_area_start;  // 使用临时区起始位置
    int save_offset_counter = 0;

    for (int i = 0; i < RegisterAllocator::NUM_TEMP_REGS; ++i) {
        std::string temp_reg = "t" + std::to_string(i);
        if (reg_alloc.isUsed(temp_reg)) {
            // 使用固定的保存位置，避免getTempStackOffset()的问题
            int offset = save_base + save_offset_counter * 4;
            save_offset_counter++;

            if (offset >= -2048 && offset < 2048) {
                out << "    sw " << temp_reg << ", " << offset << "(sp)\n";
            } else {
                std::string addr_reg = "t6";  // 使用固定的临时寄存器
                out << "    li " << addr_reg << ", " << offset << "\n";
                out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
                out << "    sw " << temp_reg << ", 0(" << addr_reg << ")\n";
            }
            saved_regs.push_back(temp_reg);
            save_offsets.push_back(offset);
        }
    }
    // 传递参数（使用寄存器a0-a7和预留出参区）
    for (int i = 0; i < n; ++i) {
        std::string arg_reg = generateExpr(node.args[i]);
        if (i < 8) {
            out << "    mv a" << i << ", " << arg_reg << "\n";
        } else {
            // 关键修复：使用current_out_arg_base而不是temp_area_end
            const int off = current_out_arg_base + (i - 8) * 4;
            if (off >= -2048 && off < 2048) {
                out << "    sw " << arg_reg << ", " << off << "(sp)\n";
            } else {
                std::string addr_reg = allocateRegister();
                out << "    li " << addr_reg << ", " << off << "\n";
                out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
                out << "    sw " << arg_reg << ", 0(" << addr_reg << ")\n";
                freeRegister(addr_reg);
            }
        }
        freeRegister(arg_reg);
    }

    // 调用函数
    out << "    jal ra, " << node.funcName << "\n";

    // 恢复保存的寄存器
    for (int i = static_cast<int>(saved_regs.size()) - 1; i >= 0; --i) {
        const int offset = save_offsets[i];
        if (offset >= -2048 && offset < 2048) {
            out << "    lw " << saved_regs[i] << ", " << offset << "(sp)\n";
        } else {
            std::string addr_reg = "t6";  // 使用固定的临时寄存器
            out << "    li " << addr_reg << ", " << offset << "\n";
            out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
            out << "    lw " << saved_regs[i] << ", 0(" << addr_reg << ")\n";
        }
    }

    // 函数返回值在a0，分配寄存器保存
    current_expr_result = allocateRegister();
    out << "    mv " << current_expr_result << ", a0\n";
}
void CodeGenerator::visit(CallStmtNode& node) {
    node.call->accept(*this);
    // 释放调用表达式使用的寄存器（无返回值需要保留）
    freeRegister(current_expr_result);
}
void CodeGenerator::generateCond(const std::shared_ptr<ExprNode>& expr, const std::string& falseLabel) {
    if (const auto andNode = std::dynamic_pointer_cast<LAndExprNode>(expr)) {
        generateCond(andNode->left, falseLabel);
        generateCond(andNode->right, falseLabel);
        return;
    }
    if (const auto orNode = std::dynamic_pointer_cast<LOrExprNode>(expr)) {
        const std::string skip_label = generateLabel("or_true_");
        generateCondOr(orNode, falseLabel, skip_label);
        return;
    }

    const std::string reg = generateExpr(expr);
    out << "    beqz " << reg << ", " << falseLabel << "\n";
    freeRegister(reg);
}

void CodeGenerator::generateCondOr(const std::shared_ptr<LOrExprNode>& node, const std::string& falseLabel, const std::string& trueLabel) {
    if (const auto leftOr = std::dynamic_pointer_cast<LOrExprNode>(node->left)) {
        generateCondOr(leftOr, falseLabel, trueLabel);
    }
    else {
        const std::string reg = generateExpr(node->left);
        out << "    bnez " << reg << ", " << trueLabel << "\n";
        freeRegister(reg);
    }
    if (const auto rightOr = std::dynamic_pointer_cast<LOrExprNode>(node->right)) {
        generateCondOr(rightOr, falseLabel, trueLabel);
    }
    else {
        const std::string reg = generateExpr(node->right);
        out << "    bnez " << reg << ", " << trueLabel << "\n";
        freeRegister(reg);
    }
    out << "    j " << falseLabel << "\n";
    out << trueLabel << ":\n";
}

// 计算临时空间需求
int CodeGenerator::calculateTempSpaceNeed(const std::shared_ptr<BlockNode>& block) {
    int temp_space = 256; // 基础临时空间
    int max_function_call_args = 0; // 最大函数调用参数数量
    int max_expression_depth = 0; // 最大表达式深度

    // 添加：记录准备函数参数时需要的最大临时存储
    int max_arg_prep_space = 0;

    std::function<void(const std::shared_ptr<BlockNode>&)> analyzeBlock;
    std::function<int(const std::shared_ptr<ExprNode>&)> analyzeExpr;

    analyzeExpr = [&](const std::shared_ptr<ExprNode>& expr) -> int {
        if (!expr) return 0;
        if (const auto funcCall = std::dynamic_pointer_cast<FuncCallExprNode>(expr)) {
            max_function_call_args = std::max(max_function_call_args, static_cast<int>(funcCall->args.size()));

            // 关键添加：计算准备参数需要的临时空间
            // 每个复杂参数表达式可能需要临时存储
            int arg_prep_count = 0;
            for (const auto& arg : funcCall->args) {
                // 如果参数是复杂表达式（不是简单变量或常量），需要临时存储
                if (std::dynamic_pointer_cast<AddExprNode>(arg) ||
                    std::dynamic_pointer_cast<MulExprNode>(arg) ||
                    std::dynamic_pointer_cast<FuncCallExprNode>(arg)) {
                    arg_prep_count++;
                }
            }
            max_arg_prep_space = std::max(max_arg_prep_space, arg_prep_count * 4);

            int depth = 1;
            for (const auto& arg : funcCall->args) {
                depth = std::max(depth, 1 + analyzeExpr(arg));
            }
            return depth;
        }
        else if (const auto binaryExpr = std::dynamic_pointer_cast<AddExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left), analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr = std::dynamic_pointer_cast<MulExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left), analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr = std::dynamic_pointer_cast<RelExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left), analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr = std::dynamic_pointer_cast<LAndExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left), analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr = std::dynamic_pointer_cast<LOrExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left), analyzeExpr(binaryExpr->right));
        }
        else if (const auto unaryExpr = std::dynamic_pointer_cast<UnaryExprNode>(expr)) {
            return 1 + analyzeExpr(unaryExpr->expr);
        }
        return 1; // 基本表达式
    };

    analyzeBlock = [&](const std::shared_ptr<BlockNode>& current_block) {
        for (const auto& stmt : current_block->stmts) {
            if (const auto exprStmt = std::dynamic_pointer_cast<ExprStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(exprStmt->expr));
            }
            else if (const auto assignStmt = std::dynamic_pointer_cast<AssignStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(assignStmt->expr));
            }
            else if (const auto declStmt = std::dynamic_pointer_cast<DeclStmtNode>(stmt)) {
                if (declStmt->initExpr) {
                    max_expression_depth = std::max(max_expression_depth, analyzeExpr(declStmt->initExpr));
                }
            }
            else if (const auto ifStmt = std::dynamic_pointer_cast<IfStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(ifStmt->cond));
                if (auto thenBlock = std::dynamic_pointer_cast<BlockNode>(ifStmt->thenStmt)) {
                    analyzeBlock(thenBlock);
                }
                if (ifStmt->elseStmt) {
                    if (auto elseBlock = std::dynamic_pointer_cast<BlockNode>(ifStmt->elseStmt)) {
                        analyzeBlock(elseBlock);
                    }
                }
            }
            else if (const auto whileStmt = std::dynamic_pointer_cast<WhileStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(whileStmt->cond));
                if (auto bodyBlock = std::dynamic_pointer_cast<BlockNode>(whileStmt->body)) {
                    analyzeBlock(bodyBlock);
                }
            }
            else if (const auto returnStmt = std::dynamic_pointer_cast<ReturnStmtNode>(stmt)) {
                if (returnStmt->retExpr) {
                    max_expression_depth = std::max(max_expression_depth, analyzeExpr(returnStmt->retExpr));
                }
            }
            else if (const auto callStmt = std::dynamic_pointer_cast<CallStmtNode>(stmt)) {
                if (const auto funcCall = std::dynamic_pointer_cast<FuncCallExprNode>(callStmt->call)) {
                    max_function_call_args = std::max(max_function_call_args, static_cast<int>(funcCall->args.size()));

                    // 关键添加：CallStmt也需要计算参数准备空间
                    int arg_prep_count = 0;
                    for (const auto& arg : funcCall->args) {
                        if (std::dynamic_pointer_cast<AddExprNode>(arg) ||
                            std::dynamic_pointer_cast<MulExprNode>(arg) ||
                            std::dynamic_pointer_cast<FuncCallExprNode>(arg)) {
                            arg_prep_count++;
                        }
                        max_expression_depth = std::max(max_expression_depth, analyzeExpr(arg));
                    }
                    max_arg_prep_space = std::max(max_arg_prep_space, arg_prep_count * 4);
                }
            }
            else if (auto blockStmt = std::dynamic_pointer_cast<BlockNode>(stmt)) {
                analyzeBlock(blockStmt);
            }
        }
    };

    analyzeBlock(block);

    // 根据分析结果计算临时空间需求
    int function_call_space = max_function_call_args * 4 + 7 * 4; // 7个临时寄存器溢出
    int expression_space = max_expression_depth * 8; // 每层8字节

    // 确保有足够的参数准备空间和寄存器保存空间
    temp_space = std::max(temp_space, function_call_space + expression_space + max_arg_prep_space);

    // 增加更多的缓冲空间，避免溢出
    temp_space += 512;  // 增加到512字节的额外缓冲区

    // 对于有很多函数调用的函数，需要更多空间
    if (max_function_call_args > 16) {
        temp_space += (max_function_call_args - 16) * 8;  // 每个额外参数8字节
    }

    return temp_space;
}
