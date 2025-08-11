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

void CodeGenerator::visit(FuncDefNode& node) {
    current_function = node.id;
    stack_offset = 0;   //
    temp_offset = 0;    //

    // 重置寄存器分配器
    reg_alloc.reset();  //每个函数都需要重置分配器

    // 清空溢出栈
    while (!spill_stack.empty()) {
        spill_stack.pop();
    }

    // 函数标签
    out << node.id << ":\n";

    // 进入新的作用域
    sym_table.enterScope();

    // 被调用函数的栈帧（局部变量空间，临时变量空间和保存的返回地址）
    int temp_space = calculateTempSpaceNeed(node.block); // 智能计算临时空间
    const int local_space = calculateLocalSpace(node.block);    // 局部变量空间
    constexpr int frame_space = 8; // ra和fp各4字节

    // 如果有很多参数，增加临时空间
    if (node.params.size() > 8) {
        temp_space += (static_cast<int>(node.params.size()) - 8) * 8;
    }

    const int param_space = std::max(0, static_cast<int>(node.params.size()) * 4);


    // 如果是递归函数（简单检测），适度增加临时空间
    bool is_recursive = false;
    const std::function<void(const std::shared_ptr<StmtNode>&)> checkRecursive =
        [&](const std::shared_ptr<StmtNode>& stmt) {
        if (const auto expr_stmt = std::dynamic_pointer_cast<ExprStmtNode>(stmt)) {
            if (const auto call = std::dynamic_pointer_cast<FuncCallExprNode>(expr_stmt->expr)) {
                if (call->funcName == node.id) {
                    is_recursive = true;
                }
            }
        }
        else if (const auto call_stmt = std::dynamic_pointer_cast<CallStmtNode>(stmt)) {
            if (const auto call = std::dynamic_pointer_cast<FuncCallExprNode>(call_stmt->call)) {
                if (call->funcName == node.id) {
                    is_recursive = true;
                }
            }
        }
        else if (const auto return_stmt = std::dynamic_pointer_cast<ReturnStmtNode>(stmt)) {
            if (return_stmt->retExpr) {
                if (const auto call = std::dynamic_pointer_cast<FuncCallExprNode>(return_stmt->retExpr)) {
                    if (call->funcName == node.id) {
                        is_recursive = true;
                    }
                }
            }
        }
        };

    // 简单检查是否有递归调用
    for (const auto& stmt : node.block->stmts) {
        checkRecursive(stmt);
        if (is_recursive) break;
    }

    if (is_recursive) {
        temp_space = std::min(temp_space + 256, 1024); // 递归函数最多1024字节临时空间
    }

    //计算被调用者函数的总栈空间（局部变量空间，临时变量空间，栈帧指针空间和参数空间）
    int total_space = param_space + local_space + frame_space + temp_space;

    // 16字节对齐
    alignStack(total_space);

    // 存储总栈空间大小，用于边界检查
    current_function_stack_size = total_space;

    // 生成函数序言
    emitPrologue(total_space);

    // 处理参数
    int current_offset = 0;   // 当前栈帧偏移量
    for (size_t i = 0; i < node.params.size(); ++i) {
        if (i < 8) {
            // 前8个参数通过寄存器传递，保存到栈中
            if (current_offset >= -2048 && current_offset < 2048) {
                out << "    sw a" << i << ", " << current_offset << "(sp)\n";
            }
            else {
                // 偏移量太大，需要先计算地址
                std::string addr_reg = allocateRegister();
                out << "    li " << addr_reg << ", " << current_offset << "\n";
                out << "    add " << addr_reg << ", sp, " << addr_reg << "\n";
                out << "    sw a" << i << ", 0(" << addr_reg << ")\n";
                freeRegister(addr_reg);
            }
            sym_table.addSymbol(node.params[i]->id, current_offset, true, static_cast<int>(i));
            current_offset += 4;
        }
        else {
            // 超过8个的参数通过栈传递
            //const int param_offset = total_space + (static_cast<int>(i) - 8) * 4;
            const int caller_offset = current_offset + local_space + temp_space + frame_space + (static_cast<int>(i) - 8) * 4;
            // 对于通过栈传递的参数，需要特别处理大偏移量
            if (caller_offset >= 2048) {
                // 为栈传递的参数分配局部空间
                sym_table.addSymbol(node.params[i]->id, current_offset, true, static_cast<int>(i));

                // 在函数开始时，将栈参数复制到局部空间
                std::string temp_reg = allocateRegister();
                out << "    li " << temp_reg << ", " << caller_offset << "\n";
                out << "    add " << temp_reg << ", sp, " << temp_reg << "\n";
                out << "    lw t1, 0(" << temp_reg << ")\n";
                out << "    sw t1, " << current_offset << "(sp)\n";
                freeRegister(temp_reg);

                current_offset += 4;
            }
            else {
                sym_table.addSymbol(node.params[i]->id, caller_offset, true, static_cast<int>(i));
            }
        }
    }

    // 设置局部变量的起始偏移量
    stack_offset = current_offset;

    // 设置临时区域的起始偏移量
    temp_area_start = stack_offset +local_space; // 临时区域 = 参数区 + 局部变量区
    temp_offset = temp_area_start;
    // 设置临时区域的结束边界（预留frame_space给ra和fp）
    temp_area_end = total_space - frame_space;

    // 生成函数体
    node.block->accept(*this);

    // 如果是main函数且没有显式return，添加默认返回
    if (node.id == "main") {
        out << "    li a0, 0\n";
    }

    // 生成函数尾声标签
    out << current_function << "_epilogue:\n";
    emitEpilogue(total_space);

    // 退出作用域
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

void CodeGenerator::emitPrologue(const int local_size) const {
    out << "    # Function prologue\n";

    // 检查栈大小是否超出立即数范围
    if (local_size >= -2048 && local_size < 2048) {
        out << "    addi sp, sp, -" << local_size << "\n";
    }
    else {
        // 使用寄存器来处理大的栈偏移
        out << "    li t0, " << local_size << "\n";
        out << "    sub sp, sp, t0\n";
    }

    // 保存 ra 和 fp
    const int ra_offset = local_size - 4;
    const int fp_offset = local_size - 8;

    if (ra_offset >= -2048 && ra_offset < 2048) {
        out << "    sw ra, " << ra_offset << "(sp)\n";
    }
    else {
        out << "    li t0, " << ra_offset << "\n";
        out << "    add t0, sp, t0\n";
        out << "    sw ra, 0(t0)\n";
    }

    if (fp_offset >= -2048 && fp_offset < 2048) {
        out << "    sw fp, " << fp_offset << "(sp)\n";
    }
    else {
        out << "    li t0, " << fp_offset << "\n";
        out << "    add t0, sp, t0\n";
        out << "    sw fp, 0(t0)\n";
    }

    // 设置新的 fp
    if (local_size >= -2048 && local_size < 2048) {
        out << "    addi fp, sp, " << local_size << "\n";
    }
    else {
        out << "    li t0, " << local_size << "\n";
        out << "    add fp, sp, t0\n";
    }
}

void CodeGenerator::emitEpilogue(const int local_size) const {
    out << "    # Function epilogue\n";

    // 恢复 ra 和 fp
    const int ra_offset = local_size - 4;
    const int fp_offset = local_size - 8;

    if (ra_offset >= -2048 && ra_offset < 2048) {
        out << "    lw ra, " << ra_offset << "(sp)\n";
    }
    else {
        out << "    li t0, " << ra_offset << "\n";
        out << "    add t0, sp, t0\n";
        out << "    lw ra, 0(t0)\n";
    }

    if (fp_offset >= -2048 && fp_offset < 2048) {
        out << "    lw fp, " << fp_offset << "(sp)\n";
    }
    else {
        out << "    li t0, " << fp_offset << "\n";
        out << "    add t0, sp, t0\n";
        out << "    lw fp, 0(t0)\n";
    }
    // 恢复栈指针，释放所有分配的栈空间
    if (local_size >= -2048 && local_size < 2048) {
        out << "    addi sp, sp, " << local_size << "\n";
    }
    else {
        out << "    li t0, " << local_size << "\n";
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
    // 保存调用者保存的寄存器 (caller-saved registers)
    std::vector<std::string> saved_regs;
    std::vector<int> save_offsets;

    // 保存所有当前使用的临时寄存器
    for (int i = 0; i < RegisterAllocator::NUM_TEMP_REGS; ++i) {
        std::string temp_reg = "t" + std::to_string(i);
        if (reg_alloc.isUsed(temp_reg)) {
            int offset = getTempStackOffset();

            if (offset >= -2048 && offset < 2048) {
                out << "    sw " << temp_reg << ", " << offset << "(sp)\n";
            }
            else {
                // 使用临时寄存器来处理大偏移
                out << "    addi sp, sp, -8\n";
                out << "    sw t5, 0(sp)\n";
                out << "    li t5, " << offset << "\n";
                out << "    add t5, sp, t5\n";
                out << "    addi t5, t5, 8\n";
                out << "    sw " << temp_reg << ", 0(t5)\n";
                out << "    lw t5, 0(sp)\n";
                out << "    addi sp, sp, 8\n";
            }

            saved_regs.push_back(temp_reg);
            save_offsets.push_back(offset);
        }
    }

// 计算参数表达式
    std::vector<std::string> arg_values;
    std::vector<int> arg_stack_offsets;

    // 先计算所有参数并保存到栈上，避免寄存器冲突
    for (const auto & arg : node.args) {
        std::string arg_reg = generateExpr(arg);
        int offset = getTempStackOffset();

        if (offset >= -2048 && offset < 2048) {
            out << "    sw " << arg_reg << ", " << offset << "(sp)\n";
        }
        else {
            out << "    addi sp, sp, -8\n";
            out << "    sw t5, 0(sp)\n";
            out << "    li t5, " << offset << "\n";
            out << "    add t5, sp, t5\n";
            out << "    addi t5, t5, 8\n";
            out << "    sw " << arg_reg << ", 0(t5)\n";
            out << "    lw t5, 0(sp)\n";
            out << "    addi sp, sp, 8\n";
        }

        arg_stack_offsets.push_back(offset);
        freeRegister(arg_reg);
    }

    // 将参数从栈加载到正确的寄存器 (a0-a7)
    for (size_t i = 0; i < std::min(node.args.size(), static_cast<size_t>(8)); ++i) {
        std::string target_reg = "a" + std::to_string(i);
        const int offset = arg_stack_offsets[i];

        if (offset >= -2048 && offset < 2048) {
            out << "    lw " << target_reg << ", " << offset << "(sp)\n";
        }
        else {
            out << "    addi sp, sp, -8\n";
            out << "    sw t5, 0(sp)\n";
            out << "    li t5, " << offset << "\n";
            out << "    add t5, sp, t5\n";
            out << "    addi t5, t5, 8\n";
            out << "    lw " << target_reg << ", 0(t5)\n";
            out << "    lw t5, 0(sp)\n";
            out << "    addi sp, sp, 8\n";
        }
    }

    // 处理超过8个参数的情况（通过栈传递）
    for (size_t i = 8; i < node.args.size(); ++i) {
        out << "    addi sp, sp, -4\n";

        const int offset = arg_stack_offsets[i];
        // 这里是调用参数压栈时，处理 offset 对应的值
        std::string temp = "t0";  // 临时寄存器
        // 注意：此时 sp 未做额外调整，所以直接用 offset
        if (offset >= -2048 && offset < 2048) {
            out << "    lw " << temp << ", " << offset << "(sp)\n";
        } else {
            // 临时保存 t5
            out << "    addi sp, sp, -8\n";
            out << "    sw t5, 0(sp)\n";
            out << "    li t5, " << offset << "\n";
            out << "    add t5, sp, t5\n";
            out << "    lw " << temp << ", 0(t5)\n";
            out << "    lw t5, 0(sp)\n";
            out << "    addi sp, sp, 8\n";
        }

        // 把取出来的参数值存到调用栈上的临时位置
        out << "    sw " << temp << ", 0(sp)\n";
    }

    // 调用函数
    out << "    jal ra, " << node.funcName << "\n";

    // 恢复栈指针（如果有超过8个参数）
    if (node.args.size() > 8) {
        const int extra_args = static_cast<int>(node.args.size()) - 8;
        out << "    addi sp, sp, " << (extra_args * 4) << "\n";
    }

    // 恢复保存的寄存器（逆序）
    for (int i = static_cast<int>(saved_regs.size()) - 1; i >= 0; --i) {
        const int offset = save_offsets[i];

        if (offset >= -2048 && offset < 2048) {
            out << "    lw " << saved_regs[i] << ", " << offset << "(sp)\n";
        }
        else {
            out << "    addi sp, sp, -8\n";
            out << "    sw t5, 0(sp)\n";
            out << "    li t5, " << offset << "\n";
            out << "    add t5, sp, t5\n";
            out << "    addi t5, t5, 8\n";
            out << "    lw " << saved_regs[i] << ", 0(t5)\n";
            out << "    lw t5, 0(sp)\n";
            out << "    addi sp, sp, 8\n";
        }
    }

    // 将返回值保存到栈上，避免被后续操作覆盖
    const int return_offset = getTempStackOffset();
    if (return_offset >= -2048 && return_offset < 2048) {
        out << "    sw a0, " << return_offset << "(sp)\n";
    }
    else {
        out << "    addi sp, sp, -8\n";
        out << "    sw t5, 0(sp)\n";
        out << "    li t5, " << return_offset << "\n";
        out << "    add t5, sp, t5\n";
        out << "    addi t5, t5, 8\n";
        out << "    sw a0, 0(t5)\n";
        out << "    lw t5, 0(sp)\n";
        out << "    addi sp, sp, 8\n";
    }

    // 分配新寄存器来保存返回值
    const std::string result_reg = allocateRegister();
    if (return_offset >= -2048 && return_offset < 2048) {
        out << "    lw " << result_reg << ", " << return_offset << "(sp)\n";
    }
    else {
        out << "    addi sp, sp, -8\n";
        out << "    sw t5, 0(sp)\n";
        out << "    li t5, " << return_offset << "\n";
        out << "    add t5, sp, t5\n";
        out << "    addi t5, t5, 8\n";
        out << "    lw " << result_reg << ", 0(t5)\n";
        out << "    lw t5, 0(sp)\n";
        out << "    addi sp, sp, 8\n";
    }

    current_expr_result = result_reg;
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

void CodeGenerator::visit(CallStmtNode& node) {
    node.call->accept(*this);
    // void函数调用语句，无需处理返回值
    if (current_expr_result != "a0") {
        freeRegister(current_expr_result);
    }
}
// 计算临时空间需求
int CodeGenerator::calculateTempSpaceNeed(const
std::shared_ptr<BlockNode>& block) {
    int temp_space = 256; // 基础临时空间
    int max_function_call_args = 0; // 最大函数调用参数数量
    int max_expression_depth = 0; // 最大表达式深度
    std::function<void(const std::shared_ptr<BlockNode>&)>
    analyzeBlock;
    std::function<int(const std::shared_ptr<ExprNode>&)>
    analyzeExpr;
    analyzeExpr = [&](const std::shared_ptr<ExprNode>& expr) ->
    int {
        if (!expr) return 0;
        if (const auto funcCall =
        std::dynamic_pointer_cast<FuncCallExprNode>(expr)) {
            max_function_call_args =
            std::max(max_function_call_args, static_cast<int>(funcCall->args.size()));
            int depth = 1;
            for (const auto& arg : funcCall->args) {
                depth = std::max(depth, 1 + analyzeExpr(arg));
            }
            return depth;
        }
        else if (const auto binaryExpr =
        std::dynamic_pointer_cast<AddExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left),
            analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr =
        std::dynamic_pointer_cast<MulExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left),analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr =
        std::dynamic_pointer_cast<RelExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left),
            analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr =
        std::dynamic_pointer_cast<LAndExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left),
            analyzeExpr(binaryExpr->right));
        }
        else if (const auto binaryExpr =
        std::dynamic_pointer_cast<LOrExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(binaryExpr->left),
            analyzeExpr(binaryExpr->right));
        }
        else if (const auto unaryExpr =
        std::dynamic_pointer_cast<UnaryExprNode>(expr)) {
            return 1 + analyzeExpr(unaryExpr->expr);
        }
        return 1; // 基本表达式
    };
    analyzeBlock = [&](const std::shared_ptr<BlockNode>&
    current_block) {
        for (const auto& stmt : current_block->stmts) {
            if (const auto exprStmt =
            std::dynamic_pointer_cast<ExprStmtNode>(stmt)) {
                max_expression_depth =
                std::max(max_expression_depth, analyzeExpr(exprStmt->expr));
            }
            else if (const auto assignStmt =
            std::dynamic_pointer_cast<AssignStmtNode>(stmt)) {
                max_expression_depth =std::max(max_expression_depth, analyzeExpr(assignStmt->expr));
            }
            else if (const auto declStmt =
            std::dynamic_pointer_cast<DeclStmtNode>(stmt)) {
                if (declStmt->initExpr) {
                    max_expression_depth =
                    std::max(max_expression_depth, analyzeExpr(declStmt->initExpr));
                }
            }
            else if (const auto ifStmt =
            std::dynamic_pointer_cast<IfStmtNode>(stmt)) {
                max_expression_depth =
                std::max(max_expression_depth, analyzeExpr(ifStmt->cond));
                if (auto thenBlock =
                std::dynamic_pointer_cast<BlockNode>(ifStmt->thenStmt)) {
                    analyzeBlock(thenBlock);
                }
                if (ifStmt->elseStmt) {
                    if (auto elseBlock =
                    std::dynamic_pointer_cast<BlockNode>(ifStmt->elseStmt)) {
                        analyzeBlock(elseBlock);
                    }
                }
            }
            else if (const auto whileStmt =
            std::dynamic_pointer_cast<WhileStmtNode>(stmt)) {
                max_expression_depth =
                std::max(max_expression_depth, analyzeExpr(whileStmt->cond));
                if (auto bodyBlock =
                std::dynamic_pointer_cast<BlockNode>(whileStmt->body)) {
                    analyzeBlock(bodyBlock);
                }
            }
            else if (const auto returnStmt =
            std::dynamic_pointer_cast<ReturnStmtNode>(stmt)){if (returnStmt->retExpr) {
                max_expression_depth =
                std::max(max_expression_depth, analyzeExpr(returnStmt->retExpr));
            }
            }
            else if (const auto callStmt =
            std::dynamic_pointer_cast<CallStmtNode>(stmt)) {
                if (const auto funcCall =
                std::dynamic_pointer_cast<FuncCallExprNode>(callStmt->call)) {
                    max_function_call_args =
                    std::max(max_function_call_args, static_cast<int>(funcCall->args.size()));
                    for (const auto& arg : funcCall->args) {
                        max_expression_depth =
                        std::max(max_expression_depth, analyzeExpr(arg));
                    }
                }
            }
            else if (auto blockStmt =
            std::dynamic_pointer_cast<BlockNode>(stmt)) {
                analyzeBlock(blockStmt);
            }
        }
    };
    analyzeBlock(block);
    // 根据分析结果计算临时空间需求
    // 函数调用参数保存空间：每个参数4字节 + 寄存器溢出保存
    int function_call_space = max_function_call_args * 4 + 7 *
    4; // 7个临时寄存器溢出
    // 表达式计算临时空间：每层嵌套需要额外的临时存储
    int expression_space = max_expression_depth * 8; // 每层8字节
    // 总临时空间 = 基础空间 + 函数调用空间 + 表达式空间
    temp_space = std::max(temp_space, function_call_space +
    expression_space);
    // 为main函数提供额外空间（通常包含更多复杂操作）
    temp_space += 200; // 额外缓冲区
    return temp_space;
}

