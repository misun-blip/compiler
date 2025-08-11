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

    // 🔧 加一个极简入口，兼容无CRT的运行环境，避免 main 尾声 jr ra 崩溃
    out << ".globl _start\n";
    out << "_start:\n";
    out << "    call main\n";   // a0 = main 的返回值
    out << "    li a7, 93\n";   // Linux RISC-V: exit syscall
    out << "    ecall\n\n";

    // 生成各函数代码
    root->accept(*this);
}


void CodeGenerator::visit(CompUnitNode& node) {
    for (const auto& func : node.funcDefs) {
        func->accept(*this);
    }
}

void CodeGenerator::storeWord(const std::string& src_reg, int offset, const std::string& base_reg) {
    if (offset >= -2048 && offset < 2048) {
        out << "    sw " << src_reg << ", " << offset << "(" << base_reg << ")\n";
    } else {
        out << "    li a7, " << offset << "\n";
        out << "    add a7, " << base_reg << ", a7\n";
        out << "    sw " << src_reg << ", 0(a7)\n";
    }
}

void CodeGenerator::loadWord(const std::string& dest_reg, int offset, const std::string& base_reg) {
    if (offset >= -2048 && offset < 2048) {
        out << "    lw " << dest_reg << ", " << offset << "(" << base_reg << ")\n";
    } else {
        out << "    li a7, " << offset << "\n";
        out << "    add a7, " << base_reg << ", a7\n";
        out << "    lw " << dest_reg << ", 0(a7)\n";
    }
}

void CodeGenerator::visit(FuncDefNode& node) {
    current_function = node.id;
    reg_alloc.reset();
    while (!spill_stack.empty()) {
        spill_stack.pop();
    }

    out << node.id << ":\n";
    sym_table.enterScope();

    // 计算空间（不预分配outgoing_space）
    const int frame_space = 8;  // ra(4B) + fp(4B)
    const int param_space = static_cast<int>(node.params.size()) * 4;
    const int local_space = calculateLocalSpace(node.block);
    int temp_space = calculateTempSpaceNeed(node.block);

    // 总帧大小
    int frame_size = param_space + local_space + temp_space + frame_space;
    alignStack(frame_size);
    current_function_stack_size = frame_size;

    // 生成序言
    emitPrologue(frame_size);

    // 处理形参 - 使用fp负偏移
    for (size_t i = 0; i < node.params.size(); ++i) {
        const std::string& param_id = node.params[i]->id;
        const int local_off = -static_cast<int>((i + 1) * 4);  // 负偏移
        if (i < 8) {
            storeWord("a" + std::to_string(i), local_off);
        } else {
            const int incoming_off = (i - 8) * 4;
            std::string temp_reg = allocateRegister();
            loadWord(temp_reg, incoming_off);  // incoming_off 正，相对fp
            storeWord(temp_reg, local_off);
            freeRegister(temp_reg);
        }

        sym_table.addSymbol(param_id, local_off, true, static_cast<int>(i));
    }

    // 设置局部变量起始偏移
    current_stack_off = -param_space - 4;  // 负偏移，跳过参数

    // 临时区（使用fp负偏移）
    temp_area_start = -param_space - local_space;
    temp_area_end = temp_area_start - temp_space;

    // 生成函数体
    node.block->accept(*this);

    // main默认返回0
    if (node.id == "main") {
        out << "    li a0, 0\n";
    }

    // 尾声
    out << current_function << "_epilogue:\n";
    emitEpilogue(frame_size);

    sym_table.exitScope();
    out << "\n";
}
// 计算局部变量的空间需求
int CodeGenerator::calculateLocalSpace(const std::shared_ptr<BlockNode>& block) {
    int space = 0;

    std::function<void(const std::shared_ptr<BlockNode>&)> calculateSpace =
        [&](const std::shared_ptr<BlockNode>& current_block) {
            for (const auto& stmt : current_block->stmts) {
                if (const auto decl = std::dynamic_pointer_cast<DeclStmtNode>(stmt)) {
                    // 每个声明都需要空间，即使变量名相同（在不同作用域）
                    space += 4;
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
    std::string reg = reg_alloc.tryAllocate();
    if (!reg.empty()) {
        return reg;
    }

    auto [forced_reg, spill_offset] = reg_alloc.forceAllocate(out, getTempStackOffset());
    if (spill_offset != -1) {  // 改为 != -1，因为-1表示无溢出
        spill_stack.emplace(forced_reg, spill_offset);
    }
    return forced_reg;
}

void CodeGenerator::freeRegister(const std::string& reg) {
    reg_alloc.free(reg);

    if (!spill_stack.empty() && spill_stack.top().first == reg) {
        auto [spilled_reg, offset] = spill_stack.top();
        spill_stack.pop();
        loadWord(spilled_reg, offset);  // 统一用loadWord
        out << "  # restore\n";  // 可选注释
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
    storeWord(result_reg, sym->offset);
    freeRegister(result_reg);
}

void CodeGenerator::visit(DeclStmtNode& node) {
    sym_table.addSymbol(node.id, current_stack_off);

    if (node.initExpr) {
        const std::string result_reg = generateExpr(node.initExpr);
        storeWord(result_reg, current_stack_off);
        freeRegister(result_reg);
    } else {
        storeWord("x0", current_stack_off);
    }

    current_stack_off -= 4;
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
        const CodeGenSymbol* sym = sym_table.lookupSymbol(node.value);
        if (!sym) {
            throw std::runtime_error("Undefined variable: " + node.value);
        }
        const std::string result_reg = allocateRegister();
        loadWord(result_reg, sym->offset);
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
    // 1) 为“第9个及以后”的栈上传参预留空间
    int stack_args = 0;
    if (node.args.size() > 8) {
        stack_args = static_cast<int>(node.args.size()) - 8;
        int bytes = stack_args * 4;
        if (bytes >= -2048 && bytes < 2048) {
            out << "    addi sp, sp, -" << bytes << "\n";
        } else {
            out << "    li t0, " << bytes << "\n";
            out << "    sub sp, sp, t0\n";
        }
    }

    // 2) 逐个实参：算一个 → 就位一个 → 立刻释放寄存器
    for (size_t i = 0; i < node.args.size(); ++i) {
        std::string r = generateExpr(node.args[i]);

        if (i < 8) {
            out << "    mv a" << i << ", " << r << "\n";
        } else {
            // 直接写入我们刚刚在当前 sp 之上的“入参区”
            storeWord(r, static_cast<int>((i - 8) * 4), "sp");
        }

        freeRegister(r);  // 立刻释放，避免被后续计算覆盖
    }

    // 3) 发起调用
    out << "    jal ra, " << node.funcName << "\n";

    // 4) 回收“溢出参数”的栈空间
    if (stack_args > 0) {
        int bytes = stack_args * 4;
        if (bytes >= -2048 && bytes < 2048) {
            out << "    addi sp, sp, " << bytes << "\n";
        } else {
            out << "    li t0, " << bytes << "\n";
            out << "    add sp, sp, t0\n";
        }
    }

    // 5) a0 作为调用表达式结果，拷到一个临时寄存器里返回给上层
    std::string res = allocateRegister();
    out << "    mv " << res << ", a0\n";
    current_expr_result = res;
}

void CodeGenerator::visit(CallStmtNode& node) {
    // 直接复用函数调用表达式的生成逻辑
    node.call->accept(*this);

    // 这是“语句形式”的调用，返回值没人用，释放掉表达式里占用的寄存器即可
    if (!current_expr_result.empty()) {
        freeRegister(current_expr_result);
        // 可选：清空标记，避免上层误用
        current_expr_result.clear();
    }
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
    int temp_space = 256;
    int max_function_call_args = 0;
    int max_expression_depth = 0;
    int max_arg_prep_space = 0;
    int max_nested_calls = 0;  // 新增：嵌套调用深度

    std::function<void(const std::shared_ptr<BlockNode>&)> analyzeBlock;
    std::function<int(const std::shared_ptr<ExprNode>&, int)> analyzeExpr;

    analyzeExpr = [&](const std::shared_ptr<ExprNode>& expr, int call_depth) -> int {
        if (!expr) return 0;

        if (const auto funcCall = std::dynamic_pointer_cast<FuncCallExprNode>(expr)) {
            max_function_call_args = std::max(max_function_call_args, static_cast<int>(funcCall->args.size()));
            max_nested_calls = std::max(max_nested_calls, call_depth + 1);

            int arg_prep_count = 0;
            for (const auto& arg : funcCall->args) {
                if (std::dynamic_pointer_cast<AddExprNode>(arg) ||
                    std::dynamic_pointer_cast<MulExprNode>(arg) ||
                    std::dynamic_pointer_cast<FuncCallExprNode>(arg)) {
                    arg_prep_count++;
                }
            }
            max_arg_prep_space = std::max(max_arg_prep_space, arg_prep_count * 4);

            int depth = 1;
            for (const auto& arg : funcCall->args) {
                depth = std::max(depth, 1 + analyzeExpr(arg, call_depth + 1));
            }
            return depth;
        }
        else if (const auto add = std::dynamic_pointer_cast<AddExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(add->left, call_depth), analyzeExpr(add->right, call_depth));
        }
        else if (const auto mul = std::dynamic_pointer_cast<MulExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(mul->left, call_depth), analyzeExpr(mul->right, call_depth));
        }
        else if (const auto rel = std::dynamic_pointer_cast<RelExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(rel->left, call_depth), analyzeExpr(rel->right, call_depth));
        }
        else if (const auto lor = std::dynamic_pointer_cast<LOrExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(lor->left, call_depth), analyzeExpr(lor->right, call_depth));
        }
        else if (const auto land = std::dynamic_pointer_cast<LAndExprNode>(expr)) {
            return 1 + std::max(analyzeExpr(land->left, call_depth), analyzeExpr(land->right, call_depth));
        }
        else if (const auto unary = std::dynamic_pointer_cast<UnaryExprNode>(expr)) {
            return 1 + analyzeExpr(unary->expr, call_depth);
        }
        return 1;
    };

    analyzeBlock = [&](const std::shared_ptr<BlockNode>& blk) {
        for (const auto& stmt : blk->stmts) {
            if (const auto exprStmt = std::dynamic_pointer_cast<ExprStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(exprStmt->expr, 0));
            }
            else if (const auto assignStmt = std::dynamic_pointer_cast<AssignStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(assignStmt->expr, 0));
            }
            else if (const auto declStmt = std::dynamic_pointer_cast<DeclStmtNode>(stmt)) {
                if (declStmt->initExpr) {
                    max_expression_depth = std::max(max_expression_depth, analyzeExpr(declStmt->initExpr, 0));
                }
            }
            else if (const auto ifStmt = std::dynamic_pointer_cast<IfStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(ifStmt->cond, 0));
                if (auto thenBlock = std::dynamic_pointer_cast<BlockNode>(ifStmt->thenStmt))
                    analyzeBlock(thenBlock);
                if (ifStmt->elseStmt) {
                    if (auto elseBlock = std::dynamic_pointer_cast<BlockNode>(ifStmt->elseStmt))
                        analyzeBlock(elseBlock);
                }
            }
            else if (const auto whileStmt = std::dynamic_pointer_cast<WhileStmtNode>(stmt)) {
                max_expression_depth = std::max(max_expression_depth, analyzeExpr(whileStmt->cond, 0));
                if (auto bodyBlock = std::dynamic_pointer_cast<BlockNode>(whileStmt->body))
                    analyzeBlock(bodyBlock);
            }
            else if (const auto returnStmt = std::dynamic_pointer_cast<ReturnStmtNode>(stmt)) {
                if (returnStmt->retExpr) {
                    max_expression_depth = std::max(max_expression_depth, analyzeExpr(returnStmt->retExpr, 0));
                }
            }
            else if (const auto callStmt = std::dynamic_pointer_cast<CallStmtNode>(stmt)) {
                if (const auto funcCall = std::dynamic_pointer_cast<FuncCallExprNode>(callStmt->call)) {
                    max_function_call_args = std::max(max_function_call_args, static_cast<int>(funcCall->args.size()));
                    int arg_prep_count = 0;
                    for (const auto& arg : funcCall->args) {
                        if (std::dynamic_pointer_cast<AddExprNode>(arg) ||
                            std::dynamic_pointer_cast<MulExprNode>(arg) ||
                            std::dynamic_pointer_cast<FuncCallExprNode>(arg)) {
                            arg_prep_count++;
                        }
                        max_expression_depth = std::max(max_expression_depth, analyzeExpr(arg, 0));
                    }
                    max_arg_prep_space = std::max(max_arg_prep_space, arg_prep_count * 4);
                }
            }
            else if (auto subBlk = std::dynamic_pointer_cast<BlockNode>(stmt)) {
                analyzeBlock(subBlk);
            }
        }
    };

    analyzeBlock(block);

    // 基础空间计算
    int function_call_space = max_function_call_args * 8;  // 增加每个参数的空间
    int expression_space = max_expression_depth * 16;      // 增加表达式栈空间
    int nested_call_space = max_nested_calls * 64;         // 嵌套调用额外空间

    temp_space = std::max(temp_space, function_call_space + expression_space + max_arg_prep_space + nested_call_space);

    // 增加基础缓冲区
    temp_space += 1024;  // 从512增加到1024

    // 大参数函数额外空间
    if (max_function_call_args > 8) {
        temp_space += (max_function_call_args - 8) * 16;  // 增加栈参数空间
    }

    // 寄存器保存区
    temp_space += RegisterAllocator::NUM_TEMP_REGS * 4 + 64;  // 增加安全边界

    // 确保最小空间
    temp_space = std::max(temp_space, 2048);  // 最小2KB

    // 对齐到16B
    if (temp_space % 16 != 0) {
        temp_space = ((temp_space / 16) + 1) * 16;
    }

    return temp_space;
}