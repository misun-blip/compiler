//
// Created by kaixuan on 2025/7/7.
//

#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <stack>
#include "Parser.h"
#include "ASTVisitor.h"

// 寄存器分配器 - 使用 t0-t6 作为临时寄存器
class RegisterAllocator {
public:
    static constexpr int NUM_TEMP_REGS = 7; // t0~t6

    RegisterAllocator() : next_reg(0) {
        for (bool & i : reg_in_use) {
            i = false;
        }
    }

    // 尝试分配寄存器，如果失败返回空字符串
    std::string tryAllocate() {
        // 循环查找空闲寄存器
        for (int i = 0; i < NUM_TEMP_REGS; ++i) {
            if (!reg_in_use[i]) {
                reg_in_use[i] = true;
                return "t" + std::to_string(i);
            }
        }
        return ""; // 没有可用寄存器
    }

    // 强制分配寄存器，如果需要会选择一个寄存器溢出
    std::pair<std::string, int> forceAllocate(std::ostream& out, const int temp_offset) {
        std::string reg = tryAllocate();
        if (!reg.empty()) {
            return { reg, -1 }; // 不需要溢出
        }

        // 选择 t6 作为溢出寄存器（或使用LRU策略）
        reg = "t6";
        int spill_offset = temp_offset;

        // 检查偏移量是否在立即数范围内
        if (spill_offset >= -2048 && spill_offset < 2048) {
            out << "    sw " << reg << ", " << spill_offset << "(sp)  # spill\n";
        }
        else {
            // 需要使用临时寄存器来计算地址
            // 使用 addi sp, sp, -8 来临时保存 t5
            out << "    addi sp, sp, -8\n";
            out << "    sw t5, 0(sp)  # save t5\n";
            out << "    li t5, " << spill_offset << "\n";
            out << "    add t5, sp, t5\n";
            out << "    addi t5, t5, 8\n";  // 补偿临时调整的sp
            out << "    sw " << reg << ", 0(t5)  # spill\n";
            out << "    lw t5, 0(sp)  # restore t5\n";
            out << "    addi sp, sp, 8\n";
        }

        return { reg, spill_offset };
    }

    void free(const std::string& reg) {
        // 提取寄存器编号，处理t10及以上的情况
        if (reg.length() >= 2 && reg[0] == 't') {
            try {
                const int reg_num = std::stoi(reg.substr(1));
                if (reg_num >= 0 && reg_num < NUM_TEMP_REGS) {
                    reg_in_use[reg_num] = false;
                }
            }
            catch (...) {}
        }
    }

    [[nodiscard]] bool isUsed(const std::string& reg) const {
        if (reg.length() >= 2 && reg[0] == 't') {
            try {
                const int reg_num = std::stoi(reg.substr(1));
                if (reg_num >= 0 && reg_num < NUM_TEMP_REGS) {
                    return reg_in_use[reg_num];
                }
            }
            catch (...) {}
        }
        return false;
    }

    void reset() {
        next_reg = 0;
        for (bool & i : reg_in_use) {
            i = false;
        }
    }

    [[nodiscard]] int getUsedCount() const {
        int count = 0;
        for (const bool i : reg_in_use) {
            if (i) count++;
        }
        return count;
    }

private:
    int next_reg;
    bool reg_in_use[NUM_TEMP_REGS]{};
};

// 符号表条目（代码生成用）
struct CodeGenSymbol {
    std::string name;
    int offset{};  // 相对于sp的偏移
    bool is_param{};
    int param_index{};
};

// 符号表（代码生成用）
class CodeGenSymbolTable {
public:
    void enterScope() {
        scopes.emplace_back();
    }

    void exitScope() {
        if (!scopes.empty()) {
            scopes.pop_back();
        }
    }

    void addSymbol(const std::string& name, const int offset, const bool is_param = false, const int param_index = -1) {
        if (!scopes.empty()) {
            CodeGenSymbol symbol;
            symbol.name = name;
            symbol.offset = offset;
            symbol.is_param = is_param;
            symbol.param_index = param_index;
            scopes.back()[name] = symbol;
        }
    }

    [[nodiscard]] const CodeGenSymbol* lookupSymbol(const std::string& name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) {
                return &found->second;
            }
        }
        return nullptr;
    }

    CodeGenSymbol* lookupSymbol(const std::string& name) {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) {
                return &found->second;
            }
        }
        return nullptr;
    }

private:
    std::vector<std::map<std::string, CodeGenSymbol>> scopes;
};

// 代码生成器
class CodeGenerator final : public ASTVisitor {
public:
    explicit CodeGenerator(std::ostream& output)
        : out(output), label_counter(0), stack_offset(0), temp_offset(0),
        in_continue_statement(false) {
    }

    void generate(const std::shared_ptr<CompUnitNode>& root);

    // ASTVisitor接口实现
    void visit(CompUnitNode& node) override;
    int computeMaxCallArgs(const std::shared_ptr<BlockNode>& block) const;
    void visit(FuncDefNode& node) override;
    void visit(BlockNode& node) override;
    void visit(StmtNode& node) override;
    void visit(ParamNode& node) override;
    void visit(ExprNode& node) override;
    void visit(LOrExprNode& node) override;
    void visit(LAndExprNode& node) override;
    void visit(RelExprNode& node) override;
    void visit(AddExprNode& node) override;
    void visit(MulExprNode& node) override;
    void visit(UnaryExprNode& node) override;
    void visit(PrimaryExprNode& node) override;
    void visit(EmptyStmtNode& node) override;
    void visit(AssignStmtNode& node) override;
    void visit(DeclStmtNode& node) override;
    void visit(IfStmtNode& node) override;
    void visit(WhileStmtNode& node) override;
    void visit(ReturnStmtNode& node) override;
    void visit(BreakStmtNode& node) override;
    void visit(ContinueStmtNode& node) override;
    void visit(ExprStmtNode& node) override;
    void visit(FuncCallExprNode& node) override;
    void visit(CallStmtNode& node) override;

    // 支持短路跳转的表达式生成
    std::string generateExpr(const std::shared_ptr<ExprNode>& expr, const std::string& falseLabel = "");

    // 生成条件跳转代码：expr为假时跳转到falseLabel
    void generateCond(const std::shared_ptr<ExprNode>& expr, const std::string& falseLabel);

    // LOrExprNode递归条件跳转辅助
    void generateCondOr(const std::shared_ptr<LOrExprNode>& node, const std::string& falseLabel, const std::string& trueLabel);

    // 新增静态方法：智能计算临时空间需求
    static int calculateTempSpaceNeed(const
    std::shared_ptr<BlockNode>& block);
private:
    std::ostream& out;
    RegisterAllocator reg_alloc;
    CodeGenSymbolTable sym_table;
    int label_counter;                          // 标签计数器
    int stack_offset;                           // 局部变量空间偏移 (sp)
    int temp_offset;                            // 临时变量空间偏移 (sp)
    int temp_area_start{};                       // 临时变量空间起始位置 (sp)
    std::string current_function;                // 当前函数名
    std::vector<std::string> break_labels;       // 用于break语句的标签
    std::vector<std::string> continue_labels;   // 用于continue语句的标签
    std::string current_expr_result;             // 当前表达式的结果寄存器
    bool in_continue_statement;                 // 是否在continue语句中
    int current_out_arg_base = 0;  // 当前函数的出参区基址（相对sp的偏移）
    // 溢出栈，记录需要恢复的寄存器和偏移
    std::stack<std::pair<std::string, int>> spill_stack;

    // 生成唯一标签
    std::string generateLabel(const std::string& prefix) {
        return prefix + std::to_string(label_counter++);
    }

    // 辅助函数
    void emitPrologue(int local_size) const;
    void emitEpilogue(int local_size) const;
    static void alignStack(int& offset);

    static int calculateLocalSpace(const std::shared_ptr<BlockNode>& block);

    // 获取一个临时栈位置

    int getTempStackOffset() {
        const int offset = temp_offset;
        temp_offset += 4;
        // 检查栈偏移量是否超出栈帧范围
        if (temp_offset >= temp_area_end) { // 检查是否超出栈帧边界
            throw std::runtime_error("Temporary stack area overflow:offset=" + std::to_string(temp_offset) +
            ", limit=" +
            std::to_string(temp_area_end));
        }
        return offset;
    }

    // 分配寄存器或溢出
    std::string allocateRegister();

    // 释放寄存器并恢复溢出的值
    void freeRegister(const std::string& reg);

    // 新增成员变量
    int temp_area_end{}; // 临时变量空间结束位置 (sp)
    int current_function_stack_size{}; // 当前函数的总栈大小
};

#endif //CODEGENERATOR_H