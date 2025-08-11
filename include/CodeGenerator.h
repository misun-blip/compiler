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

#include <list>
#include <unordered_map>
#include <unordered_set>

// 寄存器分配器 - 使用 t0-t6 作为临时寄存器，使用LRU spill策略
class RegisterAllocator {
public:
    static constexpr int NUM_TEMP_REGS = 7; // t0~t6

    RegisterAllocator() {
        reset();
    }

    // 尝试分配寄存器，如果失败返回空字符串
    std::string tryAllocate() {
        if (!free_regs.empty()) {
            auto it = free_regs.begin();
            std::string reg = *it;
            free_regs.erase(it);
            lru_used.push_back(reg);
            reg_to_iter[reg] = --lru_used.end();
            return reg;
        }
        return ""; // 没有可用寄存器
    }

    // 强制标记寄存器为已使用
    void markUsed(const std::string& reg) {
        if (free_regs.count(reg)) {
            free_regs.erase(reg);
            lru_used.push_back(reg);
            reg_to_iter[reg] = --lru_used.end();
        } else if (reg_to_iter.count(reg)) {
            // 已使用，移动到最近
            lru_used.splice(lru_used.end(), lru_used, reg_to_iter[reg]);
            reg_to_iter[reg] = --lru_used.end();
        }
    }

    // 强制分配寄存器，如果需要会选择一个寄存器溢出（LRU策略）
    std::pair<std::string, int> forceAllocate(std::ostream& out, const int temp_offset) {
        std::string reg = tryAllocate();
        if (!reg.empty()) {
            return { reg, -1 }; // 不需要溢出
        }

        if (lru_used.empty()) {
            throw std::runtime_error("No registers to spill");
        }

        // 选择LRU (front)
        reg = lru_used.front();
        lru_used.pop_front();
        reg_to_iter.erase(reg);

        // Spill
        if (temp_offset >= -2048 && temp_offset < 2048) {
            out << "    sw " << reg << ", " << temp_offset << "(fp)  # spill\n";
        } else {
            out << "    li a7, " << temp_offset << "\n";
            out << "    add a7, fp, a7\n";
            out << "    sw " << reg << ", 0(a7)  # spill\n";
        }

        // 放回作为最近使用
        lru_used.push_back(reg);
        reg_to_iter[reg] = --lru_used.end();

        return { reg, temp_offset };
    }

    void free(const std::string& reg) {
        auto it = reg_to_iter.find(reg);
        if (it != reg_to_iter.end()) {
            lru_used.erase(it->second);
            reg_to_iter.erase(it);
        }
        free_regs.insert(reg);
    }

    [[nodiscard]] bool isUsed(const std::string& reg) const {
        return !free_regs.count(reg);
    }

    void reset() {
        free_regs = {"t0", "t1", "t2", "t3", "t4", "t5", "t6"};
        lru_used.clear();
        reg_to_iter.clear();
    }

    [[nodiscard]] int getUsedCount() const {
        return NUM_TEMP_REGS - free_regs.size();
    }

private:
    std::unordered_set<std::string> free_regs;
    std::list<std::string> lru_used;
    std::unordered_map<std::string, std::list<std::string>::iterator> reg_to_iter;
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

    void storeWord(const std::string& src_reg, int offset, const std::string& base_reg = "fp");
    void loadWord(const std::string& dest_reg, int offset, const std::string& base_reg = "fp");

    // ASTVisitor接口实现
    void visit(CompUnitNode& node) override;
    //int computeMaxCallArgs(const std::shared_ptr<BlockNode>& block) const;
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
    //int current_out_arg_base = 0;  // 当前函数的出参区基址（相对sp的偏移）
    int current_stack_off = 0;  // 当前栈偏移（负值，用于局部变量）
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
        int offset = temp_area_start - temp_offset;  // - temp_offset，向下分配

        // 检查是否超出临时区（向下：offset < temp_area_end）
        if (offset - 4 < temp_area_end) {  // 下一个分配会超出
            temp_offset = 0;
            offset = temp_area_start;
        }

        int result = offset;
        temp_offset += 4;
        return result;
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