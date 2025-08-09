//
// Created by kaixuan on 2025/7/4.
//

#ifndef SEMANTICANALYZER_H
#define SEMANTICANALYZER_H

#include <memory>
#include <stdexcept>
#include "ASTVisitor.h"
#include <unordered_map>
#include <vector>
#include <string>
#include "Parser.h"

// 符号表条目结构
struct Symbol {
    std::string type;          // 类型："int"、"function"
    std::string returnType;    // 仅函数：返回类型
    std::vector<std::shared_ptr<ParamNode>> params; // 仅函数：参数列表
    bool isInitialized{};        // 仅变量：是否已初始化
    int scopeLevel{};            // 作用域层级
};

class SemanticError final : public std::runtime_error {
public:
    explicit SemanticError(const std::string& msg) : std::runtime_error(msg) {}
};

class SemanticAnalyzer : public ASTVisitor {
    // 符号表栈（嵌套作用域）
    std::vector<std::unordered_map<std::string, Symbol>> symbolTables;

    // 当前上下文信息
    std::string currentFuncReturnType; // 当前函数返回类型
    int loopDepth;                     // 循环嵌套深度
    bool inFunction;                   // 是否在函数内部

    // 内部类 TypeChecker 声明
    struct TypeChecker;

public:
    SemanticAnalyzer() : loopDepth(0), inFunction(false) {
        // 初始化全局作用域
        enterScope();
    }

    // 入口方法
    void analyze(const std::shared_ptr<CompUnitNode>& compUnit);

    // 符号表操作
    void enterScope();
    void exitScope();
    Symbol* findSymbol(const std::string& id);
    void pushSymbol(const std::string& id, const Symbol& symbol);

    // 类型检查辅助函数
    std::string checkExprType(const std::shared_ptr<ExprNode>& expr);
    static bool isTypeCompatible(const std::string& left, const std::string& right);

    // 判断是否在循环内
    [[nodiscard]] bool inLoop() const { return loopDepth > 0; }

    // 检查语句块是否包含return语句
    static bool hasReturnStatement(const std::shared_ptr<StmtNode>& stmt);

    // AST节点访问方法
    void visit(CompUnitNode& node) override;
    void visit(FuncDefNode& node) override;
    void visit(ParamNode& node) override;
    void visit(BlockNode& node) override;
    void visit(DeclStmtNode& node) override;
    void visit( AssignStmtNode& node) override;
    void visit(EmptyStmtNode& node) override;
    void visit( ExprStmtNode& node) override;
    void visit(IfStmtNode& node) override;
    void visit(WhileStmtNode& node) override;
    void visit(BreakStmtNode& node) override;
    void visit(ContinueStmtNode& node) override;
    void visit(ReturnStmtNode& node) override;

    void visit(StmtNode &node) override;
    void visit(ExprNode &node) override;
    void visit(FuncCallExprNode& node) override;
    // 表达式节点访问方法
    void visit(AddExprNode& node) override;
    void visit(MulExprNode& node) override;
    void visit(RelExprNode& node) override;
    void visit(LAndExprNode& node) override;
    void visit(LOrExprNode& node) override;
    void visit(UnaryExprNode &node) override;
    void visit(PrimaryExprNode& node) override;

    void visit(CallStmtNode& node) override;
};

struct TypeChecker : ASTVisitor {
    void visit(CallStmtNode& node) override;
};

#endif //SEMANTICANALYZER_H