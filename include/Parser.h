//
// Created by kaixuan on 2025/7/1.
//

#ifndef PARSER_H
#define PARSER_H

#include "Lexer.h"
#include <memory>
#include <utility>
#include <vector>
#include <string>
#include "ASTVisitor.h"

// AST node base class
class ASTNode {
public:
    int line;
    int column;
    ASTNode(int l, int c) : line(l), column(c) {}
    virtual ~ASTNode() = default;
    [[nodiscard]] virtual std::string toString() const = 0;
    virtual void accept(ASTVisitor& visitor) = 0;
};

// compile unit node
class CompUnitNode final : public ASTNode {
public:
    std::vector<std::shared_ptr<ASTNode>> funcDefs;
    //CompUnitNode() = default;
    CompUnitNode(int l, int c) : ASTNode(l, c) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// statement node
class StmtNode : public ASTNode {
public:
    //StmtNode() = default;
    StmtNode(int l,int c):ASTNode(l,c) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 2. 空语句（对应 ";"）
class EmptyStmtNode final : public StmtNode {
public:
    EmptyStmtNode(int l, int c) : StmtNode(l, c) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 3. 表达式语句（如 "a + b;"）
class ExprStmtNode final : public StmtNode {
public:
    std::shared_ptr<ExprNode> expr; // 存储表达式
    explicit ExprStmtNode(std::shared_ptr<ExprNode> expr,int l,int c) : StmtNode(l,c),expr(std::move(expr)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 4. 赋值语句（如 "a = 5;"）
class AssignStmtNode final : public StmtNode {
public:
    std::string id; // 变量名
    std::shared_ptr<ExprNode> expr; // 赋值表达式
    AssignStmtNode(std::string id, std::shared_ptr<ExprNode> expr,int l,int c)
        : StmtNode(l, c), id(std::move(id)), expr(std::move(expr)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 5. 变量声明语句（如 "int a = 3;"）
class DeclStmtNode final : public StmtNode {
public:
    std::string type; // 类型（目前仅int）
    std::string id; // 变量名
    std::shared_ptr<ExprNode> initExpr; // 初始化表达式（可能为nullptr）
    DeclStmtNode(std::string type, std::string id, std::shared_ptr<ExprNode> initExpr, int l, int c)
       : StmtNode(l, c), type(std::move(type)), id(std::move(id)), initExpr(std::move(initExpr)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 6. if语句（如 "if (a>0) { ... } else { ... }"）
class IfStmtNode final : public StmtNode {
public:
    std::shared_ptr<ExprNode> cond; // 条件表达式
    std::shared_ptr<StmtNode> thenStmt; // if分支语句
    std::shared_ptr<StmtNode> elseStmt; // else分支语句（可能为nullptr）
    IfStmtNode(std::shared_ptr<ExprNode> cond, std::shared_ptr<StmtNode> thenStmt, std::shared_ptr<StmtNode> elseStmt, int l, int c)
       : StmtNode(l, c), cond(std::move(cond)), thenStmt(std::move(thenStmt)), elseStmt(std::move(elseStmt)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 7. while语句（如 "while (i<10) { ... }"）
class WhileStmtNode final : public StmtNode {
public:
    std::shared_ptr<ExprNode> cond; // 循环条件
    std::shared_ptr<StmtNode> body; // 循环体语句
    WhileStmtNode(std::shared_ptr<ExprNode> cond, std::shared_ptr<StmtNode> body, int l, int c)
       : StmtNode(l, c), cond(std::move(cond)), body(std::move(body)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 8. break语句
class BreakStmtNode final : public StmtNode {
public:
    BreakStmtNode(int l, int c) : StmtNode(l, c) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 9. continue语句
class ContinueStmtNode final : public StmtNode {
public:
    ContinueStmtNode(int l, int c) : StmtNode(l, c) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 10. return语句（带返回值或不带）
class ReturnStmtNode final : public StmtNode {
public:
    std::shared_ptr<ExprNode> retExpr; // 返回表达式（可能为nullptr，对应void返回）
    ReturnStmtNode(std::shared_ptr<ExprNode> retExpr, int l, int c) : StmtNode(l, c), retExpr(std::move(retExpr)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// block node
class BlockNode final : public StmtNode {
public:
    std::vector<std::shared_ptr<StmtNode>> stmts;
    //BlockNode() = default;
    BlockNode(int l, int c) : StmtNode(l, c) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// param node
class ParamNode final : public ASTNode {
public:
    std::string type;
    std::string id;
    ParamNode(std::string type, std::string id, int l, int c) : ASTNode(l, c), type(std::move(type)), id(std::move(id)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// funcdef node
class FuncDefNode final : public ASTNode {
public:
    std::string returnType;             // function return type
    std::string id;                     // function name
    std::vector<std::shared_ptr<ParamNode>> params;    // function parameters
    std::shared_ptr<BlockNode> block;   // function body
    FuncDefNode(std::string returnType, std::string id,
              std::vector<std::shared_ptr<ParamNode>> params,
              std::shared_ptr<BlockNode> block, int l, int c)
      : ASTNode(l, c), returnType(std::move(returnType)), id(std::move(id)),
        params(std::move(params)), block(std::move(block)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// expression node
class ExprNode : public ASTNode {
public:
    //ExprNode() = default;
    ExprNode(int l, int c) : ASTNode(l, c) {}
    [[nodiscard]] std::string toString() const override = 0;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// logical or expression node
class LOrExprNode final : public ExprNode {
public:
    std::shared_ptr<ExprNode> left;     // left operand
    std::shared_ptr<ExprNode> right;    // right operand
    LOrExprNode(std::shared_ptr<ExprNode> left, std::shared_ptr<ExprNode> right, int l, int c) : ExprNode(l, c), left(std::move(left)), right(std::move(right)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// logical and expression node
class LAndExprNode final : public ExprNode {
public:
    std::shared_ptr<ExprNode> left;
    std::shared_ptr<ExprNode> right;
    LAndExprNode(std::shared_ptr<ExprNode> left, std::shared_ptr<ExprNode> right, int l, int c) : ExprNode(l, c), left(std::move(left)), right(std::move(right)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// relational expression node
class RelExprNode final : public ExprNode {
public:
    std::shared_ptr<ExprNode> left;
    std::string op;
    std::shared_ptr<ExprNode> right;
    RelExprNode(std::shared_ptr<ExprNode> left, std::string op,
                std::shared_ptr<ExprNode> right, int l, int c)
        : ExprNode(l, c), left(std::move(left)), op(std::move(op)), right(std::move(right)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// additive and subtractive expression node
class AddExprNode final : public ExprNode {
public:
    std::shared_ptr<ExprNode> left;
    std::string op;
    std::shared_ptr<ExprNode> right;
    AddExprNode(std::shared_ptr<ExprNode> left, std::string op,
               std::shared_ptr<ExprNode> right, int l, int c)
       : ExprNode(l, c), left(std::move(left)), op(std::move(op)), right(std::move(right)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// multiplicative , division, and modulo expression node
class MulExprNode final : public ExprNode {
public:
    std::shared_ptr<ExprNode> left;
    std::string op;
    std::shared_ptr<ExprNode> right;
    MulExprNode(std::shared_ptr<ExprNode> left, std::string op, std::shared_ptr<ExprNode> right, int l, int c)
        : ExprNode(l, c), left(std::move(left)), op(std::move(op)), right(std::move(right)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// unary expression node
class UnaryExprNode final : public ExprNode {
public:
    std::string op;
    std::shared_ptr<ExprNode> expr;
    UnaryExprNode(std::string op, std::shared_ptr<ExprNode> expr, int l, int c) : ExprNode(l, c), op(std::move(op)), expr(std::move(expr)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// primary expression node
class PrimaryExprNode final : public ExprNode {
public:
    std::string value;
    PrimaryExprNode(std::string value, int l, int c) : ExprNode(l, c), value(std::move(value)) {}
    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// 函数调用表达式节点
class FuncCallExprNode final : public ExprNode {
public:
    std::string funcName; // 函数名
    std::vector<std::shared_ptr<ExprNode>> args; // 函数参数列表

    FuncCallExprNode(std::string funcName, std::vector<std::shared_ptr<ExprNode>> args, int l, int c)
        : ExprNode(l, c), funcName(std::move(funcName)), args(std::move(args)) {}

    [[nodiscard]] std::string toString() const override;
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

// void函数调用语句节点
class CallStmtNode final : public StmtNode {
public:
    std::shared_ptr<FuncCallExprNode> call;
    CallStmtNode(std::shared_ptr<FuncCallExprNode> call, int l, int c) : StmtNode(l, c), call(std::move(call)) {}
    [[nodiscard]] std::string toString() const override {
        return "CallStmtNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): " + call->toString() + ";";
    }
    void accept(ASTVisitor& visitor) override { visitor.visit(*this); }
};

class Parser {
private:
    std::vector<Token> tokens;  // 存储 token 流
    size_t pos = 0;             // 当前处理的 token 位置
    Token currentToken;
    void advance();                                 // move to the next token

    std::shared_ptr<CompUnitNode> CompUnit();       // parse the whole program
    std::shared_ptr<StmtNode> Stmt();               // parse a statement
    std::shared_ptr<BlockNode> Block();
    std::shared_ptr<FuncCallExprNode> FuncCall(const std::string& funcName, int line, int column);
    std::shared_ptr<FuncDefNode> FuncDef();
    std::vector<std::shared_ptr<ParamNode>> ParamList();
    std::shared_ptr<ParamNode> Param();
    std::shared_ptr<ExprNode> Expr();
    std::shared_ptr<ExprNode> LOrExpr();
    std::shared_ptr<ExprNode> LAndExpr();
    std::shared_ptr<ExprNode> RelExpr();
    std::shared_ptr<ExprNode> AddExpr();
    std::shared_ptr<ExprNode> MulExpr();
    std::shared_ptr<ExprNode> UnaryExpr();
    std::shared_ptr<ExprNode> PrimaryExpr();
    std::shared_ptr<StmtNode> ExprStmt();
    void match(TokenType expectedType);             // check if the current token matches the expected type
public:
    explicit Parser(const std::vector<Token>& tokens);
    std::shared_ptr<CompUnitNode> parse();
};

#endif //PARSER_H