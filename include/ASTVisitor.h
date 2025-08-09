#ifndef AST_VISITOR_H
#define AST_VISITOR_H

// 前向声明AST节点类
class CompUnitNode;
class FuncDefNode;
class BlockNode;
class StmtNode;
class ParamNode;
class ExprNode;
class LOrExprNode;
class LAndExprNode;
class RelExprNode;
class AddExprNode;
class MulExprNode;
class UnaryExprNode;
class PrimaryExprNode;
class EmptyStmtNode;
class AssignStmtNode;
class DeclStmtNode;
class IfStmtNode;
class WhileStmtNode;
class ReturnStmtNode;
class BreakStmtNode;
class ContinueStmtNode;
class ExprStmtNode;
class FuncCallExprNode;
class CallStmtNode;

// 访问者接口，每个节点对应一个visit方法
class ASTVisitor {
public:
    virtual ~ASTVisitor() = default;
    virtual void visit(CompUnitNode& node) = 0;
    virtual void visit(FuncDefNode& node) = 0;
    virtual void visit(BlockNode& node) = 0;
    virtual void visit(StmtNode& node) = 0;
    virtual void visit(ParamNode& node) = 0;
    virtual void visit(ExprNode& node) = 0;
    virtual void visit(LOrExprNode& node) = 0;
    virtual void visit(LAndExprNode& node) = 0;
    virtual void visit(RelExprNode& node) = 0;
    virtual void visit(AddExprNode& node) = 0;
    virtual void visit(MulExprNode& node) = 0;
    virtual void visit(UnaryExprNode& node) = 0;
    virtual void visit(PrimaryExprNode& node) = 0;
    virtual void visit(EmptyStmtNode& node) = 0;
    virtual void visit(AssignStmtNode& node) = 0;
    virtual void visit(DeclStmtNode& node) = 0;
    virtual void visit(IfStmtNode& node) = 0;
    virtual void visit(WhileStmtNode& node) = 0;
    virtual void visit(ReturnStmtNode& node) = 0;
    virtual void visit(BreakStmtNode& node) = 0;
    virtual void visit(ContinueStmtNode& node) = 0;
    virtual void visit(ExprStmtNode& node) = 0;
    virtual void visit(FuncCallExprNode& node) = 0;
    virtual void visit(CallStmtNode& node) = 0;
};

#endif // AST_VISITOR_H