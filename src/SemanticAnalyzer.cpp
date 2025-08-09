//
// Created by kaixuan on 2025/7/4.
//
#include "SemanticAnalyzer.h"
#include <algorithm>
#include <sstream>

// TypeChecker实现
struct SemanticAnalyzer::TypeChecker final : ASTVisitor {
    std::string type = "unknown";
    SemanticAnalyzer& analyzer;

    explicit TypeChecker(SemanticAnalyzer& analyzer) : analyzer(analyzer) {}

    void visit(FuncCallExprNode& node) override {
        // 1. 检查函数是否已定义
        const Symbol* funcSym = analyzer.findSymbol(node.funcName);
        if (!funcSym) {
            throw SemanticError("Undefined function '" + node.funcName + "'");
        }

        // 2. 检查找到的符号是否为函数类型
        if (funcSym->type != "function") {
            throw SemanticError("'" + node.funcName + "' is not a function");
        }

        // 3. 检查参数数量是否匹配
        if (node.args.size() != funcSym->params.size()) {
            const std::string msg = "Function '" + node.funcName + "' expects "
                            + std::to_string(funcSym->params.size()) + " arguments, but got "
                            + std::to_string(node.args.size());
            throw SemanticError(msg);
        }

        // 4. 检查每个参数的类型是否匹配
        for (size_t i = 0; i < node.args.size(); ++i) {
            std::string argType = checkType(node.args[i]);
            std::string paramType = funcSym->params[i]->type;

            if (!isTypeCompatible(argType, paramType)) {
                const std::string msg = "Function '" + node.funcName + "' argument " + std::to_string(i + 1)
                                + " type mismatch: expected '" + paramType + "', got '" + argType + "'";
                throw SemanticError(msg);
            }
        }

        // 5. 设置函数调用表达式的类型为函数返回值类型
        type = funcSym->returnType;
    }

    std::string checkType(const std::shared_ptr<ExprNode>& expr) {
        if (!expr) {
            throw SemanticError("Expression is null");
        }
        expr->accept(*this);
        return type;
    }

    // 使用 = default 简化空实现
    void visit(CompUnitNode&) override {}
    void visit(FuncDefNode&) override {}
    void visit(BlockNode&) override {}
    void visit(StmtNode&) override {}
    void visit(ParamNode&) override {}
    void visit(ExprNode&) override {}
    void visit(EmptyStmtNode&) override {}
    void visit(AssignStmtNode&) override {}
    void visit(DeclStmtNode&) override {}
    void visit(IfStmtNode&) override {}
    void visit(WhileStmtNode&) override {}
    void visit(ReturnStmtNode&) override {}
    void visit(BreakStmtNode&) override {}
    void visit(ContinueStmtNode&) override {}
    void visit(ExprStmtNode&) override {}

    void visit(AddExprNode& node) override {
        const std::string leftType = checkType(node.left);
        const std::string rightType = checkType(node.right);

        if (leftType == "int" && rightType == "int") {
            type = "int";
        } else {
            throw SemanticError("Addition requires 'int' operands, got '" + leftType + "' and '" + rightType + "'");
        }
    }

    void visit(MulExprNode& node) override {
        const std::string leftType = checkType(node.left);
        const std::string rightType = checkType(node.right);

        if (leftType != "int" || rightType != "int") {
            throw SemanticError("Multiplication requires 'int' operands, got '" + leftType + "' and '" + rightType + "'");
        }

        // 检查除零错误
        if (node.op == "/" || node.op == "%") {
            // 检查右操作数是否为常量0
            if (const auto rightPrimary = std::dynamic_pointer_cast<PrimaryExprNode>(node.right)) {
                if (std::all_of(rightPrimary->value.begin(), rightPrimary->value.end(), isdigit)) {
                    if (rightPrimary->value == "0") {
                        throw SemanticError("Division by zero");
                    }
                }
            }
        }

        type = "int";
    }

    void visit(RelExprNode& node) override {
        const std::string leftType = checkType(node.left);
        const std::string rightType = checkType(node.right);

        if (leftType != "int" || rightType != "int") {
            throw SemanticError("Relational operator requires 'int' operands, got '" + leftType + "' and '" + rightType + "'");
        }
        type = "int";
    }

    void visit(LAndExprNode& node) override {
        const std::string leftType = checkType(node.left);
        const std::string rightType = checkType(node.right);

        if (leftType != "int" || rightType != "int") {
            throw SemanticError("Logical AND requires 'int' operands, got '" + leftType + "' and '" + rightType + "'");
        }
        type = "int";
    }

    void visit(LOrExprNode& node) override {
        const std::string leftType = checkType(node.left);
        const std::string rightType = checkType(node.right);

        if (leftType != "int" || rightType != "int") {
            throw SemanticError("Logical OR requires 'int' operands, got '" + leftType + "' and '" + rightType + "'");
        }
        type = "int";
    }

    void visit(UnaryExprNode& node) override {
        const std::string operandType = checkType(node.expr);

        if (operandType != "int") {
            throw SemanticError("Unary operator requires 'int' operand, got '" + operandType + "'");
        }
        type = "int";
    }

    void visit(PrimaryExprNode& node) override {
        // 检查是否是数字
        if (std::all_of(node.value.begin(), node.value.end(), isdigit)) {
            type = "int";
        } else {
            // 是标识符，查找符号表
            const Symbol* sym = analyzer.findSymbol(node.value);
            if (!sym) {
                throw SemanticError("Undefined variable: " + node.value);
            }
            type = sym->type;
        }
    }

    void visit(CallStmtNode& node) override {
        node.call->accept(*this);
    }
};

// SemanticAnalyzer方法实现
std::string SemanticAnalyzer::checkExprType(const std::shared_ptr<ExprNode>& expr) {
    if (!expr) return "void";
    TypeChecker checker(*this);
    return checker.checkType(expr);
}

bool SemanticAnalyzer::isTypeCompatible(const std::string& left, const std::string& right) {
    return left == right;
}

void SemanticAnalyzer::analyze(const std::shared_ptr<CompUnitNode>& compUnit) {
    compUnit->accept(*this);
}

void SemanticAnalyzer::enterScope() {
    symbolTables.emplace_back();
}

void SemanticAnalyzer::exitScope() {
    if (!symbolTables.empty()) {
        symbolTables.pop_back();
    }
}

Symbol* SemanticAnalyzer::findSymbol(const std::string& id) {
    for (auto it = symbolTables.rbegin(); it != symbolTables.rend(); ++it) {
        if (it->count(id)) {
            return &it->at(id);
        }
    }
    return nullptr;
}

void SemanticAnalyzer::pushSymbol(const std::string& id, const Symbol& symbol) {
    if (symbolTables.back().count(id)) {    // 符号重定义
        throw SemanticError("Identifier " + id + " redefined in current scope");
    }
    symbolTables.back()[id] = symbol;
}

void SemanticAnalyzer::visit(CompUnitNode& node) {
    // 检查是否有main函数
    bool hasMain = false;
    for (const auto& func : node.funcDefs) {
        // 将ASTNode转换为FuncDefNode
        const auto funcDef = std::dynamic_pointer_cast<FuncDefNode>(func);
        if (funcDef && funcDef->id == "main") {
            hasMain = true;
            // 检查main函数的签名
            if (funcDef->returnType != "int") {
                throw SemanticError("Main function must return 'int'");
            }
            if (!funcDef->params.empty()) {
                throw SemanticError("Main function must have no parameters");
            }
        }
        func->accept(*this);
    }

    if (!hasMain) {
        throw SemanticError("Program must contain a 'main' function");
    }
}

void SemanticAnalyzer::visit(FuncDefNode& node) {
    // 检查函数重定义
    if (findSymbol(node.id)) {
        throw SemanticError("Function '" + node.id + "' already defined");
    }

    // 注册函数
    Symbol funcSym;
    funcSym.type = "function";
    funcSym.returnType = node.returnType;
    funcSym.params = node.params;
    pushSymbol(node.id, funcSym);

    // 进入函数作用域
    enterScope();
    currentFuncReturnType = node.returnType;
    inFunction = true;

    // 注册参数
    for (const auto& param : node.params) {
        Symbol paramSym;
        paramSym.type = param->type;
        paramSym.isInitialized = true;
        pushSymbol(param->id, paramSym);
    }

    // 检查函数体
    node.block->accept(*this);

    // 检查int函数的返回值路径
    if (node.returnType == "int") {
        if (!hasReturnStatement(node.block)) {
            throw SemanticError("Int function '" + node.id + "' must have return statement in all execution paths");
        }
    }

    // 退出函数作用域
    exitScope();
    inFunction = false;
}

void SemanticAnalyzer::visit(ParamNode& node) {
    // 参数节点在FuncDefNode中已处理
}

void SemanticAnalyzer::visit(BlockNode& node) {
    enterScope();
    for (const auto& stmt : node.stmts) {
        stmt->accept(*this);
    }
    exitScope();
}

void SemanticAnalyzer::visit(DeclStmtNode& node) {
    // 检查重定义
    if (symbolTables.back().count(node.id)) {
        throw SemanticError("Variable '" + node.id + "' already defined in current scope");
    }

    // 检查初始化表达式类型
    if (node.initExpr) {
        const std::string exprType = checkExprType(node.initExpr);
        if (!isTypeCompatible(node.type, exprType)) {
            throw SemanticError("Type mismatch in initialization");
        }
    }

    // 注册变量
    Symbol varSym;
    varSym.type = node.type;
    varSym.isInitialized = node.initExpr != nullptr;
    pushSymbol(node.id, varSym);
}

void SemanticAnalyzer::visit( AssignStmtNode& node) {
    // 检查变量是否存在
    Symbol* sym = findSymbol(node.id);
    if (!sym) {
        throw SemanticError("Undefined variable: " + node.id);
    }

    // 检查类型匹配
    if (const std::string exprType = checkExprType(node.expr); !isTypeCompatible(sym->type, exprType)) {
        throw SemanticError("Type mismatch in assignment");
    }

    sym->isInitialized = true;
}

void SemanticAnalyzer::visit(EmptyStmtNode& node) {
    // 空语句，无需检查
}

void SemanticAnalyzer::visit( ExprStmtNode& node) {
    checkExprType(node.expr);
}

void SemanticAnalyzer::visit(IfStmtNode& node) {
    if (const std::string condType = checkExprType(node.cond); condType != "int") {
        throw SemanticError("Condition in if statement must be 'int' type");
    }

    node.thenStmt->accept(*this);
    if (node.elseStmt) {
        node.elseStmt->accept(*this);
    }
}

void SemanticAnalyzer::visit(WhileStmtNode& node) {
    if (const std::string condType = checkExprType(node.cond); condType != "int") {
        throw SemanticError("Condition in while statement must be 'int' type");
    }

    loopDepth++;
    node.body->accept(*this);
    loopDepth--;
}

void SemanticAnalyzer::visit(BreakStmtNode& node) {
    if (!inLoop()) {
        throw SemanticError("'break' statement not in loop");
    }
}

void SemanticAnalyzer::visit(ContinueStmtNode& node) {
    if (!inLoop()) {
        throw SemanticError("'continue' statement not in loop");
    }
}

void SemanticAnalyzer::visit(ReturnStmtNode& node) {
    if (!inFunction) {
        throw SemanticError("'return' statement not in function");
    }

    if (node.retExpr) {
        if (const std::string retType = checkExprType(node.retExpr); !isTypeCompatible(currentFuncReturnType, retType)) {
            throw SemanticError("Return type mismatch");
        }
    } else {
        if (currentFuncReturnType != "void") {
            throw SemanticError("Non-void function must return a value");
        }
    }
}

// 基础访问方法（不应被调用）
void SemanticAnalyzer::visit(StmtNode& node) {
    throw SemanticError("Should not visit base StmtNode");
}

void SemanticAnalyzer::visit(ExprNode& node) {
    throw SemanticError("Should not visit base ExprNode");
}

void SemanticAnalyzer::visit(FuncCallExprNode& node) {
    // 检查函数是否已定义
    const Symbol* funcSym = findSymbol(node.funcName);
    if (!funcSym || funcSym->type != "function") {
        throw SemanticError("Undefined function: " + node.funcName);
    }

    // 检查函数调用顺序（函数调用必须写在被调函数声明之后）
    // 这个检查在全局作用域中进行，确保函数已经声明
    if (symbolTables.size() == 1) { // 在全局作用域中
        // 检查是否在main函数之前调用了其他函数
        if (currentFuncReturnType.empty() && node.funcName != "main") {
            throw SemanticError("Function '" + node.funcName + "' called before its declaration");
        }
    }

    // 检查参数数量是否匹配
    if (node.args.size() != funcSym->params.size()) {
        throw SemanticError("Incorrect number of arguments for function: " + node.funcName);
    }

    // 检查参数类型是否匹配
    for (size_t i = 0; i < node.args.size(); ++i) {
        const std::string argType = checkExprType(node.args[i]);
        const std::string paramType = funcSym->params[i]->type;
        if (!isTypeCompatible(argType, paramType)) {
            throw SemanticError("Type mismatch in argument " + std::to_string(i + 1) + " for function: " + node.funcName);
        }
    }
}


void SemanticAnalyzer::visit(LOrExprNode& node) {
    checkExprType(node.left);
    checkExprType(node.right);
}

void SemanticAnalyzer::visit(LAndExprNode& node) {
    checkExprType(node.left);
    checkExprType(node.right);
}

void SemanticAnalyzer::visit(RelExprNode& node) {
    checkExprType(node.left);
    checkExprType(node.right);
}

void SemanticAnalyzer::visit(AddExprNode& node) {
    checkExprType(node.left);
    checkExprType(node.right);
}

void SemanticAnalyzer::visit(MulExprNode& node) {
    checkExprType(node.left);
    checkExprType(node.right);
}

void SemanticAnalyzer::visit(UnaryExprNode& node) {
    checkExprType(node.expr);
}

void SemanticAnalyzer::visit(PrimaryExprNode& node) {
    // 检查 value 是否为纯数字（字面量）
    const bool isNumber = std::all_of(node.value.begin(), node.value.end(), isdigit);

    if (!isNumber) {
        // 如果不是数字，则认为是标识符，检查是否已定义
        if (!findSymbol(node.value)) {
            throw SemanticError("Undefined variable: " + node.value);
        }
    }
    // 如果是数字字面量，则无需额外检查
}

// 检查语句块是否包含return语句
bool SemanticAnalyzer::hasReturnStatement(const std::shared_ptr<StmtNode>& stmt) {
    if (!stmt) return false;

    // 检查是否为return语句
    if (dynamic_cast<ReturnStmtNode*>(stmt.get())) {
        return true;
    }

    // 检查是否为if语句
    if (const auto ifStmt = dynamic_cast<IfStmtNode*>(stmt.get())) {
        const bool thenHasReturn = hasReturnStatement(ifStmt->thenStmt);
        const bool elseHasReturn = ifStmt->elseStmt ? hasReturnStatement(ifStmt->elseStmt) : false;
        return thenHasReturn && elseHasReturn; // 只有两个分支都有return才返回true
    }

    // 检查是否为while语句
    if (const auto whileStmt = dynamic_cast<WhileStmtNode*>(stmt.get())) {
        return hasReturnStatement(whileStmt->body);
    }

    // 检查是否为块语句
    if (const auto blockStmt = dynamic_cast<BlockNode*>(stmt.get())) {
        for (const auto& childStmt : blockStmt->stmts) {
            if (hasReturnStatement(childStmt)) {
                return true;
            }
        }
    }

    return false;
}

void SemanticAnalyzer::visit(CallStmtNode& node) {
    node.call->accept(*this);
}