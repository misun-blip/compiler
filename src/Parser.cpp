#include "Parser.h"
#include "Lexer.h"  // 引入TokenType和tokenTypeToString声明
#include <sstream>
#include <stdexcept>

// AST节点toString实现
std::string CompUnitNode::toString() const {
    std::stringstream ss;
    ss << "CompUnitNode: {\n";
    for (const auto& func : funcDefs) {
        ss << "  " << func->toString() << "\n";
    }
    ss << "}";
    return ss.str();
}

std::string StmtNode::toString() const {
    return "StmtNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + ")";
}


// 1. 空语句（";"）
std::string EmptyStmtNode::toString() const {
    return "EmptyStmtNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): ;";
}

// 2. 表达式语句（如 "a + b;"）
std::string ExprStmtNode::toString() const {
    return "ExprStmtNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): (" + expr->toString() + ");";
}

// 3. 赋值语句（如 "a = 5;"）
std::string AssignStmtNode::toString() const {
    return "AssignStmtNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): " + id + " = (" + expr->toString() + ");";
}

// 4. 变量声明语句（如 "int a = 3;"）
std::string DeclStmtNode::toString() const {
    std::stringstream ss;
    ss << "DeclStmtNode (line: " << line << ", column: " << column << "): " << type << " " << id;
    if (initExpr) { // 若有初始化表达式
        ss << " = (" << initExpr->toString() << ")";
    }
    ss << ";";
    return ss.str();
}

// 5. if语句
std::string IfStmtNode::toString() const {
    std::stringstream ss;
    ss << "IfStmtNode (line: " << line << ", column: " << column << "): \n  if (" << cond->toString() << ")\n  "
      << thenStmt->toString().replace(0, 0, "  "); // 缩进分支语句
    if (elseStmt) { // 若有else分支
        ss << "\n  else  "
           << elseStmt->toString().replace(0, 0, "  ");
    }
    return ss.str();
}

// 6. while语句
std::string WhileStmtNode::toString() const {
    std::stringstream ss;
    ss << "WhileStmtNode: while (" << cond->toString() << ")\n  "
       << body->toString().replace(0, 0, "  "); // 缩进循环体
    return ss.str();
}

// 7. break语句
std::string BreakStmtNode::toString() const {
    return "BreakStmtNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): break;";
}

// 8. continue语句
std::string ContinueStmtNode::toString() const {
    return "ContinueStmtNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): continue;";
}

// 9. return语句
std::string ReturnStmtNode::toString() const {
    std::stringstream ss;
    ss << "ReturnStmtNode (line: " << line << ", column: " << column << "): return";
    if (retExpr) { // 若有返回值
        ss << " (" << retExpr->toString() << ")";
    }
    ss << ";";
    return ss.str();
}


std::string BlockNode::toString() const {
    std::stringstream ss;
 ss << "BlockNode (line: " << line << ", column: " << column << "): {\n";    for (const auto& stmt : stmts) {
        // 缩进子语句（替换每行开头为4个空格）
        std::string stmtStr = stmt->toString();
        stmtStr = "    " + stmtStr; // 首行缩进
        size_t pos = stmtStr.find('\n');
        while (pos != std::string::npos) {
            stmtStr.insert(pos + 1, "    ");
            pos = stmtStr.find('\n', pos + 5);
        }
        ss << stmtStr << "\n";
    }
    ss << "  }";
    return ss.str();
}

std::string FuncDefNode::toString() const {
    std::stringstream ss;
    ss << "FuncDefNode (line: " << line << ", column: " << column << "): " << returnType << " " << id << "(";
    for (size_t i = 0; i < params.size(); ++i) {
        ss << params[i]->type << " " << params[i]->id;
        if (i < params.size() - 1) ss << ", ";
    }
    ss << ") " << block->toString();
    return ss.str();
}

std::string ParamNode::toString() const {
    return "ParamNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): " + type + " " + id;
}


std::string ExprNode::toString() const {
    return "ExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + ")";
}

std::string LOrExprNode::toString() const {
    return "LOrExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): (" + left->toString() + " || " + right->toString() + ")";
}


std::string LAndExprNode::toString() const {
    return "LAndExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): (" + left->toString() + " && " + right->toString() + ")";
}

std::string RelExprNode::toString() const {
    return "RelExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): (" + left->toString() + " " + op + " " + right->toString() + ")";
}

std::string AddExprNode::toString() const {
    return "AddExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): (" + left->toString() + " " + op + " " + right->toString() + ")";
}

std::string MulExprNode::toString() const {
    return "MulExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): (" + left->toString() + " " + op + " " + right->toString() + ")";
}

std::string UnaryExprNode::toString() const {
    return "UnaryExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): (" + op + " " + expr->toString() + ")";
}

std::string PrimaryExprNode::toString() const {
    return "PrimaryExprNode (line: " + std::to_string(line) + ", column: " + std::to_string(column) + "): " + value;
}

std::string FuncCallExprNode::toString() const {
    std::ostringstream oss;
    oss << funcName << "(";
    for (size_t i = 0; i < args.size(); ++i) {
        if (i > 0) {
            oss << ", ";
        }
        oss << args[i]->toString();
    }
    oss << ")";
    return oss.str();
}


// Parser.cpp 中修改构造函数
Parser::Parser(const std::vector<Token>& tokenList)
    : tokens(tokenList),
      pos(0),
      currentToken(TokenType::EOF_TOKEN, "", 0, 0)   // 初始化列表中临时初始化
{
    // 修正 currentToken 为实际的第一个 token
    if (pos < tokens.size()) {
        currentToken = tokens[pos];  // 覆盖临时值
        pos++;
    } else {
      currentToken = {TokenType::EOF_TOKEN, "", 0, 0};   // 保持 EOF
    }

}

void Parser::advance() {
    if (pos < tokens.size()) {
        currentToken = tokens[pos];
        pos++;
    } else {
        currentToken = {TokenType::EOF_TOKEN, "", 0, 0};
    }
}

void Parser::match(TokenType expectedType) {
    if (currentToken.type == expectedType) {
        advance();
    } else {
        throw std::runtime_error("Expected token: " + tokenTypeToString(expectedType) +
                                 ", but got: " + tokenTypeToString(currentToken.type)+
                                 " (value: " + currentToken.value + ") at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
    }
}


std::shared_ptr<FuncCallExprNode> Parser::FuncCall(const std::string& funcName, int line, int column) {
    match(TokenType::LPAREN);

    std::vector<std::shared_ptr<ExprNode>> args;
    if (currentToken.type != TokenType::RPAREN) {
        args.push_back(Expr());
        while (currentToken.type == TokenType::COMMA) {
            match(TokenType::COMMA);
            args.push_back(Expr());
        }
    }
    match(TokenType::RPAREN);

    return std::make_shared<FuncCallExprNode>(funcName, args, line, column);
}


// 编译单元解析
std::shared_ptr<CompUnitNode> Parser::CompUnit() {
    auto compUnit = std::make_shared<CompUnitNode>(currentToken.line, currentToken.column);
    // 解析所有函数定义（匹配文法CompUnit → FuncDef+）
    while (currentToken.type == TokenType::INT || currentToken.type == TokenType::VOID) {
        compUnit->funcDefs.push_back(FuncDef());
    }
    return compUnit;
}

// 语句解析
std::shared_ptr<StmtNode> Parser::Stmt() {
    if (currentToken.type == TokenType::LBRACE) {
        return Block();
    }
    if (currentToken.type == TokenType::SEMICOLON) {
        int line = currentToken.line;
        int column = currentToken.column;
        advance();
        return std::make_shared<EmptyStmtNode>(line, column);
    }
    if (currentToken.type == TokenType::INT) {
        // 检查是否为函数定义：int id(params) { ... }
        // 先保存当前位置，以便回退
        size_t savedPos = pos;
        Token savedToken = currentToken;

        // 尝试匹配 int id
        match(TokenType::INT);
        if (currentToken.type != TokenType::IDENTIFIER) {
            // 回退到原始位置
            pos = savedPos;
            currentToken = savedToken;
            throw std::runtime_error("Expected identifier after 'int' at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
        }

        // 检查下一个token是否为左括号
        std::string id = currentToken.value;
        match(TokenType::IDENTIFIER);

        if (currentToken.type == TokenType::LPAREN) {
            // 这是函数定义，回退到原始位置，让上层处理
            pos = savedPos;
            currentToken = savedToken;
            throw std::runtime_error("Function definition not allowed in statement context at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
        }

        // 这是变量声明：int id (= expr)?; (可选初始化)
        std::string type = "int";
        int line = savedToken.line;
        int column = savedToken.column;
        std::shared_ptr<ExprNode> initExpr = nullptr;

        if (currentToken.type == TokenType::ASSIGN) {
            match(TokenType::ASSIGN);
            initExpr = Expr(); // 解析初始化表达式
        }

        match(TokenType::SEMICOLON);
        return std::make_shared<DeclStmtNode>(type, id, initExpr, line, column);
    }
    if (currentToken.type == TokenType::IDENTIFIER) {
        // 可能是赋值语句、void函数调用或表达式语句
        std::string id = currentToken.value;
        int line = currentToken.line;
        int column = currentToken.column;
        advance();
        if (currentToken.type == TokenType::ASSIGN) {
            match(TokenType::ASSIGN);
            auto expr = Expr();
            match(TokenType::SEMICOLON);
            return std::make_shared<AssignStmtNode>(id, expr, line, column);
        }
        if (currentToken.type == TokenType::LPAREN) {
            // void函数调用语句
            auto call = FuncCall(id, line, column);
            match(TokenType::SEMICOLON);
            return std::make_shared<CallStmtNode>(call, line, column);
        }
        // 其它情况为表达式语句
        // 需要回退一个位置，因为我们已经消费了标识符
        pos--;
        currentToken = tokens[pos];
        return ExprStmt();
    }
    if (currentToken.type == TokenType::IF) {
        // if语句：if (expr) stmt (else stmt)?
        int line = currentToken.line;
        int column = currentToken.column;
        match(TokenType::IF);
        match(TokenType::LPAREN);
        auto cond = Expr();
        match(TokenType::RPAREN);
        auto thenStmt = Stmt();
        std::shared_ptr<StmtNode> elseStmt = nullptr;
        if (currentToken.type == TokenType::ELSE) {
            match(TokenType::ELSE);
            elseStmt = Stmt();
        }
        return std::make_shared<IfStmtNode>(cond, thenStmt, elseStmt, line, column);
    }
    if (currentToken.type == TokenType::WHILE) {
        // while语句：while (expr) stmt
        int line = currentToken.line;
        int column = currentToken.column;
        match(TokenType::WHILE);
        match(TokenType::LPAREN);
        auto cond = Expr();
        match(TokenType::RPAREN);
        auto body = Stmt();
        return std::make_shared<WhileStmtNode>(cond, body, line, column);
    }
    if (currentToken.type == TokenType::BREAK) {
        int line = currentToken.line;
        int column = currentToken.column;
        match(TokenType::BREAK);
        match(TokenType::SEMICOLON);
        return std::make_shared<BreakStmtNode>(line, column);
    }
    if (currentToken.type == TokenType::CONTINUE) {
        int line = currentToken.line;
        int column = currentToken.column;
        match(TokenType::CONTINUE);
        match(TokenType::SEMICOLON);
        return std::make_shared<ContinueStmtNode>(line, column);
    }
    if (currentToken.type == TokenType::RETURN) {
        int line = currentToken.line;
        int column = currentToken.column;
        match(TokenType::RETURN);
        std::shared_ptr<ExprNode> retExpr = nullptr;
        if (currentToken.type != TokenType::SEMICOLON) {
            retExpr = Expr(); // 返回表达式
        }
        match(TokenType::SEMICOLON);
        return std::make_shared<ReturnStmtNode>(retExpr, line, column);
    }
    throw std::runtime_error("Unknown statement type at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
}


// 块解析
std::shared_ptr<BlockNode> Parser::Block() {
    int line = currentToken.line;
    int column = currentToken.column;
    auto block = std::make_shared<BlockNode>(line, column);
    match(TokenType::LBRACE);
    while (currentToken.type != TokenType::RBRACE) {
        if (currentToken.type == TokenType::EOF_TOKEN) {
            throw std::runtime_error("Unclosed block (missing '}') at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
        }
        block->stmts.push_back(Stmt()); // 解析块内所有语句
    }
    match(TokenType::RBRACE);
    return block;
}




// 函数定义解析
std::shared_ptr<FuncDefNode> Parser::FuncDef() {
    std::string returnType;
    int line = currentToken.line;
    int column = currentToken.column;
    if (currentToken.type == TokenType::INT) {
        returnType = "int";
        match(TokenType::INT);
    } else if (currentToken.type == TokenType::VOID) {
        returnType = "void";
        match(TokenType::VOID);
    } else {
        throw std::runtime_error("Syntax error: expected INT or VOID at line "
            + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
    }

    // 解析函数名
    if (currentToken.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected function name at line "
            + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
    }
    std::string id = currentToken.value;
    match(TokenType::IDENTIFIER);

    // 解析参数列表
    match(TokenType::LPAREN);
    std::vector<std::shared_ptr<ParamNode>> params;

    // 处理空参数列表的情况
    if (currentToken.type != TokenType::RPAREN) {
        params = ParamList();  // 调用 ParamList 解析参数
    }

    match(TokenType::RPAREN);  // 匹配右括号

    auto block = Block();
    return std::make_shared<FuncDefNode>(returnType, id, params, block, line, column);
}

// 参数列表解析
std::vector<std::shared_ptr<ParamNode>> Parser::ParamList() {
    std::vector<std::shared_ptr<ParamNode>> params;

    // 至少解析一个参数
    params.push_back(Param());

    // 解析后续参数（如果有逗号分隔）
    while (currentToken.type == TokenType::COMMA) {
        match(TokenType::COMMA);
        if (currentToken.type != TokenType::INT) {
            throw std::runtime_error("Missing parameter type after comma at line "
                + std::to_string(currentToken.line));
        }
        params.push_back(Param());
    }

    return params;
}

// 参数解析
std::shared_ptr<ParamNode> Parser::Param() {
    std::string type;
    int line = currentToken.line;
    int column = currentToken.column;
    if (currentToken.type == TokenType::INT) {
        type = "int";
        match(TokenType::INT);
    } else {
        throw std::runtime_error("Syntax error: expected INT for parameter type at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
    }
    if (currentToken.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected parameter name at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column));
    }
    std::string id = currentToken.value;
    match(TokenType::IDENTIFIER);
    return std::make_shared<ParamNode>(type, id, line, column);
}

// 表达式解析（入口）
std::shared_ptr<ExprNode> Parser::Expr() {
    return LOrExpr(); // 从逻辑或表达式开始解析
}

// 逻辑或表达式解析（LOrExpr → LAndExpr | LOrExpr || LAndExpr）
std::shared_ptr<ExprNode> Parser::LOrExpr() {
    auto left = LAndExpr();
    while (currentToken.type == TokenType::LOGICAL_OR) {
        std::string op = tokenTypeToString(currentToken.type);
        int line = currentToken.line;
        int column = currentToken.column;
        match(TokenType::LOGICAL_OR);
        auto right = LAndExpr();
        left = std::make_shared<LOrExprNode>(left, right, line, column);
    }
    return left;
}

// 逻辑与表达式解析（LAndExpr → RelExpr | LAndExpr && RelExpr）
std::shared_ptr<ExprNode> Parser::LAndExpr() {
    auto left = RelExpr();
    while (currentToken.type == TokenType::LOGICAL_AND) {
        std::string op = tokenTypeToString(currentToken.type);
        int line = currentToken.line;
        int column = currentToken.column;
        match(TokenType::LOGICAL_AND);
        auto right = RelExpr();
        left = std::make_shared<LAndExprNode>(left, right, line, column);
    }
    return left;
}

// 关系表达式解析（RelExpr → AddExpr | RelExpr (<|>|<=|>=|==|!=) AddExpr）
std::shared_ptr<ExprNode> Parser::RelExpr() {
    auto left = AddExpr();
    while (currentToken.type == TokenType::LT ||
           currentToken.type == TokenType::GT ||
           currentToken.type == TokenType::LTE ||
           currentToken.type == TokenType::GTE ||
           currentToken.type == TokenType::EQ ||
           currentToken.type == TokenType::NEQ) {
        // 记录关系运算符的位置
        std::string op = currentToken.value;
        int line = currentToken.line;
        int column = currentToken.column;
        advance();
        auto right = AddExpr();
        left = std::make_shared<RelExprNode>(left, op, right, line, column); // 传递位置
           }
    return left;
}

// 加减表达式解析（AddExpr → MulExpr | AddExpr (+|-) MulExpr）
std::shared_ptr<ExprNode> Parser::AddExpr() {
    auto left = MulExpr();
    while (currentToken.type == TokenType::PLUS || currentToken.type == TokenType::MINUS) {
        // 记录运算符的位置
        std::string op = currentToken.value;
        int line = currentToken.line;
        int column = currentToken.column;
        advance(); // 消耗加减运算符
        auto right = MulExpr();
        // 创建具体子类节点并传递位置信息
        left = std::make_shared<AddExprNode>(left, op, right, line, column);
    }
    return left;
}

// 乘除模表达式解析（MulExpr → UnaryExpr | MulExpr (*|/|%) UnaryExpr）
std::shared_ptr<ExprNode> Parser::MulExpr() {
    auto left = UnaryExpr();
    while (currentToken.type == TokenType::STAR ||
           currentToken.type == TokenType::SLASH ||
           currentToken.type == TokenType::PERCENT) {
        // 记录运算符的位置
        std::string op = currentToken.value;
        int line = currentToken.line;
        int column = currentToken.column;
        advance(); // 消耗乘除模运算符
        auto right = UnaryExpr();
        // 创建具体子类节点并传递位置信息
        left = std::make_shared<MulExprNode>(left, op, right, line, column);
           }
    return left;
}
// 一元表达式解析（UnaryExpr → PrimaryExpr | (+|-|!) UnaryExpr）
std::shared_ptr<ExprNode> Parser::UnaryExpr() {
    if (currentToken.type == TokenType::PLUS ||
        currentToken.type == TokenType::MINUS ||
        currentToken.type == TokenType::NOT) {
        std::string op = currentToken.value;
        int line = currentToken.line;
        int column = currentToken.column;
        advance();
        auto expr = UnaryExpr();
        return std::make_shared<UnaryExprNode>(op, expr, line, column);
    }
    return PrimaryExpr();
}

// 基本表达式解析（PrimaryExpr → ID | NUMBER | (Expr)）|ID "(" (Expr (",” Expr)*)? ")"
std::shared_ptr<ExprNode> Parser::PrimaryExpr() {
    if (currentToken.type == TokenType::IDENTIFIER) {
        // 记录标识符的位置
        std::string value = currentToken.value;
        int line = currentToken.line;
        int column = currentToken.column;
        advance();

        // 检查是否为函数调用
        if (currentToken.type == TokenType::LPAREN) {
            return FuncCall(value, line, column);
        }

        return std::make_shared<PrimaryExprNode>(value, line, column); // 传递位置
    }
    if (currentToken.type == TokenType::NUMBER) {
        // 记录数字的位置（正负都可以）
        std::string value = currentToken.value;
        int line = currentToken.line;
        int column = currentToken.column;
        advance();
        return std::make_shared<PrimaryExprNode>(value, line, column); // 传递位置
    }
    if (currentToken.type == TokenType::LPAREN) {
        int line = currentToken.line;
        int column = currentToken.column;
        advance();
        auto expr = Expr();
        match(TokenType::RPAREN);
        return expr; // 直接返回表达式节点
    }
    throw std::runtime_error("Syntax error: invalid primary expression at line " + std::to_string(currentToken.line));
}
// 表达式语句解析
std::shared_ptr<StmtNode> Parser::ExprStmt() {
    // 表达式语句的位置为表达式的起始位置（取当前Token位置）
    int line = currentToken.line;
    int column = currentToken.column;
    auto expr = Expr();
    match(TokenType::SEMICOLON);
    return std::make_shared<ExprStmtNode>(expr, line, column); // 返回具体子类
}
// 解析入口
std::shared_ptr<CompUnitNode> Parser::parse() {
    return CompUnit();
}

