#include "Lexer.h"
#include <cctype>

// Token类实现
//Token::Token(TokenType t, std::string v) : type(t), value(std::move(v)) {}

// Lexer类实现
Lexer::Lexer(std::string inputCode) : input(std::move(inputCode)) {}

void Lexer::updatePosition(const char c) {
    if (c == '\n') {
        line++;
        column = 1;
    } else {
        column++;
    }
}

char Lexer::peek() const {      //查找字符
    if (pos < input.size()) {
        return input[pos];
    }
    return '\0';
}
char Lexer::peek2() const {      //查找字符
    if (pos < input.size()) {
        return input[pos+1];
    }
    return '\0';
}

char Lexer::consume() {         //吃掉字符
    return input[pos++];
}

void Lexer::skipWhitespace() {  //跳过空白格
    while (std::isspace(peek())) {
        consume();
    }
}

void Lexer::skipComment() {
    // 支持多组连续注释
    while (true) {
        if (peek() == '/' && peek2() == '/') {
            // 单行注释，跳到行尾
            consume(); consume(); // 跳过//
            while (peek() != '\n' && peek() != '\0') {
                const char c = consume();
                updatePosition(c);
            }
            if (peek() == '\n') {
                updatePosition(consume());
            }
        } else if (peek() == '/' && peek2() == '*') {
            // 多行注释，跳到最近的*/
            consume(); consume(); // 跳过/*
            while (true) {
                if (peek() == '\0') break;
                if (peek() == '*' && peek2() == '/') {
                    consume(); consume();
                    break;
                }
                const char c = consume();
                updatePosition(c);
            }
        } else {
            break;
        }
        // 跳过注释后可能还有连续注释，继续循环
        skipWhitespace();
    }
}

Token Lexer::identifierOrKeyword() {
    std::string result;
    while (std::isalnum(peek()) || peek() == '_') { // 字母或下划线
        result += consume();
        updatePosition(result.back());
    }
    // 检查是否为关键字
    if (result == "int") return {TokenType::INT, result, line, column};
    if (result == "void") return {TokenType::VOID, result, line, column};
    if (result == "if") return {TokenType::IF, result, line, column};
    if (result == "else") return {TokenType::ELSE, result, line, column};
    if (result == "while") return {TokenType::WHILE, result, line, column};
    if (result == "break") return {TokenType::BREAK, result, line, column};
    if (result == "continue") return {TokenType::CONTINUE, result, line, column};
    if (result == "return") return {TokenType::RETURN, result, line, column};
    return {TokenType::IDENTIFIER, result, line, column};     //不是关键字，返回标识符
}


Token Lexer::numberLiteral() {
    std::string result;
    if (!std::isdigit(peek())) {
        // 如果不是数字，直接返回错误token，防止死循环
        return {TokenType::ERROR, "", line, column};
    }
    while (std::isdigit(peek())) {
        result += consume();
        updatePosition(result.back());
    }
    return {TokenType::NUMBER, result, line, column};
}

Token Lexer::operatorOrDelimiter() {
    const char c = consume();
    updatePosition(c);
    std::string  result;
    switch (c) {
        case '+': return {TokenType::PLUS, "+", line, column};
        case '-': return {TokenType::MINUS, "-", line, column};
        case '*': return {TokenType::STAR, "*", line, column};
        case '/': return {TokenType::SLASH, "/", line, column};
        case '%': return {TokenType::PERCENT, "%", line, column};
        case '=':
            if (peek() == '=') {
                result += consume();
                updatePosition(result.back());
                return {TokenType::EQ, "==", line, column};
            }
            return {TokenType::ASSIGN, "=", line, column};
        case '!':
            if (peek() == '=') {
                result += consume();
                updatePosition(result.back());
                return {TokenType::NEQ, "!=", line, column};
            }
            return {TokenType::NOT, "!", line, column};
        case '<':
            if (peek() == '=') {
                result += consume();
                updatePosition(result.back());
                return {TokenType::LTE, "<=", line, column};
            }
            return {TokenType::LT, "<", line, column};
        case '>':
            if (peek() == '=') {
                result += consume();
                updatePosition(result.back());
                return {TokenType::GTE, ">=", line, column};
            }
            return {TokenType::GT, ">", line, column};
        case ',': return {TokenType::COMMA, ",", line, column};
        case ';': return {TokenType::SEMICOLON, ";", line, column};
        case '(': return {TokenType::LPAREN, "(", line, column};
        case ')': return {TokenType::RPAREN, ")", line, column};
        case '{': return {TokenType::LBRACE, "{", line, column};
        case '}': return {TokenType::RBRACE, "}", line, column};
        case '&':
            if (peek() == '&') {
                result += consume();
                updatePosition(result.back());
                return {TokenType::LOGICAL_AND, "&&", line, column};
            }
            return {TokenType::ERROR, "&", line, column};
        case '|':
            if (peek() == '|') {
                result += consume();
                updatePosition(result.back());
                return {TokenType::LOGICAL_OR, "||", line, column};
            }
            return {TokenType::ERROR, "|", line, column};
        default:
            break;
    }
    return {TokenType::ERROR, std::string(1, c), line, column};
}


Token Lexer::nextToken() {
    int loopCount = 0;
    while (true) {
        loopCount++;
        if (loopCount > 1000) { // 防止无限循环
            return {TokenType::ERROR, "Infinite loop detected", line, column};
        }

        skipWhitespace();
        if (pos >= input.size()) {
            return {TokenType::EOF_TOKEN, "", line, column};
        }

        const char current = peek();
        const int startLine = line;
        const int startCol = column;

        if (current == '/') {
            // 检查是否为注释
            if (pos + 1 < input.size() && (input[pos + 1] == '/' || input[pos + 1] == '*')) {
                skipComment();
                continue; // 跳过注释后继续循环
            }
            {
                // 不是注释，直接处理为运算符
                auto token = operatorOrDelimiter();
                token.line = startLine;
                token.column = startCol;
                return token;
            }
        }

        if ( std::isalpha(current) || current == '_') {
            auto token = identifierOrKeyword();
            token.line = startLine;
            token.column = startCol;
            return token;
        }
        if (std::isdigit(current)) {
            auto token = numberLiteral();
            token.line = startLine;
            token.column = startCol;
            return token;
        }
        {
            // 处理运算符和分隔符
            auto token = operatorOrDelimiter();
            token.line = startLine;
            token.column = startCol;
            return token;
        }
    }
}

// 辅助函数：将TokenType转换为字符串（需在Lexer.h中声明或内联）
std::string tokenTypeToString(const TokenType type) {
    switch (type) {
        case TokenType::INT: return "INT";
        case TokenType::VOID: return "VOID";
        case TokenType::IF: return "IF";
        case TokenType::ELSE: return "ELSE";
        case TokenType::WHILE: return "WHILE";
        case TokenType::BREAK: return "BREAK";
        case TokenType::CONTINUE: return "CONTINUE";
        case TokenType::RETURN: return "RETURN";
        case TokenType::IDENTIFIER: return "IDENTIFIER";
        case TokenType::NUMBER: return "NUMBER";
        case TokenType::PLUS: return "PLUS";
        case TokenType::MINUS: return "MINUS";
        case TokenType::STAR: return "STAR";
        case TokenType::SLASH: return "SLASH";
        case TokenType::PERCENT: return "PERCENT";
        case TokenType::ASSIGN: return "ASSIGN";
        case TokenType::EQ: return "EQ";
        case TokenType::NEQ: return "NEQ";
        case TokenType::LT: return "LT";
        case TokenType::GT: return "GT";
        case TokenType::LTE: return "LTE";
        case TokenType::GTE: return "GTE";
        case TokenType::LOGICAL_AND: return "LOGICAL_AND";
        case TokenType::LOGICAL_OR: return "LOGICAL_OR";
        case TokenType::NOT: return "NOT";
        case TokenType::SEMICOLON: return "SEMICOLON";
        case TokenType::LPAREN: return "LPAREN";
        case TokenType::RPAREN: return "RPAREN";
        case TokenType::LBRACE: return "LBRACE";
        case TokenType::RBRACE: return "RBRACE";
        case TokenType::COMMA: return "COMMA";
        case TokenType::ERROR: return "ERROR";
        case TokenType::EOF_TOKEN: return "EOF_TOKEN";
        default: return "UNKNOWN";
    }
}
// 将字符串转换为TokenType的函数
TokenType stringToTokenType(const std::string& typeStr) {
    if (typeStr == "INT") return TokenType::INT;
    if (typeStr == "VOID") return TokenType::VOID;
    if (typeStr == "IDENTIFIER") return TokenType::IDENTIFIER;
    if (typeStr == "NUMBER") return TokenType::NUMBER;
    if (typeStr == "LPAREN") return TokenType::LPAREN;
    if (typeStr == "RPAREN") return TokenType::RPAREN;
    if (typeStr == "LBRACE") return TokenType::LBRACE;
    if (typeStr == "RBRACE") return TokenType::RBRACE;
    if (typeStr == "ASSIGN") return TokenType::ASSIGN;
    if (typeStr == "SEMICOLON") return TokenType::SEMICOLON;
    if (typeStr == "WHILE") return TokenType::WHILE;
    if (typeStr == "GT") return TokenType::GT;
    if (typeStr == "MINUS") return TokenType::MINUS;
    if (typeStr == "IF") return TokenType::IF;
    if (typeStr == "EQ") return TokenType::EQ;
    if (typeStr == "BREAK") return TokenType::BREAK;
    if (typeStr == "CONTINUE") return TokenType::CONTINUE;
    if (typeStr == "ELSE") return TokenType::ELSE;
    if (typeStr == "PLUS") return TokenType::PLUS;
    if (typeStr == "RETURN") return TokenType::RETURN;
    if (typeStr == "LOGICAL_AND") return TokenType::LOGICAL_AND;
    if (typeStr == "LOGICAL_OR") return TokenType::LOGICAL_OR;
    if (typeStr == "NOT") return TokenType::NOT;
    if (typeStr == "NEQ") return TokenType::NEQ;
    if (typeStr == "LT") return TokenType::LT;
    if (typeStr == "GTE") return TokenType::GTE;
    if (typeStr == "LTE") return TokenType::LTE;
    if (typeStr == "STAR") return TokenType::STAR;
    if (typeStr == "SLASH") return TokenType::SLASH;
    if (typeStr == "PERCENT") return TokenType::PERCENT;
    if (typeStr == "COMMA") return TokenType::COMMA;
    if (typeStr == "ERROR") return TokenType::ERROR;
    if (typeStr == "EOF_TOKEN") return TokenType::EOF_TOKEN;

    return TokenType::ERROR; // 默认返回ERROR类型
}