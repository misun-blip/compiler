//
// Created by kaixuan on 2025/7/1.
//

#ifndef LEX_H
#define LEX_H

#include<iostream>
#include<string>

// 定义词法单元类型的枚举
enum class TokenType {
    // 关键字
    INT, VOID, IF, ELSE, WHILE, BREAK, CONTINUE, RETURN,COMMA,
    // 标识符
    IDENTIFIER,
    // 字面量
    NUMBER,
    // 运算符
    PLUS, MINUS, STAR, SLASH, PERCENT,
    ASSIGN, EQ, NEQ, LT, GT, LTE, GTE,
    LOGICAL_AND, LOGICAL_OR, NOT,
    // 界符
    SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE,COMMENT_START,COMMENT_END,COMMENT_LINE,
    // 错误标识
    ERROR,
    // 文件结束标识
    EOF_TOKEN
};

std::string tokenTypeToString(TokenType type);

TokenType stringToTokenType(const std::string& typeStr);

//词法单元类
class Token {
public:
    TokenType type;
    std::string value;
    int line;
    int column;
    Token(TokenType t, std::string v,int l,int c) : type(t), value(std::move(v)),line(l), column(c) {}
};

// 词法分析器类
class Lexer {
private:
    std::string input;  // 输入的ToyC代码
    size_t pos = 0;     // 当前读取位置
    int line = 1;
    int column = 1;

    [[nodiscard]] char peek() const;
    [[nodiscard]] char peek2() const;
    char consume();
    void skipWhitespace();
    void skipComment();
    Token identifierOrKeyword();
    Token numberLiteral();
    Token operatorOrDelimiter();

    void updatePosition(char c);
public:
    explicit Lexer(std::string inputCode);
    Token nextToken();
};
#endif //LEX_H
