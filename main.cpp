#include "Lexer.h"
#include "Parser.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdio>

#include "CodeGenerator.h"
#include "SemanticAnalyzer.h"

int main() {
    try {
         /*//从文件读取源代码
        std::string inputFilename = "../test/19_many_arguments.tc";
        std::ifstream File(inputFilename);
        if (!File.is_open()) {
            std::cerr << "error: file " << inputFilename << " not found or not accessible" << std::endl;
            return 1;
        }
        std::string inputCode;
        std::string line;
        while (std::getline(File, line)) {
            inputCode += line + '\n';
        }*/

        // 从标准输入读取源代码
        std::string inputCode;
        std::string line;
        while (std::getline(std::cin, line)) {
            inputCode += line + '\n';
        }

        // 词法分析
        Lexer lexer(inputCode);
        std::vector<Token> tokens;
        Token token = lexer.nextToken();
        while (token.type != TokenType::EOF_TOKEN) {
            tokens.push_back(token);
            token = lexer.nextToken();
        }
        tokens.push_back(token); // 添加EOF_TOKEN

        // 检查token流是否为空
        if (tokens.empty()) {
            std::cerr << "错误：词法分析结果为空" << std::endl;
            return 1;
        }
     //  std::cout<<"词法分析完成"<<std::endl;
        // 语法分析
        Parser parser(tokens);
        auto compUnit = parser.parse();

        // 检查AST是否为空
        if (!compUnit) {
            std::cerr << "错误：语法分析结果为空" << std::endl;
            return 1;
        }
       // std::cout<<"语法分析完成"<<std::endl;
        // 语义分析
        SemanticAnalyzer analyzer;
        analyzer.analyze(compUnit);

     //  std::cout<<"语义分析完成"<<std::endl;

        //代码生成并输出到标准输出（已重定向到文件）
        CodeGenerator codegen(std::cout);
          /*std::ofstream output("../result/compilation_result.txt");
        if (!output.is_open()) {
          throw std::runtime_error("无法打开输出文件");
         }
         CodeGenerator codegen(output);*/
        codegen.generate(compUnit);

    } catch (const SemanticError& e) {
        std::cerr << "语义错误: " << e.what() << std::endl;
        return 1;
    } catch (const std::runtime_error& e) {
        std::cerr << "编译错误: " << e.what() << std::endl;
        return 1;
    } catch (const std::exception& e) {
        std::cerr << "异常错误: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "发生未知错误" << std::endl;
        return 1;
    }

    return 0;
}