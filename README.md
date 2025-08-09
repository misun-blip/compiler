# 编译系统实践任务

ToyCCompiler/

├── src/                    		# 源文件目录

│   ├── Lexer.cpp              # 词法分析器类实现

│   ├── Parser.cpp             # 语法分析器类实现

│   ├── SemanticAnalyzer.cpp      # 语义分析类实现

│   ├── Codegen.cpp             # 汇编代码生成类实现

│   

├── include/                # 头文件目录

│   ├── Lexer.h              # 词法分析器类声明

│   ├── Parser.h             # 语法分析器类声明

│   ├── SemanticAnalyzer.h    # 语义分析器类声明

│   ├── CodeGenerator.h             # 汇编代码生成类声明

│

├── test/                   # 测试目录

│   ├── testlex.txt      # 词法分析器测试代码

│   ├── testparse.txt     # 语法分析器测试代码

│

├── main.cpp          # 集成测试

├── CMakeLists.txt          # CMake构建文件

└── README.md               # 项目说明文档

### 主要模块功能说明

1. 词法分析器(lexer)
   - lexer.h: 定义Token类型枚举（如INT, PLUS, IDENTIFIER等）、字符串与Token互相转换的函数和词法分析器类
   - lexer.cpp: 实现分析源代码并转化为Token流，同时处理注释和空白，并实现string与token转换的函数
2. 语法分析器(parser)
   - parser.h: 定义各种AST结点、语法分析器类
   - parser.cpp: 实现AST结点的成员函数（toString()），实现语法分析器的各种语句和块的分析
3. 语义分析器（SemanticAnalyzer）
   - SemanticAnalyzer.h:定义符号表条目结构，定义语义分析器类和错误分析类
   - SemanticAnalyzer.h：实现语义分析类的各种成员函数（检查变量和类型匹配等）
4. 汇编代码生器（CodeGenerator）
   - CodeGenerator.h:定义寄存器分配器类、符号表类和代码生成器类
   - Codegen.cpp：实现各种语句和块结点的代码生成
5. 测试目录（test）
   - testlex: 示例的toyc代码，用于生成token流
   - testparser：示例的toyc代码，用于调试语法分析过程
6. 主程序(main.cpp)
   - 集成各个模块，实现编译流程：
     toyc代码 → 词法分析 → 语法分析 → 语义分析 → 优化 → 代码生成