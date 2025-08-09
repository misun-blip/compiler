cmake_minimum_required(VERSION 3.28.3)
project(Toyc_Complier)

# 设置 C++ 标准
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# 自动查找源文件和头文件
file(GLOB_RECURSE SOURCES src/*.cpp)
file(GLOB_RECURSE HEADERS include/*.h)

# 添加可执行文件
add_executable(Toyc_Complier main.cpp ${SOURCES} ${HEADERS})

# 添加头文件搜索路径
target_include_directories(Toyc_Complier PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)