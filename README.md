# MiniCC

MiniCC是一个基于LLVM的编译器前端，实现了C语言的部分核心特性，包含double数据类型、基本运算符、if-else和while控制流、函数定义。MiniCC的主要模块包括如下：
- lexer: 词法分析器，将源代码解析成token流
- parser: 语法分析器， 使用递归下降的方法解析EBNF表示，根据token流构建一颗抽象语法树（AST）
- codegen：自顶向下遍历ADT，为每个AST节点及其依赖的内容生成LLVM IR

# 使用方式

编译`miniCC.cpp`生成编译器：

```
clang++ -g -O3 miniCC.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -o miniCC
```

运行简单的测试：

```
./miniCC sum.cpp -o sum.o
./miniCC sub.cpp -o sub.o
clang++ main.cpp sum.o sub.o -o main
./main
```

程序输出：

```
sum of 3.0 and 4.0: 7
sub of 3.0 and 4.0: -1
```

# Tips

在将各种类型AST节点转换成LLVM IR时，可以编写简单的C++源文件，使用clang命令生成可阅读风格的LLVM IR文件，即`.ll`文件，观察clang是如何生成LLVM IR的。例如：

```
clang -S -emit-llvm ir.cpp -o ir.ll
```

# Reference

- [My First Language Frontend with LLVM Tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)
- [手把手教你构建 C 语言编译器](https://lotabout.me/2015/write-a-C-interpreter-0/)
- [700行手写编译器](https://www.bilibili.com/video/BV1Kf4y1V783/?spm_id_from=333.337.search-card.all.click&vd_source=7dfb2f22bf18e191ca08edc109d9a452)
