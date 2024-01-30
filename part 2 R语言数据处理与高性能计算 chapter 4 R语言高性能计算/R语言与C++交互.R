#下载Rcpp程序包
install.packages("Rcpp")

#1.从hello world开始

# 我们需要新建两个文件，并放在同一个目录中。
# ·demo.cpp，C++程序的源文件
# ·demo.r，R程序源文件（本文件）

#加载Rcpp包
library(Rcpp)
#编译和加载demo.cpp文件
sourceCpp(file='D:/significant_data/R/b站分享/R语言量化投资/part 2 R语言数据处理与高性能计算 chapter 4 R语言高性能计算/demo.cpp')
#执行封装在demo.cpp中的R代码
hello('world')
hello('Conan')
#执行hello函数
hello('R')

#2.R和Rcpp的混写代码

# sourceCpp（）函数还提供了一种代码混写的方法，就是在R的代码中直接嵌入C++代码。
sourceCpp(code='
  #include <Rcpp.h>
  #include <string>  
  
  using namespace std;
  using namespace Rcpp;
  
  //[[Rcpp::export]]
  string hello(string name) {
    cout << "hello "<<name << endl;  
    return name;
  }
')
hello('R2')
# 这种多语言混写的语法虽然不太推荐，但对于几行代码来说，还是很方便的。






