#include <Rcpp.h>
#include <string>
using namespace std;
using namespace Rcpp;
//[[Rcpp::export]]
string hello(string name){
	cout<<"hello"<<name<<endl;
	return name;
} 
/*** R
hello('world')
hello('Conan')
*/

//上面Rcpp部分的代码，我们可以分三部分来看。
//1.#include和using部分：包引用和命名空间的声明。
//<Rcpp.h>和namespace是C++程序必需加载的，由于使用了string类型作为参数和返回值，
//所以需要引入类库<string>，并声明命名空间namespace std。

//2.功能函数部分：定义了一个hello（string name）函数，有一个参数是string类型，
//返回值也为string类型。需要强调的是，对R开放的函数必须增加//[[Rcpp：：export]]的注释声明。

//3.代码执行：由/***R和*/包含，为R语言的代码，会默认被执行。
