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

//����Rcpp���ֵĴ��룬���ǿ��Է�������������
//1.#include��using���֣������ú������ռ��������
//<Rcpp.h>��namespace��C++���������صģ�����ʹ����string������Ϊ�����ͷ���ֵ��
//������Ҫ�������<string>�������������ռ�namespace std��

//2.���ܺ������֣�������һ��hello��string name����������һ��������string���ͣ�
//����ֵҲΪstring���͡���Ҫǿ�����ǣ���R���ŵĺ�����������//[[Rcpp����export]]��ע��������

//3.����ִ�У���/***R��*/������ΪR���ԵĴ��룬��Ĭ�ϱ�ִ�С�
