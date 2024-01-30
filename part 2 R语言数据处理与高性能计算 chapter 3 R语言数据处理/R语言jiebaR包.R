install.packages("jiebaR")
library(jiebaR)

#3.5.2 jiebaR上手

#案例1：对一段文字进行分词
wk=worker()
wk["我是《R的极客理想》的学习者"]
wk["我是R语言的深度用户"]

# jiebaR提供了3种分词语句的写法，上面例子中用的是[]符号的语法，
# 还可以使用<=符合语法，或者使用segment（）函数。虽然形式不同，但是分词效果是一样的。

#使用<=符号的语法如下：
wk<='另一种符合的语法'
#使用segment()函数的语法如下：
segment("segment()函数语句的写法",wk)
#我们也可以直接对文本文件进行分词，在当前目录新建一个文本文件idea.txt，运行后发现新文本以空格分词
wk['D:/significant_data/R/b站分享/R语言量化投资/part 2 R语言数据处理与高性能计算 chapter 3 R语言数据处理/idea.txt']


#3.5.3 分词引擎

#3.5.4 配置词典

#查看默认的词库位置
show_dictpath()
#查看目录
dir(show_dictpath())

# 词典目录中包括了多个文件。
# user.dict.utf8：用户词典文件，utf8编码。
# stop_words.utf8：停止词文件，utf8编码。
# jieba.dict.zip：jieba.dict.utf8的压缩包。
# hmm_model.zip：hmm_model.utf8的压缩包。
# idf.zip：idf.utf8的压缩包。
# backup.rda：无注释。
# model.rda：无注释。
# README.md：用户帮助说明文件。

#打开系统词典文件user.dict.utf8，并打印前50行。
scan(file="C:/Users/17949/Documents/R/win-library/4.1/jiebaRD/dict/user.dict.utf8",
     what=character(),nlines=50,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')
#用户词典第一行有二列，第一列为词项，第二列为词性标记，没有词频的列。用户词典的默认词频为系统词库中的最大词频。

#使用自定义词典对idea.txt再进行分词
work_new_word<-worker()
new_user_word(work_new_word, c("R语言","R的极客理想","大数据","数据"))
segment("D:/significant_data/R/b站分享/R语言量化投资/part 2 R语言数据处理与高性能计算 chapter 3 R语言数据处理/idea.txt",work_new_word)


#3.5.5 停止词过滤

#配置stop_word文件的方法
wk=worker(stop_word = "stop_word.txt") #notepad++
#加载分词引擎，并配置停止词过滤
segment<-wk["我是《R的极客理想》图书学习者"]
segment
#上面的文本，我们对“我是”通过停止词进行了过滤。
#如果还想过滤“学习者”一词，可以动态地调用filter_segment（）函数。
filter<-c("学习者")
filter_segment(segment,filter)

#3.5.6 关键词提取

#案例
wk=worker()
segment<-wk["R的极客理想系列文章，涵盖了R的思想，使用，工具，创新等的一系列要点，以我个人的学习和体验去诠释R的强大。"]
#计算词频
freq(segment)
#取TF-IDF前5的关键词
keys=worker("keywords",topn = 5)
#计算关键词
vector_keywords(segment,keys)

# 使用jiebaR包处理分词确实简单，几行的代码就能实现分词的各种算法操作。
# 有了这个工具，我们就可以在文档中发现各种语言规则并进行文本挖掘了

