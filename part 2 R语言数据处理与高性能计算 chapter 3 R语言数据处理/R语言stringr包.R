install.packages("stringr")
library(stringr)

#3.4.2 part1:字符串拼接

#（1）str_c
#sep：把多个字符串拼接成一个大的字符串，用于字符串的分隔符
str_c('a','b')
str_c('a','b',sep='-')
str_c(c('a','a1'),c('b','b1'),sep='-')
#collapse：把多个向量参数拼接为一个大的字符串，用于字符串的分隔符
str_c(head(letters),collapse = "")
str_c(head(letters),collapse = ", ")
str_c('a','b',collapse = "-")  #collapse参数，对多个字符串无效
str_c(c('a','a1'),c('b','b1'),collapse = '-')
#拼接有NA值的字符串向量时，NA还是NA
str_c(c("a",NA,"b"),"-d")

#对比str_c函数和paste函数之间的不同点：

#多字符串拼接，默认的sep参数行为不一致：
str_c('a','b')
paste('a','b')
#向量拼接字符串，collapse参数的行为不一致：
str_c(head(letters),collapse="")
paste(head(letters),collapse="")
#拼接有NA值的字符串向量，对NA的处理行为不一致：
str_c(c("a",NA,"b"),"-d")
paste(c("a",NA,"b"),"-d")


#（2）str_trim

#只过滤左边的空格
str_trim("  left space    ",side='left')
#只过滤右边空格
str_trim("  left space    ",side='right')
#过滤两边空格
str_trim("  left space    ",side='both')

#（3）str_pad

#从左边补充空格，直到字符串长度为20
str_pad("conan",20,"left")
#从右边补充空格，直到字符串长度为20
str_pad("conan",20,"right")
#从左右两边各补充x字符，直到字符串长度为20
str_pad("conan",20,"both","x")

#（4）str_dup

val<-c("abca4",123,"cba2")
#复制2次
str_dup(val,2)
#按位置复制
str_dup(val,1:3)

#（5）str_wrap

txt<-'R语言作为统计学一门语言，一直在小众领域闪耀着光芒。直到大数据的爆发，
R语言变成了一门炙手可热的数据分析的利器。随着越来越多的工程背景的人的加入，
R语言的社区在迅速扩大成长。现在已不仅仅是统计领域，教育，银行，电商，互联网….都在使用R语言。'

#设置宽度为40个字符
cat(str_wrap(txt,width=40))
#设置宽度为60字符，首行缩进2字符
cat(str_wrap(txt,width=60,indent=2))
#设置宽度为10字符，非首行缩进4字符
cat(str_wrap(txt,width=10,exdent=4))

#（6）str_sub

txt<-"I am Conan."
#截取1-4索引位置的字符串
str_sub(txt,1,4)
#截取1-6索引位置的字符串
str_sub(txt,end=6)  #str_sub(txt,1,6)
#截取6到结束的索引位置的字符串
str_sub(txt,6)
#分2段截取字符串
str_sub(txt,c(1,4),c(6,8))
#通过负坐标截取字符串
str_sub(txt,-3)       #截取倒数3个
str_sub(txt,end=-3)   #截取至倒数第3个（包括倒数第三个）

#对截取的字符串进行赋值
x<-"AAABBBCCC"
#在字符串的1的位置赋值1
str_sub(x,1,1)<-1
x
#在字符串2到4的位置赋值234
str_sub(x,2,4)<-"234"
x


#3.4.2 part2:字符串计算

#（1）str_count

#对字符串中匹配的字符计数
str_count('aaa444sssddd',"a")
#对字符串向量中匹配的字符计数
fruit<-c("apple","banana","pear","pineapple")
str_count(fruit,"a")
str_count(fruit,"p")
#对字符串中的'.'字符计数，由于'.'是正则表达式的匹配符，直接判断计数结果是不对的
str_count(c("a.", ".",".a.",NA),".")
#用fixed匹配字符
str_count(c("a.",".",".a.",NA),fixed("."))

#（2）str_length

#计算字符串的长度
str_length(c("I","am","哔哩哔哩",NA))

#（3）str_sort

#对字符串值进行排序：
str_sort(c('a',1,2,'11'),locale="en")  #按ASCII字母排序
str_sort(letters,decreasing = TRUE)    #倒序排序
str_sort(c('你','好','粉','丝','日','志'),locale="zh") #按拼音排序

#对NA值进行排序
str_sort(c(NA,'1',NA),na_last=TRUE)    #把NA放在最后面
str_sort(c(NA,'1',NA),na_last=FALSE)   #把NA放在最前面
str_sort(c(NA,'1',NA),na_last=NA)      #去掉NA



#3.4.2 part3:字符串匹配

#（1）str_split
val<-"abc,123,234,iuuu"
#以,进行分割
s1<-str_split(val,",")
s1
#以,进行分割，保留2块
s2<-str_split(val,",",2)
s2
#查看str_split()函数操作的结果类型list
class(s1)
#用str_split_fixed()函数进行分割，结果是matrix
s3<-str_split_fixed(val,",",2)
s3
class(s3)

#（2）str_subset
val<-c("abc","123","cba")
#全文匹配
str_subset(val,"a")
#开头匹配
str_subset(val,"^a")
#结尾匹配
str_subset(val,"a$")

#（3）word
val<-c("I am Conan.","http://fens.me, ok")
#默认以空格分割，取第一个位置的字符串
word(val,1)
word(val,2)


#以,分割，取第一个位置的字符串
val<-'111,222,333,444'
word(val,1,sep=fixed(','))
word(val,3,sep=fixed(','))


#（4）str_detect
val<-c("abca4",123,"cba2")
#检查字符串向量，是否包括a
str_detect(val,"a")
#检查字符串向量，是否以a开头
str_detect(val,"^a")
#检查字符串向量，是否以a结尾
str_detect(val,"a$")

#（5）str_match

#从字符串中提取匹配组
val<-c("abc",123,"cba")
#匹配字符a，并返回对应的字符
str_match(val,"a")
#匹配字符0-9，限1个，并返回对应的字符
str_match(val,"[0-9]")
#匹配字符0-9，不限数量，并返回对应的字符
str_match(val,"[0-9]*")

#从字符串提取匹配组，以字符串matrix形式返回
str_match_all(val,"a")
str_match_all(val,"[0-9]")

#（6）str_replace
val<-c("abc",123,"cba")
#把目标字符串第一个出现的a或b替换为-
str_replace(val,"[ab]","-")
#把目标字符串所有出现的a或b替换为-
str_replace_all(val,"[ab]","-")

#（7）str_replace_na

#把NA替换为字符串
str_replace_na(c(NA,'NA',"abc"),'x')
str_replace_na(c(NA,NA,"abc"),'x')

#（8）str_locate
val<-c("abca",123,"cba")
#匹配a在字符串中的位置
str_locate(val,"a")
#用向量匹配
str_locate(val,c("a",12,"b"))
#以字符串matrix形式返回
str_locate_all(val,"a")
#匹配a或b字符串，以字符串matrix形式返回
str_locate_all(val,"[ab]")

#（9）str_extract
val<-c("abca4",123,"cba2")
#返回匹配的数字
str_extract(val,"\\d")
#返回匹配的字符
str_extract(val,"[a-z]+")



#3.4.2 part4:字符串变换

#（1）str_conv

#把中文字符字节化
x<-charToRaw('你好')
x
#默认win系统字符集为GBK，GB2312为GBK子集，转码正常
str_conv(x,"GBK")
str_conv(x,"GB2312")
#转UTF-8失败
str_conv(x,"UTF-8")
#将unicode转为UTF-8
x1<-"\u5317\u4eac"
str_conv(x1,"UTF-8")

#（2）str_to_upper

#字符串大写转换
val<-"I am conan.Welcome to my blog."
#全大写
str_to_upper(val)
#全小写
str_to_lower(val)
#首字母大写
str_to_title(val)



































