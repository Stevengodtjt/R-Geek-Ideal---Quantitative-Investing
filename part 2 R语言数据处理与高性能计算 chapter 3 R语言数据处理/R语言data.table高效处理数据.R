#3.2.2 （1）
install.packages("data.table")
library(data.table)
#创建一个data.frame数据框
df<-data.frame(a=c('A','B','C','A','A','B'),b=rnorm(6))
df
#对于data.table来说，创建一个数据集地语法和data.frame一样
dt<-data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt
#检查df、dt两个对象的类型，可以看到data.table是对data.frame地扩展类型
class(df) 
class(dt)


#3.2.2 （2）

#data.frame转为data.table
df<-data.frame(a=c('A','B','C','A','A','B'),b=rnorm(6))
#检查类型
class(df)
#转型为data.table对象
df2<-data.table(df)
#检查类型
class(df2)

#data.table转为data.frame
dt <- data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
#检查类型
class(dt)
#转型为data.frame
dt2<-data.frame(dt)
class(dt2)


#3.2.2 （3）

#data.table基本的数据查询方法
dt <- data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt
#按行或按列查询：
dt[2,]             #取第二行数据
dt[2]              #不加也可以
dt$a               #取a列的值
dt[a=='B',]        #取a列中值为B的行
dt[,a=='B']        #取a列中值为B的行的判断
which(dt[,a=='B']) #取a列中值为B的行的索引

#上面的操作，不管是用索引值，==和$都是data.frame操作一样的。下面我们取data.table特殊设计的keys来查询：
setkey(dt,a) #设置a列为索引列
dt           #打印dt对象，发现数据已经按照a列字母对应ASCII码值进行了排序

#按照自定义的索引进行查询
dt["B",]             #取a列中值为B的行
dt["B",mult="first"] #取a列中值为B的行，并保留第一行
dt["B",mult="last"]  #取a列中值为B的行，并保留最后一行
dt["b"]              #取a列中值为b的行，没有数据则为NA

#从上面的代码测试中我们可以看出，在定义了keys后，我们在查询的时候就不用再指定列了，
#方括号中的第一位置默认会留给keys，作为索引匹配的查询条件。从代码的角度，又节省了一个变量定义的代码。
#同时，可以用mult参数，对数据集增加过滤条件，让代码本身也变得更高效。如果查询的值不是索引列包括的值，则返回NA。


#3.2.2 （4）
dt <- data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt
#增加1列，列名为c(b列的值加2)
dt[,c:=b+2]
dt
#增加2列，列名为c1，c2
dt[,':='(c1=1:6,c2=2:7)]
dt
#增加2列，另一种方法
dt[,c('d1','d2'):=list(1:6,2:7)]
dt

#删除c1列
dt[,c1:=NULL]
dt
#同时删除d1，d2列
dt[,c('d1','d2'):=NULL]
dt

#修改：给b赋值为30
dt[,b:=30]
dt
#修改：对a列值为B的行，c2列值大于3的行，b列赋值为100
dt[a=='B'&c2>3,b:=100]
dt


#3.2.2 （5）
dt<-data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt
#对整个b列数据求和
dt[,sum(b)]
#按a列分组，并对b列按分组求和
dt[,sum(b),by=a]


#3.2.2 （6）

#6名学生分别参加A和B的两门考试，每门考试得分不一样
student<-data.table(id=1:6,name=c('Dan','Mike','Ann','Yang','Li','Kate'))
student
#分别参加A和B两门考试
score<-data.table(id=1:12,stuId=rep(1:6,2),score=runif(12,60,99),class=c(rep('A',6),rep('B',6)))
score
#通过学生ID，把学生和考试成绩2个数据集进行连接
setkey(score,"stuId")    #设置score数据集，key为stuId
setkey(student,"id")     #设置student数据集，key为id
#合并两个数据集的数据
student[score,nomatch=NA,mult="all"] 

#最后我们会看到，两个数据集的结果合并在了一个结果数据集中。这样就完成了数据连接的操作。
#从代码的角度来看，1行代码要比用data.frame去拼接方便得多。

























