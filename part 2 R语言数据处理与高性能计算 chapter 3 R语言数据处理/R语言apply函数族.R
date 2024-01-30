#3.1.2 apply函数

x<-matrix(1:12,ncol=3) #3列
x
#对矩阵的每一行求和用apply
apply(x,1,sum)

#按行循环，让数据框的x1列加1，并计算出x1、x2列的均值

#生成data.frame
x<-cbind(x1=3,x2=c(4:1,2:5))
x
#自定义函数myFUN，第一个参数x为数据，第二第三个参数为自定义参数，可以通过apply的'…'进行传入
myFUN<- function(x, c1, c2) {
  c(sum(x[c1],1), mean(x[c2])) 
}
#对数据框按行做循环，每行分别传递给myFUN函数，设置c1、c2对应myFUN的第二第三个参数
#最后用t()函数对结果进行行列转置
t(apply(x,1,myFUN,c1='x1',c2=c('x1','x2')))

#法二：
df<-data.frame()
for(i in 1:nrow(x)){
  row<-x[i,]
  df<-rbind(df,rbind(c(sum(row[1],1), mean(row))))
}
df

#法三：直接用R语言内置向量计算
data.frame(x1=x[,1]+1,x2=rowMeans(x))

#比较一下3种操作在性能上的消耗

#清空环境变量
rm(list=ls())
#封装FUN1
fun1<-function(x){
  myFUN<- function(x, c1, c2) {
    c(sum(x[c1],1), mean(x[c2])) 
  }
  apply(x,1,myFUN,c1='x1',c2=c('x1','x2'))
}
#封装FUN2
fun2<-function(x){
  df<-data.frame()
  for(i in 1:nrow(x)){
    row<-x[i,]
    df<-rbind(df,rbind(c(sum(row[1],1), mean(row))))
  }
}
#封装FUN3
fun3<-function(x){
  data.frame(x1=x[,1]+1,x2=rowMeans(x))
}
#生成数据集
x <- cbind(x1=3, x2 = c(400:1, 2:500))
#分别统计3种方法的CPU耗时
system.time(fun1(x))
system.time(fun2(x))
system.time(fun3(x))

# 从CPU的耗时来看，用for循环实现的计算是耗时最⻓的，
# apply实现的循环耗时很短，而直接使用R语言内置向量计算的操作几乎不耗时。
# 通过上面的测试，对同一个计算来说，优先考虑R语言内置的向量计算，
# 必须要用到循环时，则使用apply函数，应该尽量避免显示的使用for、while等操作方法。


#3.1.3 lapply函数

# 构建一个list数据集x，分别包括a，b，c三个KEY值
x <- list(a = 1:10, b = rnorm(6,10,5), c = c(TRUE,FALSE,FALSE,TRUE))
x
#分别计算每个KEY对应的数据的分位数
lapply(x,fivenum)

#如上，lapply函数就可以很方便地把list数据集进行循环操作了，还可以用data.frame数据集按列进行循环，
#但如果传入的数据集是一个向量或矩阵对象，那么直接使用lapply就不能达到想要的效果了。
x <- cbind(x1=3, x2=c(2:1,4:5))
x
class(x)
lapply(x, sum) #lapply函数会分别循环矩阵中的每个值，而不是按行或按列进行分组计算。

#3.1.4 sapply函数

x <- cbind(x1=3, x2=c(2:1,4:5))
#对矩阵计算，计算过程同lapply函数
sapply(x, sum)
#检查结果类型，sapply返回类型为向量，而lapply返回类型为list
class(sapply(x,sum))
class(lapply(x,sum))
#当simplify=FALSE和USE.NAMES=FALSE时，sapply和lapply相同

#simplify为array时：
a<-1:2
#按数组分类
sapply(a,function(x) matrix(x,2,2), simplify='array')
#默认情况则自动合并分组
sapply(a,function(x) matrix(x,2,2))

#对于字符串的向量，还可以自动生成数据名
val<-head(letters)
#默认设置数据名
sapply(val,paste,USE.NAMES=TRUE)
#USE.NAMES=FALSE,则不设置数据名
sapply(val,paste,USE.NAMES=FALSE)


#3.1.5 vapply函数

#生成数据集
x<-data.frame(cbind(x1=3,x2=c(2:1,4:5)))
x
#设置行名，4行分别为a，b，c，d
vapply(x,cumsum,FUN.VALUE=c('a'=0,'b'=0,'c'=0,'d'=0))  #列累计和
#当不设置，为默认的索引值
a<-sapply(x,cumsum)
a
#手动的方式设置行名
row.names(a)<-c('a','b','c','d')
a
#通过使用vapply函数可以直接设置返回值的行名，这样子做其实可以节省一行的代码，
#让代码看起来更顺畅，当然如果不愿意多记一个函数，也可以直接忽略它，只用sapply函数就够了。


#3.1.6 mapply函数

#eg1：比较3个向量大小，按索引顺序取较大的值：
set.seed(1)
#定义3个向量
x<-1:10
y<-5:-4
z<-round(runif(10,-5,5))
x
y
z
#按索引顺序取较大的值
mapply(max,x,y,z)

#eg2：生成4个符合正态分布的数据集，分别对应的均值和方差为c(1,10,100,1000)
set.seed(1)
#长度为4
n<-rep(4,4)
#m为均值，v为方差
m<-v<-c(1,10,100,1000)
#生成4组数据，按列分组
mapply(rnorm,n,m,v)
# 由于mapply函数是可以接收多个参数的，所以我们在做数据操作的时候，就不需要把数据先
# 合并为data.frame了，直接一次操作就能计算出结果了。


#3.1.7 tapply函数

#eg：计算不同品种鸢尾花的花瓣（iris）长度的均值
#通过iris$Species品种进行分组
tapply(iris$Petal.Length,iris$Species,mean)

#对向量x和y进行计算，并以向量t为索引进行分组求和：
set.seed(1)
#定义x，y向量
x<-y<-1:10
x
y
#设置分组索引t
t<-round(runif(10,1,100)%%2)
t
#对x进行分组求和
tapply(x,t,sum)
#由于tapply只接收一个向量参数，通过“...”可以把再把其他参数传给自定义的FUN函数，
#如果我们想对向量y也进行求和，把y作为tapply的第4个参数传入，就能进行计算。
tapply(x,t,sum,y)

#得到的结果并不符合我们的预期，结果不是把x和y对应的t分组后求和，而是得到了其他的结果。
#第4个参数y传入sum时，并不是按照循环一个一个传进去的，而是每次传入完整的向量数据，
#那么再执行sum时，sum（y）=55，所以t=0时，x=8，再加上y=55，最后计算结果为63。
#那么，我们在使用“...”去传入其他的参数的时候，一定要看清楚传递过程的描述，才不会出现算法上的错误。


#3.1.8 rapply函数

#eg：对一个list的数据进行过滤，对所有数字型numeric的数据从小到大进行排序
set.seed(1)
x=list(a=12,b=4:1,c=c('b','a'))
y=pi
z=data.frame(a=rnorm(10),b=10:1)
a <- list(x=x,y=y,z=z)
a
#进行排序，并替换原list的值
rapply(a,sort, classes='numeric',how='replace')
class(a$z$b)
#从结果中不难发现，只有a$z$a的数据进行了排序，检查a$z$b的类型，发现为integer，是不等于numeric的，所以没有排序。

#对字符串类型的数据进行操作，对所有的字符串型加一个字符串“++++”，非字符串类型数据设置为NA
rapply(a, function(x) paste(x,'++++'), classes="character",deflt=NA, how="list")
class(a$x$c)
#只有a$x$c为字符串向量，合并为一个新字符串。那么，有了rapply函数就可以对list类型的数据进行方便地数据过滤了。


#3.1.9 eapply函数

#定义一个环境空间，然后对环境空间的变量进行循环处理
env<-new.env(hash=TRUE)
#向这个环境空间存入3个变量
env$a<-1:10
env$beta<-exp(-3:3)
env$logic<-c(TRUE,FALSE,FALSE,TRUE)
env
#查看env空间中的变量
ls(env)
#查看env空间中的变量字符串结构
ls.str(env)
#计算env环境空间中所有变量的均值
eapply(env,mean)
















