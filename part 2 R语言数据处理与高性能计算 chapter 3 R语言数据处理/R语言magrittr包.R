#3.3.2 （1）
install.packages("magrittr")
library(magrittr)

#正常代码
set.seed(1)                #设置随机种子
n1<-rnorm(10000)           #step1
n2<-abs(n1)*50             #step2
n3<-matrix(n2,ncol = 100)  #step3
n4<-round(rowMeans(n3))    #step4
hist(n4%%7)                #step5
# 上面代码的写法是，每一行实现一个条件，但中间多了不少临时变量。下面再看另外一种写法，括号包一切。
set.seed(1)
hist(round(rowMeans(matrix(abs(rnorm(10000))*50,ncol=100)))%%7)

# 以上分别用两种常⻅的代码⻛格，实现了我们的需求。下面再看看%>%的方式。
set.seed(1)
rnorm(10000) %>%
  abs %>% `*` (50)  %>%
  matrix(ncol=100)  %>%
  rowMeans %>% round %>% 
  `%%`(7) %>% hist

# 一行代码不仅搞定了所有的事情，而且结构清楚，可读性非常强。这就是管道代码的⻛格所带来的优雅和简约。


#3.3.2 （2）

# 由于输出直方图后，返回值为空，那么再继续管道就会把空值向右进行传递，这样计算最后一步时就会出错。
# 这时我们的需求是，把除以7的余数向右传递给最后一步求和，那么就可以用到%T>%了

#直接使用%>%向右传值出现异常
set.seed(1)
rnorm(10000) %>%
  abs %>% `*` (50)  %>%
  matrix(ncol=100)  %>%
  rowMeans %>% round %>% 
  `%%`(7) %>% hist %>% sum

#使用%T>%把左边的值，再向右传值，则结果正确：
set.seed(1)
rnorm(10000) %>%
  abs %>% `*` (50)  %>%
  matrix(ncol=100)  %>%
  rowMeans %>% round %>% 
  `%%`(7) %T>% hist %>% sum

#3.3.2 （3）

# 下面定义一个3列10行的data.frame，列名分别为x、y、z，为x列大于5的数据集。
# 若不使用%$%，通常写法为：
set.seed(1)
df<-data.frame(x=1:10,y=rnorm(10),z=letters[1:10])
df[which(df$x>5),]

# 使用%$%把列名x直接传到右侧进行判断。这里.代表左侧的完整数据对象。
# 一行代码就实现了需求，而且这里不需要显示的定义中间变量。
set.seed(1)
data.frame(x=1:10,y=rnorm(10),z=letters[1:10]) %$% .[which(x>5),]


#3.3.2 （4）

# 定义一个符合正态分布的100个随机数，计算绝对值，并按从小到大的顺序排序，获得并取前10个数字赋值给x
# 这里有一个陷阱，需要注意一下%<>%必须要用在第一个管道的对象处，才能完成赋值的操作，
# 如果不是左侧第一个位置，那么赋值将不起作用。
set.seed(1)
x<-rnorm(100)
x %<>% abs %>% sort %>% head(10)
x


#3.3.3 （1）
#对一个包括10个随机数的向量先乘5再加5

#使用符号的方法
set.seed(1)
rnorm(10) %>% `*`(5) %>% `+`(5) 
#使用函数的写法
set.seed(1)
rnorm(10) %>% multiply_by(5) %>% add(5) 
#上面的计算结果是完全一样的，用函数替换了符号。

#3.3.3 （2）
#对一个包括10个随机数的向量先乘5再加5，求出向量的均值和标准差，并从小到大排序后返回前5条
set.seed(1)
rnorm(10)    %>%
  multiply_by(5) %>%
  add(5)         %>%
  { 
    cat("mean:", mean(.), 
        "Var:", var(.), "\n")
    sort(.) %>% head
  }
#通过{}包装代码块，就可以很方便地完成很多处理的复杂操作。


#3.3.3 （3）

#对鸢尾花数据集进行处理，只保留第一行和最后一行
iris %>%
  (function(x) {
    if (nrow(x) > 2) 
      rbind(head(x, 1), tail(x, 1))
    else x
  })
#这里参数x就是iris数据集，作为函数的显式参数被应用于后续的数据处理过程。






