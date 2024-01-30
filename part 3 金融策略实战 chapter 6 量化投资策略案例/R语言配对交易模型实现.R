#6.3.2 配对交易模型

# 根据概念，我们生成两个虚拟的金融产品X、Y，包括时间和价格字段。
# 让X和Y的两个产品都价格符合正态分布，生成100个日期的数据。
# 由于是测试程序，日期字段包括了自然日，暂时理解为连续的日期。

set.seed(1)                         #设置随机种子
dates<-as.Date('2010-01-01')+1:100  #100个日期
x<-round(rnorm(100,50,40),2)        #随机生成X产品，100个正态分析的收盘价
y<-round(rnorm(100,50,40),2)        #随机生成Y产品，100个正态分析的收盘价
df<-data.frame(dates,x,y)
head(df,20)

#把数据进行可视化
install.packages("ggplot2")
install.packages("scales")
install.packages("reshape2")
library(ggplot2)
library(scales)
library(reshape2)

#数据转型
df2<-melt(df,c('dates'))

#画图
g<-ggplot(data=df2,aes(x=dates,y=value,colour=variable))
g<-g+geom_line()
g<-g+scale_x_date(date_breaks = "1 week",date_labels='%m-%d')
g<-g+labs(x='date',y='Price')
g
# 图中，X轴为时间，Y轴是价格，红色线为X的产品的价格，蓝色线为Y产品的价格。
# 我们可以直观的看出，X、Y两个产品无任何关系


# 根据配对交易的假设条件，如果两个金融产品的价差是收敛的，接下来验证价差的收敛性。
# 我们用X的产品价格减去Y产品的价格，当差值为正的时候，我们认为X的价格过高，则做空X，同时Y的价格过低，则做多Y；
# 当差值为负的时候，我们认为X的价格过低，则做多X，同时Y的价格过高，则做空Y；
# 当差值为0时，价格被市场所修复，则全部平仓。

# 为了让差异更明显，我们定义的计算公式如下：
# 价差Z=X价格-Y价格
# Z>10时，做空X，做多Y；Z<0时，平仓
# Z<-10时，做多X，做空Y；Z>0时，平仓

#计算差价，然后计算交易统计
df$diff<-df$x-df$y

#找到差价大于10的点
idx<-which(df$diff>10)
idx<-idx[-which(diff(idx)==1)-1]

#打印差价的索引值
idx

# 接下来，我们进行模拟交易，取第一个索引值的点，在2010-01-04时做空X，做多Y。
# 当差价小于0，即在2010-01-06时，进行平仓
head(df,20)

#当差价大于10时，做空X，当差价小于0时，平仓
#第4行做空，第6行平仓
xprofit<- df$x[4]-df$x[6];xprofit

#当差价大于10时，做多Y，当差价小于0时，平仓
#第4行做空，第6行平仓
yprofit<- df$y[6]-df$y[4];yprofit

# 从交易结果来看，我们第一笔配对交易就是赚钱的。
# 原因：根据配对交易的假设条件，如果两个金融产品的价差是收敛的，通过协整性检验的方法，
# 我们可验证数据的收敛性。那么如果数据是收敛的，它还会具备均值回归的特性。

#画出X、Y的价差图
plot(df$diff,type='l')
# 可以明显的看出，价差一直围绕着0上下波动，这是明显收敛的，同时符合均值回归的特性
# 这就是市场的规则，通过配对交易的方法，我们找到市场的无效性，从而赚取套利的收益。


#6.3.3 用R语言实现配对交易

#1.数据准备

# 本节用到的数据，是铜的1分钟线的数据，从2016年日2月1日到2016年日2月29日日盘的交易数据，
# 以CSV格式保存到本地文件cu1605.csv和cu1606.csv。商品期货的日盘交易时间分为3段：9：00-10：15，
# 10：30-11：30，13：30-15：00。当前测试不考虑夜盘的数据。

setwd("D:/significant_data/R/b站分享/R语言量化投资/part 3 金融策略实战 chapter 6 量化投资策略案例")
csv1<-read.csv(file="cu1605.csv")
csv2<-read.csv(file="cu1606.csv")

# 一共5列：
# ·第1列：交易时间，date
# ·第2列：开盘价，Open，35870。
# ·第3列：最高价，High，35900。
# ·第4列：最低价，Low，35860。
# ·第5列：收盘价，Close，35880。

# 通过R语言加载铜的1分钟线数据，因为我们进行日内交易，所以在加载时就进行了转换，
# 按日期进行分组，生成R语言的list对象，同时把每日的data.frame类型对象转成XTS时间序列类型对象，方便后续的数据处理
install.packages("xts")
install.packages("TTR")
library(xts)
library(TTR)

#读取csv数据文件
read<-function(file){ 
  df<-read.table(file=file,header=FALSE,sep = ",", na.strings = "NULL") #读文件
  names(df)<-c("date","Open","High","Low","Close")                      #设置列名
  dl<-split(df,format(as.POSIXct(df$date),'%Y-%m-%d'))                  #按日期分组
  
  lapply(dl,function(item){
    xts(item[-1],order.by = as.POSIXct(item$date))                      #换成xts类型数据
  })
}

#加载数据
cu1605<-read(file='cu1605.csv')
cu1606<-read(file='cu1606.csv')

#查看数据类型
class(cu1605)

#查看数据的日期索引值
names(cu1605)

#查看每日的数据量
nrow(cu1605[[1]])

#查看cu1605合约的数据
head(cu1605[['2016-02-01']])


#2.配对交易模型

# 以2016年2月1日为例进行交易，以1分钟线的close价格来计算cu1605和cu1606两个合约的价差。
# 下面我们对数据进行操作，合并2个合约在2016年2月1日的数据，并对空值进行处理，最后计算出两个合约的价差

#合并数据
xdf<-merge(cu1605[['2016-02-01']]$Close,cu1606[['2016-02-01']]$Close)
names(xdf)<-c('x1','x2')

#用前值替换空值
xdf<-na.locf(xdf)

#计算价差
xdf$diff<-xdf$x1-xdf$x2

#前20行
head(xdf,20)

# 数据解释：
# ·x1列：第一腿，对应cu1605合约。
# ·x2列：第二腿，对应cu1606合约。
# ·diff列：cu1605-cu1606。

#计算价差范围
range(xdf$diff)

#计算价差均值
mean(xdf$diff)

#画出价差分布柱状图
hist(xdf$diff,10)

# 从价差的结果看，每1分钟cu1605合约都小于cu1606合约，从-110到-20价差不等，并且以-63为均值上下反复震荡
# 假设以-63为均值回归点，当差值为大于-45的时候，认为X的价格过高做空X，同时Y的价格过低做多Y；
# 当差值小于-75的时候，认为X的价格过低做多X，同时Y的价格过高做空Y；
# 当差值为-63时，价格被市场所修复，则全部平仓。
# 以cu1605和cu1606的两个合约按照1∶1持仓进行配比，1手多单对1手空单。



#由于作者自己开发的配对交易软件包缺失，后续暂时缺失
#感兴趣的可直接参考作者后续分析：https://blog.csdn.net/fens/article/details/84634623?ops_request_misc=%257B%2522request%255Fid%2522%253A%2522166193551316782412585720%2522%252C%2522scm%2522%253A%252220140713.130102334..%2522%257D&request_id=166193551316782412585720&biz_id=0&utm_medium=distribute.pc_search_result.none-task-blog-2~all~sobaiduend~default-1-84634623-null-null.142^v44^pc_rank_34_default_2&utm_term=R%E8%AF%AD%E8%A8%80%E9%85%8D%E5%AF%B9%E4%BA%A4%E6%98%93&spm=1018.2226.3001.4187















