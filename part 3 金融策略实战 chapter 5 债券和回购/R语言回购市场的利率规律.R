#整理回购GC001数据

#首先对数据进行简单的可视化，整体观察一下数据情况：
#以收盘价最高价作曲线图，并以利率10%作为标线，看看有多⻓时间，回购利率是高于10%的
install.packages("xts")
install.packages("ggplot2")
install.packages("scales")
library(xts)
library(ggplot2)
library(scales)

#读入数据
setwd("D:/significant_data/R/b站分享/R语言量化投资/part 3 金融策略实战 chapter 5 债券和回购")
gc<-read.csv(file="GC001.csv",header=TRUE,encoding="UTF-8")
names(gc)<-c('date','open','high','low','close','value','volumn')

#转为时间序列类型
gcx<-xts(gc[,-1],order.by=as.Date(gc$date,formate='%Y-%m-%d'))

#画图，利率走势
g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(gcx[,c('high','close')],melt=TRUE))
g<-g+geom_line(size=1)
g<-g+geom_hline(yintercept=10,col='blue',size=1)
g<-g+scale_y_continuous(breaks=seq(0,100,10))
g

# 图中，红色线为回购利率最高价，蓝色线为回购利率的收盘价，平行于X轴的直线为10%的利率水平。
# 我们可以明显地看出，在2007年到2008年间，回购利率异常的高，表示市场突然缺钱，而且是之前未出现过的情况。
# 从2012年到2015年，回购市场的利率频繁地超过10%的利率水平，表示市场缺钱的一种常态化。

# 再来分析一下回购市场的规律，一般在什么时间会出现利率升高。
# 我们先找出最高价大于6%的数据，然后计算在周几出现的机率较大，在每月几日的出现几率较大。
install.packages("lubridate")
library(lubridate)

#收益率大于6
gcx6<-gcx[which(gcx$high>6),]
#一般在周几出现几率较大
table(wday(index(gcx6))-1)
#一般在每月几日的出现几率大
table(days_in_month(index(gcx6)))

# 从数据的结果来看，GC001在周四时，最高价在6%以上出现的次数最多，
# 这是因为周四正回购方借到的钱可以使用3天，包括了周末。

# 从月度分析，发现在月末最高价在6%以上出现的次数最多，
# 这是因为很多短期票据到期，银行结算时临时缺钱。

# 那么，我们可以抓住这样的机会，在利率升高时交易逆回购，获得更高的无⻛险收益。

#最后，回到现实生活中，建议大家可以开个户试一下逆回购。逆回购虽然赚不到大钱，不过转移银行储蓄存款倒是还不错的选择。



