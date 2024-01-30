#加载程序包
install.packages("quantmod")
install.packages("PerformanceAnalytics")
library(quantmod) #数据下载和图形可视化
library(PerformanceAnalytics)  #进行指标风险

#从yahoo下载3只股票数据，和SP500数据
symbols=c("IBM","GE","AAPL","^GSPC")  
getSymbols(symbols,src="yahoo",from="2010-01-01")
head(GSPC) #前6行
tail(GSPC) #后6行

#画出SP500的K线图
barChart(GSPC)

#对调整后的4个品种的价格进行合并

#改列名
names(IBM)<-c("open","high","low","close","volume","adjusted")
names(GE)<-c("open","high","low","close","volume","adjusted")  #通用电气
names(AAPL)<-c("open","high","low","close","volume","adjusted")
names(GSPC)<-c("open","high","low","close","volume","adjusted")

#数据合并
dat=merge(IBM$adjusted,GE$adjusted,AAPL$adjusted,GSPC$adjusted)
names(dat)<-c("IBM","GE","AAPL","GSPC")

#打印前6行
head(dat)

#计算每日收益率，合并收益率到dat_ret
IBM_ret<-dailyReturn(IBM)  
GE_ret<-dailyReturn(GE)
AAPL_ret<-dailyReturn(AAPL)
SP500_ret=dailyReturn(GSPC)
dat_ret<-merge(IBM_ret,GE_ret,AAPL_ret,SP500_ret)
names(dat_ret)<-c("IBM","GE","AAPL","SP500")
head(dat_ret)

#定义无风险收益率为4%，计算4个资产的平均年化收益率

#无风险收益率
Rf<-0.04/12
#计算平均年化收益率，平均年化标准差，平均年化Sharpe
results<-table.AnnualizedReturns(dat_ret,Rf=Rf)
results    #从平均年化收益率来看，这段时间标普的平均年化收益率有11%，很难超越

#再进行统计指标分析，从另外一个维度看数据
table.Stats(dat_ret)
#通过统计指标分析，每个资产有3132个样本点无缺失值；
#日最小收益率方面，GE为-0.15，日最大收益率方面，GE为0.15

#画出IBM股票的日收益图
chart.Bar(dat_ret[,1], main="IBM Daily Returns")
#IBM股票的月收益图
chart.Bar(monthlyReturn(IBM), main="IBM Monthly Returns")
#4个品种的累计收益率图
chart.CumReturns(dat_ret,main="Total Returns",legend.loc="topleft") #AAPL一骑绝尘
#相关性分析
chart.Correlation(dat_ret, histogram=TRUE, pch="+") #3只股票都和SP500呈较强的正相关性


#以SP500为市场组合，分别计算3只股票的alpha和beta
CAPM.alpha(dat_ret[,1:3],dat_ret[,4],Rf=Rf)
CAPM.beta(dat_ret[,1:3],dat_ret[,4],Rf=Rf)

#3只股票中，只有AAPL的alpha是正的，说明领先于市场
#3只股票中，GE的Beta是最大的且大于1，在上升时期beta越大，获得的市场收益也越大
#综合考虑可以配置大部分AAPL，少部分GE，少部分标普












