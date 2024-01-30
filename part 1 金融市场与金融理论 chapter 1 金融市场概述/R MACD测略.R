#MACD策略

#加载程序库
install.packages("quantmod")
install.packages("TTR")
install.packages("PerformanceAnalytics")
library(quantmod) #数据下载和图形可视化
library(TTR)  #进行模型计算
library(PerformanceAnalytics)  #进行指标风险
library(ggplot2)
library(scales)

#从雅虎财经下载全球指数数据
options(stringAsFactors=FALSE) #不变成属性数据，按字符串读入
symbols=c("^GSPC","^N225","^HSI","^STI","000001.SS")  
#GSPC:标普500，N225日经225，HSI:恒生指数，STI:富时新加坡STI,000001.SS:上证综指
suppressWarnings(getSymbols(symbols,src="yahoo",from="2012-01-01"))  #从2012-01-01开始


GSPC
#取指数价格调整后的数据，合并数据
df<-merge(GSPC$GSPC.Adjusted,HSI$HSI.Adjusted,N225$N225.Adjusted,STI$STI.Adjusted,`000001.SS`$`000001.SS.Adjusted`)
#对列重命名
names(df)<-c("GSPC","HSI","N225","STI","SSE")  

#查看数据前六行
head(df)

#查看数据最后六行
tail(df)

#查看数据类型，为xts（R语言中的时间序列类型）
class(df)

#画出全球指数
g<-ggplot(aes(x=Index,y=Value,colour=Series),data=fortify(df,melt=TRUE))
g<-g+geom_line(size=1)
g<-g+scale_y_continuous(breaks=seq(1000,50000,4000))
g<-g+ggtitle("Global Index")   
g

#各国指数成立时间不同，成分股也不同，导致指数值有的很大，有的很小，
#因此不能用指数大小判断好坏。通常情况转换为收益率进行比较
#全球指数的每日累计收益率
ret_df<-Return.calculate(df,method="discrete")
chart.CumReturns(ret_df,legend.loc="topleft",main="Cumulative Daily Rerturns for Global Index")

#收益率越高，说明指数在这期间是表现越多的，越应该投资
#⽇经 225（N225）指数⼤幅超越了其他指数，中国的上证综指（SSE）较平稳，标普 500（GSPC）⾛势稳健。


#接下来计算指数的平均年化收益率，如果我 们把钱⼀直投资在这些指数上⾯，那么每年的平均回报是多少呢？

#计算指数的平均年化收益率
Return.annualized(ret_df)   #标普500，日经指数年化回报率最高。综合波动性考虑，GSPC首选


#接下来，我们构建⼀个简单的MACD模型， 通过MACD策略再对上⾯5个指数构建交易策略。
MACD<-function(dt,n=30){
  names(dt)<-c('close')
  
  #MACD移动平均均线
  dat<-na.locf(dt)
  dat$ma<-SMA(dat$close,n)
  
  #交易信号
  sig_buy<-which(dat$ma-dat$close>0)
  sig_Sell<-which(dat$ma-dat$close<0)
  sig_buy<-sig_buy[which(diff(sig_buy)>1)]
  sig_Sell<-sig_Sell[which(diff(sig_Sell)>1)]
  if(first(sig_Sell)<first(sig_buy)) sig_Sell<-sig_Sell[-1]
  if(last(sig_Sell)<last(sig_buy)) sig_buy<-sig_buy[-length(sig_buy)]
  
  #交易清单
  trade_dat<-do.call(rbind.data.frame, apply(cbind(sig_buy,sig_Sell),1,function(row){
    dt[row[1]:row[2],]
  }))
  
  #计算收益率
  ret_trade<-Return.calculate(trade_dat, method="discrete")
  return(ret_trade)
}
#MACD策略每日收益率
macd_ret<-lapply(df, function(col) MACD(col,30))

#MACD策略，年化收益率
t(do.call(rbind.data.frame, lapply(macd_ret,Return.annualized)))

# 写了⼀个MACD的策略函数，相当于建模的过程，函数的输出即策略的收益率。然后， 我们把指数数据传给MACD（）函数，
# 经过计算 输出策略收益率。最后，把策略收益率与纯指数 收益率放到⼀起来进⾏对⽐




#用一根均线的MACD策略，平均年化收益率会大幅优于纯指数的收益率


















