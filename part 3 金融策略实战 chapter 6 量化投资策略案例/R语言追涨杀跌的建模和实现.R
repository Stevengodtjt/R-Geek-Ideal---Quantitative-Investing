#1.数据准备

# 本次用到的数据可以通过新浪财经爬取，还可以用quantmod包从Yahoo财经下载。
# 本文用到的数据，包括A股日K线（向前复权）数据，从2014年7月到2015年日7月，以CSV格式保存到本地文件stock.csv
install.packages("plyr")
install.packages("xts")
install.packages("TTR")
install.packages("ggplot2")
install.packages("scales")
library(plyr)
library(xts)
library(TTR)
library(ggplot2)
library(scales)

setwd("D:/significant_data/R/b站分享/R语言量化投资/part 3 金融策略实战 chapter 6 量化投资策略案例")
csv<-read.csv(file="stock.csv")
# 一共7列：
# ·第1列：股票代码，code，000001.SZ
# ·第2列：交易日期，date，2014-07-02
# ·第3列：开盘价，Open，8.14
# ·第4列：最高价，High，8.18
# ·第5列：最低价，Low，8.10
# ·第6列：收盘价，Close，8.17
# ·第7列：交易量，Volume，28604171

# 通过R语言加载股票数据，由于数据所有股票都是混合在一起的，而进行计算时又需要按每只票股计算，
# 所以在数据加载时就进行了转换，按股票代码进行分组，生成R语言的list对象，同时把每只股票的
# data.frame类型对象转成XTS时间序列类型对象，方便后续的数据处理。

#读取csv数据文件
read<-function(file){ 
  df<-read.table(file=file,header=FALSE,sep = ",", na.strings = "NULL") #读文件
  names(df)<-c("code","date","Open","High","Low","Close","Volume")      #设置列名
  dl<-split(df[-1],df$code)                                             #按code分组
  
  lapply(dl,function(row){                                              #换成xts类型的数据      
    xts(row[-1],order.by = as.Date(row$date))
  })
}

#加载数据
data<-read("stock.csv")

#查看数据类型
class(data)

#查看数据的索引值
head(names(data))

#查看包括的股票数量
length(data)

#获得时间范围
dateArea<-function(sDate,eDate,before=0){
  return(paste(sDate-before,eDate,sep="/"))
}

#查看股票000001.SZ
head(data[['000001.SZ']])


#2.追涨杀跌模型

# 为了能拉近我们对市场的了解，取从2015年1月1日开始的数据来创建追涨杀跌的模型。以茅台
# （600519）为例，画出茅台自2015年以来的每日收盘价，以及20日最高价和10日最低价。

#日k线数据
title<-'600519.SH'
stock<-data[[title]]                           #获得股票数据                  
sDate<-as.Date("2015-01-01")                   #开始日期               
eDate<-as.Date("2015-07-16")                   #结束日期  
cdata<-stock[dateArea(sDate,eDate)]$Close  #获得收盘价
vdata<-stock[dateArea(sDate,eDate)]$Volume #获得交易量

#收盘价
names(cdata)<-"Value"  #重置列名
tail(cdata)

#交易量
tail(vdata)

#定义画图函数drawLine（），支持画出多条曲线，包括收盘价、最高价、最低价
drawLine<-function(cdata,titie="Stock",sDate=min(index(cdata)),eDate=max(index(cdata)),breaks="1 year"){
  if(sDate<min(index(cdata))) sDate=min(index(cdata))
  if(eDate>max(index(cdata))) eDate=max(index(cdata))  
  cdata<-na.omit(cdata)     
  
  g<-ggplot(aes(x=Index, y=Value),data=fortify(cdata[,1],melt=TRUE))
  g<-g+geom_line()
  
  if(ncol(cdata)>1){ #多条线
    g<-g+geom_line(aes(colour=Series),data=fortify(cdata[,-1],melt=TRUE))  
  }
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
  g<-g+ylim(min(cdata$Value), max(cdata$Value))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  g
}
drawLine(cdata,title,sDate,eDate,'1 month') #画出收盘价

#计算最近20日的最高价和10日的最低价
minmax<-function(data,max=20,min=10){
  d1<-na.locf(data,fromLast=TRUE)
  d2<-merge(d1,min=runMin(d1,min),max=runMax(d1,max))
  return(d2[,-1])
}

#画出股价，最高价和最低价
ldata<-cbind(cdata,minmax(cdata))
drawLine(ldata,title,sDate,eDate,'1 month')

# 图中有3条线，黑色线为茅台的每日收盘价，上方蓝色线为最近20日最高价，下方红色线为最近10日最低价
# 根据模型的计算公式，我们计算买入信号，当股价向上突破最近20日最高价格时买入

#买入信号函数
buyPoint<-function(ldata){   
  idx<-which(ldata$Value == ldata$max)
  return(ldata[idx,])                                  
}

#计算买入的点
buydata<-buyPoint(ldata)
buydata

#画出买入的信号图，可以直观地看到效果
drawPoint<-function(ldata,pdata,titie,sDate,eDate,breaks="1 year"){
  ldata<-na.omit(ldata)
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  
  if(is.data.frame(pdata)){
    g<-g+geom_point(aes(x=Index,y=Value,colour=op),data=pdata,size=4)
  }else{
    g<-g+geom_point(aes(x=Index,y=Value,colour=Series),data=na.omit(fortify(pdata,melt=TRUE)),size=4)  
  }
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  g
}

drawPoint(ldata,buydata$Value,title,sDate,eDate,'1 month')

# 如图所示，实心点为股价大于等于最近20日最高价的点，作为买入信号。
# 所有买入信号点都出现在单边上行的牛势中，对2015年上半年以来的行情来说，追涨的信号会被大量触发
# 接下来，我们继续计算卖出信号点，当股价小于等于最近10日最低价时作为卖出信号点。

#计算卖出信号点
stopPoint<-function(ldata,buydata){  
  idx<-which(ldata$Value == ldata$min)
  idx<-idx[which(c(0,diff(idx))!=1)]       #第一点用0表示
  selldata<-ldata[idx,]                    #所有低于最小值的点
  idx2<-sapply(index(buydata),function(e){ #买后的卖点
    head(which(index(selldata)>e),1)
  })
  return(selldata[unique(idx2),])
} 

#卖出信号
selldata<-stopPoint(ldata,buydata)
selldata

#一共有2笔卖出信号,为了让数据更加直观，我们合并买入信号和卖出信号，进行画图可视化
bsdata<-merge(buydata$Value,selldata$Value)
names(bsdata)<-c("buy","sell")
drawPoint(ldata,bsdata,title,sDate,eDate,'1 month')

# 图中，紫色圆点为卖出信号点，红色圆点为买入信号点。我们可以很明显地看出，
# 如果根据交易信号在红色点买入，紫色点卖出，是应该赚钱的。那么具体赚了多少呢，需要计算出来

#合并交易信号
signal<-function(buy, sell){
  selldf<-data.frame(sell,op=as.character(rep("S",nrow(sell))))
  buydf<-data.frame(buy,op=as.character(rep("B",nrow(buy))))
  sdata<-rbind(buydf,selldf) 
  #交易信号数据
  sdata[order(as.Date(row.names(sdata))),]
}

sdata<-signal(buydata,selldata)                                   
sdata

# 接下来，我们利用交易信号数据，进行模拟交易。设定交易参数和规则：
# ·以10万元人⺠币为本金。
# ·买入信号出现时，以收盘价买入，每次买入价值1万元的股票。如果连续出现买入信号，则一直买入。若现金不足1万元，则跳过买入信号。
# ·卖出信号出现时，以收盘价卖出，一次性平仓信号对应的股票。
# ·手续费为0元。
# 下面进行模拟交易：

trade<-function(sdata,capital=100000,fixMoney=10000){ #交易信号，总资金，每次定投资金
  amount<-0
  cash<-capital
  ticks<-data.frame()
  for(i in 1:nrow(sdata)){
    row<-sdata[i,]
    if(row$op=='B'){
      if(cash<fixMoney){
        print(paste(row.names(row),"No enough cash"))
        next
      }
      amount0<-floor(fixMoney/row$Value)             #本次交易量
      amount<-amount+amount0
      cash<-cash-amount0*row$Value
    }  
    if(row$op=='S'){
      cash<-cash+amount*row$Value
      amount<-0
    }     
    row$cash<-round(cash,2)
    row$amount<-amount
    row$asset<-round(cash+amount*row$Value,2)
    ticks<-rbind(ticks,row)
  }
  ticks$diff<-c(0,round(diff(ticks$asset),2))
  rise<-ticks[intersect(which(ticks$diff>0),which(ticks$op=='S')),] #赚钱的交易
  fall<-ticks[intersect(which(ticks$diff<0),which(ticks$op=='S')),] #赔钱的交易
  return(list(ticks=ticks,rise=rise,fall=fall))
}

result<-trade(sdata,100000,10000)  #交易结果(有6次现金不足)
result$ticks #交易明细

nrow(result$ticks)
# 一共发生了15笔交易，其中13笔买入，2笔卖出。最后，资金剩余121837.2元，赚了21837.2元，收益率为22%





#模型策略2：当股价低于前一个买入点的价格时进行卖出，小于10日最低价为止损点

#计算卖出的信号点
sellPoint<-function(ldata,buydata){
  arr<-c()
  for(i in 1:nrow(buydata)){
    if(i>1){                               #跳转第一个点
      date<-index(buydata[i,])  
      
      #价格小于上一次的买入价格就跳出
      last<-as.vector(buydata[i-1,]$Value) #上一次买入的价格
      lst<-ldata[paste(date,"/",sep="")]$Value      
      idx<-head(which(lst < last),1)
      if(length(idx)>0){        
        arr<-rbind(arr,index(lst[idx]))
      }
    }
  }
  selldata<-ldata[as.Date(unique(arr)),]
  
  #过滤多余的卖出点
  bsdata<-merge(buydata$Value,selldata$Value)
  names(bsdata)<-c("buy","Value")
  idx1<-which(!is.na(bsdata$Value))
  idx2<-idx1[which(c(0,diff(idx1))==1)]
  bsdata$Value[idx2]<-NA
  return(bsdata$Value[which(!is.na(bsdata$Value))])
}

#卖出信号
selldata<-sellPoint(ldata,buydata)
selldata
nrow(selldata) #卖出的信号点有7个，比上次多5个

#合并交易信号
sdata<-signal(buydata$Value,selldata$Value)
sdata

result<-trade(sdata,100000,10000)  #交易结果
result$ticks #交易明细（收益不如策略1）

#可视化
stopdata<-stopPoint(ldata,buydata)                         #止损信号
bsdata<-merge(buydata$Value,selldata$Value,stopdata$Value) #合并买卖信号、止损信号
names(bsdata)<-c("buy","sell","stop")
drawPoint(ldata,bsdata,title,sDate,eDate,'1 month')        #画图





















