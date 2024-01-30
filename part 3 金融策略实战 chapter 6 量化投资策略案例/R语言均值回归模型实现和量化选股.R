#6.1.2 均值回归模型

#本文用到的数据，包括A股日K线（向前复权）数据，从2014年7月到2015年日7月，以CSV格式保存到本地文件stock.csv
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
  dl<-split(df[-1],df$code) #按code分组
  
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

#查看股票000001.SZ
head(data[['000001.SZ']])

#把数据准备好就可以建立模型了


#2.均值回归模型

# 为了能拉近我们对市场的了解，我们取从2015年1月1日开始的数据，并创建均值回归模型。以平安银行
# （000001）的为例，画出平安银行2015年以来的日K线和均线。

#获得时间范围
dateArea<-function(sDate=Sys.Date()-365,eDate= Sys.Date(),before=0){
  if(class(sDate)=='character') sDate=as.Date(sDate)
  if(class(eDate)=='character') eDate=as.Date(eDate)  
  return(paste(sDate-before,eDate,sep="/"))
}

#计算移动平均线
ma<-function(cdata,mas=c(5,20,60)){
  if(nrow(cdata)<=max(mas)) return(NULL)
  ldata<-cdata
  for(m in mas){
    ldata<-merge(ldata,SMA(cdata,m))
  }
  names(ldata)<-c('Value',paste('ma',mas,sep=''))
  return(ldata)
}

#日k线和均线
title<-'000001.SZ'                                   
SZ000001<-data[[title]]                              #获得股票数据
sDate<-as.Date("2015-01-01")                         #开始日期
eDate<-as.Date("2015-07-10")                         #结束日期
cdata<-SZ000001[dateArea(sDate,eDate,360)]$Close     #获得收盘价
ldata<-ma(cdata,c(5,20,60))                          #选择移动平均指标
tail(ldata)                                          #打印移动平均指标

# 我们设置3条移动平均线，分别是5日平均线、20日平均线、60日平均线，
# 当然也可以按照自己的个性要求设置符合自己的周期。画出日K线和均线图。
drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=max(index(ldata)),breaks="1 year",avg=FALSE,out=FALSE){
  if(sDate<min(index(ldata))) sDate=min(index(ldata))
  if(eDate>max(index(ldata))) eDate=max(index(ldata))  
  ldata<-na.omit(ldata)
  
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  
  if(avg){
    meanVal<<-round(mean(ldata[dateArea(sDate,eDate)]$Value),2) #均值
    g<-g+geom_hline(aes(yintercept=meanVal),color="red",alpha=0.8,size=1,linetype="dashed")
    g<-g+geom_text(aes(x=sDate, y=meanVal,label=meanVal),color="red",vjust=-0.4)
  }
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
  g<-g+ylim(min(ldata$Value), max(ldata$Value))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  g
}

drawLine(ldata,title,sDate,eDate,'1 month',TRUE)                #画图

# 如图所示，60日的移动平均线是最平滑的，5日的移动平均线是波动最大的。
# 5日平均线和股价的交叉明显多于60日平均线和股价的交叉。
# 那么可以说在相同的时间周期内，短周期的移动平均线比⻓周期的移动平均线更具有均值回归的特点。

#分别计算不同周期的股价与移动平均线的差值的平均标准差：
getMaSd<-function(ldata,mas=20,sDate,eDate){
  if(is.null(ldata) || nrow(ldata)<= max(mas)) return(NULL)          
  col<-paste('ma',mas,sep='')
  ldata<-ldata[,c("Value",col)]                     
  ldata$dif<-ldata[,col]-ldata$Value          
  ldata$sd<-runSD(ldata[,"dif"],mas)               
  ldata$rate<-round(ldata$dif/ldata$sd,2)        
  ldata[dateArea(sDate,eDate)]                   
}

#5日平均线的差值、平均标准差
ldata5<-getMaSd(ldata,5,sDate,eDate)
head(ldata5)

#20日平均线的差值、平均标准差
ldata20<-getMaSd(ldata,20,sDate,eDate)
head(ldata20)

#60日平均线的差值、平均标准差
ldata60<-getMaSd(ldata,60,sDate,eDate)
head(ldata60)

# 5日的平均线的差值和平均标准差是最小的，而60日的平均线的差值和平均标准差是最大的。
# 如果我们以5日移动平均线作为均值，会频繁进行交易，但每次收益都很小，可能都不够手续费的成本；
# 另一方面，如果我们以60日移动平均线做为均值，交易次数会较少，但可能会出现股票形成趋势性上涨或下跌，
# ⻓时间不能回归的情况，造成现金头寸的紧张。综合上面的2种情况，我们可以选择20日均线作为均值的标的。

# 根据模型的计算公式，当差值超过2倍的平均标准差时，我们认为股价出现了偏离，以偏离点作为模型的
# 买入信号，当均线和股价再次相交时作为卖出信号。上一步，我们已经计算出了偏离值，并保存在rate列中。
# 下面我们要找到大于2倍标准化差的点，并画图。

#差值和平均标准差，大于2倍平均标准差的点
buyPoint<-function(ldata,x=2,dir=2){     
  idx<-which(ldata$rate>x)           
  if(dir==2){                      
    idx<-c(idx,which(ldata$rate<x*-1))
  }
  return(ldata[idx,])                                  
}

#画交易信号点
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

#多空信号点
buydata<-buyPoint(ldata20,2,2)      
#画图
drawPoint(ldata20[,c(1,2)],buydata$Value,title,sDate,eDate,'1 month')

# 图中的圆点就是买入的信号点，由于我们对股票只能进行单向交易，即低买高卖，并不能直接做空，
# 所以我们要过滤股价高于移动平均线的点，只留下股价低于移动平均线的点，即我们的买入信号点。

#画出买入信号点，只保留股价低于移动平均线的点
buydata<-buyPoint(ldata20,2,1) #做多信号点
drawPoint(ldata20[,c(1,2)],buydata$Value,title,sDate,eDate,'1 month') #画图

#计算卖出的信号点，当买入后，下一个股价与移动平均线的交点就是卖出的信号点，看看是否可以赚到钱
sellPoint<-function(ldata,buydata){  
  buy<-buydata[which(buydata$dif>0),]
  
  aidx<-index(ldata[which(ldata$dif<=0),])
  sellIdx<-sapply(index(buy),function(ele){
    head(which(aidx>ele),1)
  })
  ldata[aidx[unique(unlist(sellIdx))]]
}

#卖出信号
selldata<-sellPoint(ldata20,buydata)
selldata

#把买入信号和卖出信号合并到一张图
bsdata<-merge(buydata$Value,selldata$Value)
names(bsdata)<-c("buy","sell")
drawPoint(ldata20[,c(1,2)],bsdata,title,sDate,eDate,'1 month')

# 如图所示，我们在左侧4个点的位置进行买入，而在最右侧点位置进行卖出，确实是赚钱的。
# 那么究竟赚了多少钱呢？我们还需要精确的计算出来。

#  利用交易信号数据进行模拟交易。我们设定交易参数和规则：
# ·以10万元人⺠币为本金。
# ·买入信号出现时，以收盘价买入，每次买入价值1万元的股票。如果连续出现买入信号，则一直买入。若现金不足1万元时，则跳过买入信号。
# ·卖出信号出现时，以收盘价卖出，一次性平仓信号对应的股票。
# ·手续费为0元

#合并交易信号
signal<-function(buy, sell){
  selldf<-data.frame(sell,op=as.character(rep("S",nrow(sell))))
  buydf<-data.frame(buy,op=as.character(rep("B",nrow(buy))))
  sdata<-rbind(buydf,selldf)                        
  sdata[order(as.Date(row.names(sdata))),]
}

#交易信号数据
sdata<-signal(buydata,selldata)               
sdata

#模拟交易
trade<-function(sdata,capital=100000,fixMoney=10000){
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
      amount0<-floor(fixMoney/row$Value)
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
  rise<-ticks[intersect(which(ticks$diff>0),which(ticks$op=='S')),] 
  fall<-ticks[intersect(which(ticks$diff<0),which(ticks$op=='S')),] 
  
  return(list(
    ticks=ticks,
    rise=rise,
    fall=fall
  ))
}

#交易结果
result<-trade(sdata,100000,10000)  

#看一下每笔交易的明细
result$ticks
#一共发生了5笔交易，其中4笔买入，1笔卖出。
#最后，资金剩余103275.88元，赚了3275.88元，收益率为3.275%。（本金为100000元）

result$rise #在卖出时，赚钱的交易有1笔
result$fall #在卖出时，赔钱的交易没有发生

#再对比一下资产净值和股价

#资产净值曲线
drawAsset<-function(ldata,adata,sDate=FALSE,capital=100000){
  if(!sDate) sDate<-index(ldata)[1]
  adata<-rbind(adata,as.xts(capital,as.Date(sDate)))
  
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(x=as.Date(Index), y=Value,colour=Series),data=fortify(adata,melt=TRUE))
  g<-g+facet_grid(Series ~ .,scales = "free_y")
  g<-g+scale_y_continuous(labels=dollar_format(prefix = "￥"))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  g
}

drawAsset(ldata20,as.xts(result$ticks['asset']))  #画图

# 我们对一支股票进行了测试，发现是有机会的，那么我再换另外一支股票，看一下是否有同样的效果。
# 把刚才数据操作的过程封装到统一的quick函数中，就可以快速验证均值回归在其他股票的表现情况了
quick<-function(title,sDate,eDate){ 
  stock<-data[[title]]
  cdata<-stock[dateArea(sDate,eDate,360)]$Close
  ldata<-ma(cdata,c(20))
  ldata<-getMaSd(ldata,20,sDate,eDate)
  buydata<-buyPoint(ldata,2,1)  
  selldata<-sellPoint(ldata,buydata)
  sdata<-signal(buydata,selldata)
  return(trade(sdata))
}

#用TCL科技（000100）试一下，看看有无机会
title<-"000100.SZ"
sDate<-as.Date("2015-01-01")  #开始日期
eDate<-as.Date("2015-07-10")  #结束日期
quick(title,sDate,eDate)

#从数据结果看，又赚到了。一共发生了8笔交易，其中6笔买入，2笔卖出。
#最后资金剩余101102.18元，赚了1102.18元，收益率为1.1%

#画出交易信号图
stock<-data[[title]]
cdata<-stock[dateArea(sDate,eDate,360)]$Close
ldata<-ma(cdata,c(20))
ldata<-getMaSd(ldata,20,sDate,eDate)
buydata<-buyPoint(ldata,2,1)  
selldata<-sellPoint(ldata,buydata)
bsdata<-merge(buydata$Value,selldata$Value)
names(bsdata)<-c("buy","sell")
drawPoint(ldata[,c(1,2)],bsdata,title,sDate,eDate,'1 month')


#6.1.3 量化选股

#对全市场股票进行扫描，首先计算差值、平均值和平均标准差
sDate<-as.Date("2015-01-01")   #开始日期
eDate<-as.Date("2015-07-10")   #结束日期

#计算差值、平均值和平均标准差
data0<-lapply(data,function(stock){         
  cdata<-stock[dateArea(sDate,eDate,360)]$Close
  ldata<-ma(cdata,c(5,20))
  getMaSd(na.omit(ldata),20,sDate,eDate)
})

#去掉空数据
data0<-data0[!sapply(data0, is.null)]   

#全市场股票
length(data)

#有效的股票
length(data0)

#查看第一只股票
head(data0[[1]])

# 第一次扫描后，有2697支股票是符合条件的，有85支股票由于数据样本不足被排除。
# 接下来，继续对2697支股票进行筛选，找到符合要求的买入信号点。

#计算买入信号
buys<-lapply(data0,function(stock){ 
  if(nrow(stock)==0) return(NULL)
  buy<-buyPoint(stock,2,1)
  if(nrow(buy)>0) {
    return(buy)
  }
})

#去掉空数据
buys<-buys[!sapply(buys, is.null)] 

#查看所有买入信号的股票
length(buys)

#查看买入信号
head(buys)

# 通过计算发现，有1819支股票在这半年中产生过买入信号。每支股票产生的买入信号的时间和频率都是不同，
# 这样我们就可以把钱分散投资到不同的股票上，同时分散⻛险。如果交易信号同一天出现在多支股票上，
# 而我们资金有限，又想让收益最大化，那么可以选择偏离值最大的股票进行交易，接下来找到每日偏离最大的股票

#合并数据，从list转型为data.frame
buydf<-ldply(buys,function(e){  
  data.frame(date=index(e), coredata(e))
}) 

#选出同一日rate最大的股票，作为买入信号
buydatas<-ddply(buydf, .(date), function(row) {  
  row[row$rate == max(row$rate ),]
}) 

#查看买入信号
nrow(buydatas)

#查看买入信号细节
head(buydatas)

#最后，我们选出81个买入信号点，基本上每个交易日都是买入信号。有了买入信号，接下来找出卖出信号

#卖出信号
selldatas<-data.frame()
for(i in 1:nrow(buydatas)){
  buydata<-buydatas[i,]
  if(is.data.frame(buydata)){
    buydata<-xts(buydata,order.by=as.Date(buydata$date))
  }
  
  ldata<-data0[[buydata$.id]] 
  sell<-sellPoint(ldata,buydata)
  
  if(nrow(sell)>0){
    sell<-data.frame(sell,.id=buydata$.id,date=index(sell))
    selldatas<-rbind(selldatas,sell)
  }
}

#卖出信号去重
selldatas<-unique(selldatas)  
nrow(selldatas)

#查看卖出信号
head(selldatas)

#通过计算，一共有33个卖出信号点。最后，合并买入信号和卖出信号，并计算收益
buydatas$op<-'B'                   #买入标志     
selldatas$op<-'S'                  #卖出标志
sdatas<-rbind(buydatas,selldatas)  #合并数据
row.names(sdatas)<-1:nrow(sdatas)  #重设行号
sdatas<-sdatas[order(sdatas$.id),] #按股票代码排序

#查看合并的信号
head(sdatas)

#最后，按照股票进行分组，分别计算个股的收益

#计算个股的收益
slist<-split(sdatas[-1],sdatas$.id) #按股票代码分组
results<-lapply(slist,trade)

#查看信号的股票
names(results)

#查看万科A（000002）的股票
results[['000002.SZ']]$ticks       #通过优化的规则设计，一共有2笔交易，赚了495元

# 本节到此就要结束了！但其实还有很多的事情要做，比如对模型参数的优化，用10日均线代替20日均线，
# 用3倍标准差偏移代替2倍标准差偏移，对样本进行正态分布的检验，结合其他趋势类模型共同产生信号等

