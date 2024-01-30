#1.了解国债

install.packages("ggplot2")
library(xts)
library(ggplot2)
#读入数据bondSaving.csv
setwd("D:/significant_data/R/b站分享/R语言量化投资/part 3 金融策略实战 chapter 5 债券和回购")
bs<-read.csv("bondSaving.csv",header=TRUE)
head(bs)
#重新定义列名,bond为国债，saving为储蓄
names(bs)<-c("date","bond","saving")
#转换为时间序列类型
bsxts<-xts(bs[-1],order.by=as.Date(bs$date))
head(bsxts)
#对空值向后填充
bsxts<-na.locf(bsxts,fromLast = FALSE)

#na.locf说明（A function for replacing each NA with the most recent non-NA prior to it.）
na.locf(c(1, NA, NA, NA))
na.locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))

#画图
g<-ggplot(aes(x=Index, y=Value, colour=Series),data=fortify(bsxts,melt=TRUE))
g<-g+geom_line()
g 

# 图中，x轴为时间，y轴为利率值。我们对2002年〜2015年1年期债券到期收益率和
# 1年期定期存款的利率进行比较。陡峭的曲线（bond）为中债国债到期收益率，
# 平直曲线（saving）为中国人⺠银行发布的1年期定期储蓄存储。很多时候，
# 国债利率都高于存款利率。为什么是这样的情况？这需要我们更深入地理解中国经济才能解释。



#4.国债的历史表现

#读取数据bonds.csv
bonds<-read.csv("bonds.csv",header=TRUE)
head(bonds)
#列名重新定义ytm1为1年期，ytm3为3年期，ytm10为10年期
names(bonds)<-c("date","ytm1","ytm3","ytm10")
head(bonds)
#转换为时间序列类型
bondsxts<-xts(bonds[-1],order.by=as.Date(bonds$date))
bondsxts<-na.locf(bondsxts,fromLast=FALSE)
#画出收益率曲线
g<-ggplot(aes(x=Index, y=Value, colour=Series),data=fortify(bondsxts,melt=TRUE))
g<-g+geom_line()
g 

# 图中位于上方的曲线为10年期国债到期收益率，位于中间的曲线为3年期国债到期收益率，
# 位于下方的曲线为1年期国债到期收益率。通过画图对比我们发现：
# ·10年国债到期收益率的走势比1年期和3年期的平稳，1年期的走势波动是最大的。
# ·10年期国债到期收益率基本都在3%以上，在4%左右上下波动，受利率调控影响明显。
# ·1年期国债的到期收益率在1%〜4%上下波动，受货币市场短期流动性影响明显。

#接下来，对数据进行量化，用数据分析的方法计算出指标，来验证直观的判断。
#比如，计算3条收益率曲线的标准化差，以衡量波动性。

#计算标准差
apply(bonds[,-1],2,sd) 
# 从计算结果来看，1年期到期收益率的标准差是0.729，是最大的，而10年期的到期收益率的标准差是0.588，是最小的。

#再按年来划分，分别计算3条收益率曲线的标准差。
sdy<-apply.yearly(bondsxts,function(cols) apply(cols,2,sd))
sdy
#然后，我们找到标准差变动最大的，看看是哪年
apply(sdy,2,max)                     #最大值
sdy[unique(apply(sdy,2,which.max)),] #最大值出现的时间

# 1年期国债和3年期国债的最大变动值，都超过了10年期国债，在2008年时波动最大。而10年期国债在2005年波动最大。


#换个角度看，以年为周期，看一下10年国债收益率的情况。

install.packages("lubridate")
install.packages("reshape2")
library(lubridate)
library(reshape2)
#整理数据
ytm<-bondsxts$ytm10['2010/'] #取从2010开始的数据
years<-unique(year(index(ytm)))
df<-data.frame(matrix(NA,ncol=length(years),nrow=366))
names(df)<-years
#按年分组
apply.yearly(ytm,function(x){
  ycol<-format(index(x),'%Y')[1]
  df[yday(x),ycol]<<-x
})
#整理数据
df<-cbind(id=1:366,df)
dt<-na.omit(melt(df,id.vars="id"))
#画图
g<-ggplot(aes(x=id,y=value, colour=variable),data=dt)
g<-g+geom_line(size=1)
g<-g+xlab('Day of Year')
g 

# x轴代表一年中的365天，y轴代表收益率的值。图中可以明显的发现，2015年下半年收益率开始下行，
# 这与2015年降息有很大关系。从2017年开始，收益率开始上行，预示着央行可能要开始加息了。






