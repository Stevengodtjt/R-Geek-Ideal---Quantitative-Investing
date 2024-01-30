#4.企业债统计分析

install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)
setwd("D:/significant_data/R/b站分享/R语言量化投资/part 3 金融策略实战 chapter 5 债券和回购")
#上交所企业债
sh<-read.table(file="sh_bond.csv",header=FALSE,sep=",",colClasses = "character",fileEncoding="utf-8", encoding = "utf-8")
head(sh)
#数据一共6列：
#·名称
#·代码
#·简称
#·银行间代码
#·银行间简称
#·到期日期
names(sh)<-c("名称","代码","简称","银行间代码","银行间简称","到期日期")
sh$到期日期<-as.Date(sh$到期日期,format="%Y-%m-%d")
head(sh)

#深交所企业债
sz<-read.table(file="sz_bond.csv",header=FALSE,sep=",",colClasses = "character",fileEncoding="utf-8", encoding = "utf-8")
head(sz)
names(sz)<-c("名称","代码","简称","银行间代码","银行间简称","到期日期")
sz$到期日期<-as.Date(sz$到期日期,format="%Y-%m-%d")
head(sz)

#（1）合并所有未到期的企业债券
ss<-rbind(sh,sz) #按行合并
head(ss)

#接下来，利用基本的债券数据，我们开始做一些对债券市场的研究分析。
#首先，计算最近到期的前10支债券。
tmp<-ss[order(ss$到期日期),]   
tmp
head(tmp[,c(2,3,6)],10) #ss第2、3、6列
# 由于数据采集的时间是在2013年12月30日，那么最近到期的债券是在2014年02月19日，还有2个月的时间。

#把所有债券按到期时间进行统计，并以柱状图形式输出到期日期分布
g<-ggplot(ss, aes(x=到期日期))
g<-g+geom_histogram(binwidth=50,position="identity")
g<-g+scale_x_date(breaks = date_breaks(width="1 year"),labels = date_format("%Y"), limits = c(as.Date("2014-01-01"),as.Date("2024-01-01")))
g<-g+xlab("到期日期")+ylab("债券数量")
g
# 图中，2014年到期的债券并不多，从2018年到2020年，会有大批的债券到期，
# 这几年到来时企业将面临着非常大的还债压力，同时银行也承担着极大的⻛险，
# 一旦出现违约事情，后果将不堪设想。

#（2）再计算最近付息的前30支债券。
ss$付息日期<-as.Date(paste(2014,format(ss$到期日期,format='-%m-%d'),sep=""))
tmp<-ss[order(ss$付息日期),]     
head(tmp[,c(2,3,6,7)],20)

#对所有债券按付息日期进行统计，并以柱状图形式输出付息日期分布
g<-ggplot(ss, aes(x=付息日期))
g<-g+geom_histogram(binwidth=1,position="identity")
g<-g+scale_x_date(breaks=date_breaks("1 month"),labels = date_format("%Y%m"), limits = c(as.Date("2014-01-01"),as.Date("2014-12-31")))
g<-g+xlab("付息日期")+ylab("债券数量")
g
# 从付息的分析情况来看，每月都是比较平稳的，2014年的11月略多于其他月份。














