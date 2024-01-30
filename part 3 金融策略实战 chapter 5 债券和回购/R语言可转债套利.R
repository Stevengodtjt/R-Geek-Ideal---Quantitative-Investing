#3.1 获取数据

install.packages("rjson")
library(rjson)
#读入json数据
setwd("D:/significant_data/R/b站分享/R语言量化投资/part 3 金融策略实战 chapter 5 债券和回购")
json<-fromJSON(paste(readLines("cb.json"),collapse=""))
#转换json为data.frame
df<-data.frame(matrix(unlist(json),nrow=length(json),byrow=TRUE),stringsAsFactors = FALSE)
head(df)
#对列重命名
names(df)<-names(json[[1]])
head(df,2)

# ·转债代码：symbol
# ·代码：code
# ·名称：name
# ·动态市盈率：pettm
# ·交易量：volume
# ·是否操作：hasexist，雪球网站自己定义的功能
# ·当前价：current
# ·涨跌幅：percent
# ·最高价：high
# ·最代价：low
# ·52周最高价：high52w
# ·52周最低价：low52w
# ·开盘价：open
# ·正股代码：kzz_stock_symbol
# ·正股简称：kzz_stock_name
# ·正股价：kzz_stock_current
# ·正股涨跌幅：kzz_stock_percent
# ·转股价：kzz_convert_price
# ·转股价值：kzz_covert_value，衍生指标，转股价值=正股现价/转股价*100
# ·溢价率（%）：kzz_cpr，衍生指标，溢价率=（转债现价-转股价值）/转股价值
# ·回售触发价：kzz_putback_price，募集说明书约定的连续N日低于转股价的X%可提前回售，回售触发价=当期转股价*X%
# ·转股期间：kzz_convert_time
# ·纯债价值：kzz_straight_price
# ·正股净资产：net_assets
# ·税前收益（%）：benefit_before_tax
# ·税后收益（%）：benefit_after_tax
# ·正股市净率：pb
# ·转债占比（%）：convert_bond_ratio
# ·到期赎回价格：kzz_redempt_price
# ·转债规模（亿）：totalissuescale
# ·剩余规模（亿）：outstandingamt
# ·到期时间：maturitydate
# ·剩余年限：remain_year
# ·标准券折算率：convertrate，衍生指标，标准券折算率=中登公布的标准券折算率/转债现价
# ·具体利率：interestrtmemo


#3.2 数据清洗和过滤

#查看数据类型，都是字符串类型的
str(df)
#把字符串型的数字列转为数值型
df$volume<-as.numeric(df$volume)
df$current<-as.numeric(df$current)
df$change<-as.numeric(df$change)
df$percent<-as.numeric(df$percent)
df$high<-as.numeric(df$high)
df$low<-as.numeric(df$low)
df$high52w<-as.numeric(df$high52w)
df$low52w<-as.numeric(df$low52w)
df$open<-as.numeric(df$open)
df$kzz_stock_current<-as.numeric(df$kzz_stock_current)
df$kzz_convert_price<-as.numeric(df$kzz_convert_price)
df$kzz_covert_value<-as.numeric(df$kzz_covert_value)
df$kzz_cpr<-as.numeric(df$kzz_cpr)
df$kzz_putback_price<-as.numeric(df$kzz_putback_price)
df$kzz_redempt_price<-as.numeric(df$kzz_redempt_price)
df$kzz_straight_price<-as.numeric(df$kzz_straight_price)
df$kzz_stock_percent<-as.numeric(df$kzz_stock_percent)
df$pb<-as.numeric(df$pb)
df$net_assets<-as.numeric(df$net_assets)
df$benefit_before_tax<-as.numeric(df$benefit_before_tax)
df$benefit_after_tax<-as.numeric(df$benefit_after_tax)
df$convert_bond_ratio<-as.numeric(df$convert_bond_ratio)
df$totalissuescale<-as.numeric(df$totalissuescale)
df$outstandingamt<-as.numeric(df$outstandingamt)
df$remain_year<-as.numeric(df$remain_year)
df$convertrate<-as.numeric(df$convertrate)
#再次查看数据类型，从chr变成了num
str(df)

#3.3 负溢价率套利策略

install.packages("stringr")
install.packages("magrittr")
library(stringr)
library(magrittr)
#连续数据变换
df2<-df[,c('kzz_convert_time')]  #提取每一行的转股期
df2<-df2 %>% str_split(pattern='-') %>%  #把转股期间拆开（开始和结束）
  unlist %>% 
  matrix(ncol=2, byrow=TRUE) %>% 
  data.frame(stringsAsFactors=FALSE) %>% 
  cbind(df$symbol,df$name)

#设置日期
date<-as.Date("2017-02-21")
date

#找到处于转股期的可转债
df2<-df2 %>% {
  df2$X1<-as.Date(df2$X1,format='%Y.%m.%d')
  df2$X2<-as.Date(df2$X2,format='%Y.%m.%d')
  df2[intersect(which(df2$X1 < date),which(df2$X2 > date)),]  
}

# 很多时候，我们在观察市场时，都会有自己独特的视角，个性的数据需求非常重要，
# 但市场工具基本都不能满足，只能靠自己编程来实现了。通过实时的对市场进行扫描，
# 第一时间发现套利机会，如当预期收益大于3%，亏损概率少于10%时，果断入场赌一把。

#创建自己定义的列表，用于观察可转债数据
df3<-df[which(df$symbol %in% df2$`df$symbol`),]                                 #细化匹配df2数据
df3$name2<-str_c(df3$name,'(',df3$code,')')                                     #df3新加一列：名称（代码）
df3$time<-format(Sys.time(),'%H:%M:%S')                                         #df3新加一列，操作时间
df3$current2<-str_c(df3$current,'(',df3$percent,')')                            #df3新加一列，当前价
df3$current3<-str_c(df3$kzz_stock_current,'(',df3$kzz_stock_percent,')')        #df3新加一列，正股价
cols<-c('name2','time','current2','current3','kzz_cpr')
df4<-df3[order(df3$kzz_cpr),cols]                                               #按顺序提取df3对应列
names(df4)<-c('转债名称','更新时间','转债价格(涨幅)','正股价格(涨幅)','溢价率') #改名
df4

# 我们可以监控可转债与正股之间的变化，当正股上涨，但可转债没与正股完全同步时，就存在套利空间。
# 那么，除了基于原始数据的基本指标，我们需要自定义一些衍生指标，比如预期收益率、亏损概率、
# 夏普比率等指标。这样我们就可以更直观地看指标，而不用自己在脑子里做数字的转化了，大脑放松，理性地决策。
# 我们需要再增加4个衍生指标，分别是预期收益率、20天波动率、亏损概率、年化夏普值。
# 我们可以整理一下4个衍生指标计算公式：

# (1)预期收益率=（转股价值-转债价格）/转债价格×100%
# (2)20日波动率=标准差（20日移动平均收益率）×平方根（250）
# (3)亏损概率=正态化的预期收益率，以0为均值，20日波动率为方差
# (4)年化夏普值=预期收益率/20日波动率×平方根（250）

# 我们以格力转债（110030）为例，计算格力转债（110030）的4个衍生指标。那么要计算这
# 几个指标，刚才的数据显然是不够的，我们需要再加入一些数据，主要是格力转债（110030）
# 的价格数据和格力地产（600185）的价格数据。

#把格力转债（110030）的价格数据，保存到110030.csv文件中
#把格力地产（600185）的原始价格数据保存到600185.csv文件中

#提取格力转债和格力地产的数据，进行指标计算
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#读取格力转债数据
df110030<-read.csv(file="110030.csv",header=TRUE)
x110030<-na.omit(df110030)
x110030<-xts(x110030[,c('收盘价','转股价值')],order.by = as.Date(as.character(x110030$日期),format='%Y-%m-%d'))   #时间递增顺序排列，只提取收盘价和转股价值
names(x110030)<-c('close','value')

#读取格力地产数据
df600185<-read.csv(file="600185.csv",header=TRUE)
x600185<-xts(df600185$close,order.by = as.Date(as.character(df600185$date),format='%Y-%m-%d'))
names(x600185)<-'stock'
x110030<-merge(x110030,x600185,all = FALSE)
head(x110030)

# 我们构造的数据集包括3列，分别为格力转债收盘价、转股价值、格力地产收盘价。
# 由于转股价格会直接影响溢价率，而转股价格又是以事件作为调整，我们需要在数据中设置转股价字段。

#转股价格，发生3次改变
x110030$convetPrice<-NA
x110030$convetPrice['2014-12-26']<-20.9
x110030$convetPrice['2016-05-26']<-7.39
x110030$convetPrice['2016-08-25']<-7.26
#对NA值进行填充
x110030<-na.locf(x110030)

# 接下来计算衍生指标

#转债溢价率
x110030$Premium<-(x110030$close - x110030$value)/x110030$value
#预期收益率
x110030$expRet<-(x110030$value-x110030$close)/x110030$close
#转债价格累计收益率
x110030$ret<-Return.calculate(x110030$close,method="log")
x110030$ret[which(is.na(x110030$ret))]<-0
x110030$ret<-cumprod(1 + x110030$ret)-1
#20天波动率
install.packages("TTR")
library(TTR)
x110030$vol <-volatility(x110030$close,n=20)
#亏损概率
x110030$loss<- pnorm(-1 * x110030$expRet, 0, x110030$vol)
#年化夏普率
x110030$sharpe<-x110030$expRet/x110030$vol*sqrt(250)

#对所有字段的比率乘100%
x110030$Premium<-x110030$Premium*100
x110030$expRet<-x110030$expRet*100
x110030$ret<-x110030$ret*100
x110030$vol<-x110030$vol*100
x110030$loss<-x110030$loss*100

# 查看我们自定义的数据计算结果，数据一共10列，包括格力转债价格（close）、转股价值（value）、
# 格力地产价格（stock）、转股价格（convetPrice）、溢价率%（Premium）、预期收益率%（expRet）、
# 转债收益率%（ret）、20日波动率%（vol）、损失概率%（loss）、年化夏普率%（sharpe）
tail(x110030)

# 从打印的最近6日的结果看，格力转债有高达40%的溢价率，转债的价值已经严重超过格力地产的价值了，
# 完全不具备转股套利的特征。我们试试，能不能找到收益率大于0的期间。
x110030[which(x110030$expRet>0),]

# 有4条记录预期收益率大于0，再次查询转股时间是从2015-06-30至2019-12-24，那么这4个
# 机会出现时，表示还没有开始进行转股。从整个结果来看格力转债不适用负溢价率套利的策略。



#4 数据可视化解读
install.packages("ggplot2")
install.apckages("scales")
library(ggplot2)
library(scales)

#画溢价率图
g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(x110030[,c('close','value','Premium')],melt=TRUE))
g<-g+geom_line(size=1)
g 

# 图中，把格力转债价格（close）、转股价值（value）和溢价率（Premium）放到一起，
# 非常明显地看出，格力转债从刚发行就被快速推高，溢价率一直维持在很高的水平，
# 这可能是人为的因素就是想避免转股的发生，经查询后发现，未转股比例达到了99.80%。
# 这么高的溢价率，如果市场能有反向操作，做空转债做多股票，那必定会大赚一笔，迅速把溢价率拉平。

g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(x110030[,c('value','stock','convetPrice')],melt=TRUE))
g<-g+geom_line(size=1)+scale_y_log10()
g 

# 图中，把格力地产价格（stock）、转股价格（convetPrice）和溢转股价值（value）放到一起，
# 格力地产在2016年5月进行了一次利润分配及转增股本，股价下调，同时转股价格调整，转股价值并没有发生变化。

g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(x110030[,c('ret','expRet','vol','loss','sharpe')],melt=TRUE))
g<-g+geom_line(size=1)
g 

# 图中，我们把所有衍生指标都放一起，发现格力转债的在2015年7月波动率非常大，
# 而进入2016年后波动率变小并趋于稳定。转债价格收益率从2015年7月开始，基本一直在下跌。
# 转股套利策略的预期收益率也是负的，夏普值也是负的，亏损概率在50%〜100%。最好别碰。

# 对一支转债整体分析了一遍后，接下来的事情就变得简单了。我们需要把所有转债的数据都拿到，
# 用上面的方法进行计算。如果获得的是实时数据，你就可以对整个可转债市场做到实时监控，
# 最后把结果发布到网站上动态更新，再加上短信、邮件等的提醒功能，你的自定义可转债交易监控系统就完成了





