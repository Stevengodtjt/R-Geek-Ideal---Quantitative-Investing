#2.3.2

#加载类库
library(xts)
library(reshape2)
library(ggplot2)

#设置环境变量
options(stringsAsFactors = FALSE)

#读取数据
dat<-read.csv("D:/significant_data/R/b站分享/R语言量化投资/part 1 金融市场与金融理论 chapter 2 金融理论模型/future.csv",sep=",")
#转型为xts格式
df<-xts(dat[,-1],order.by = as.Date(dat[,1])) #as.POSIXct也行
#数据集已存在df变量中
head(df,20)
# 索引：时间
# x1：焦炭(j1505)合约的1分钟线的报价数据
# x2：焦煤(jm1605)合约的1分钟线的报价数据
# x3：铁矿石(i1605)合约的1分钟线的报价数据
# x4：热卷(hc1605)合约的1分钟线的报价数据
# y： 螺纹钢(rb1605)合约的1分钟线的报价数据

# # 假设螺纹钢的价格与其他4个商品的价格有线性关系，
# 那么我们建⽴以螺纹钢为因变量，以焦煤、焦炭、铁矿⽯和热卷的为⾃变量的多元线性回归模型。
# ⽤公式表⽰为：y=a+b*x1+c*x2+d*x3+e*x4+ε（残差）


#2.3.3

#建立多元线性回归模型：
lm1<-lm(y~x1+x2+x3+x4,data=df)
#打印参数估计结果
lm1
#这样就得到了y和x关系的方程：y=212.878+0.8542*x1+0.6672*x2-0.6674*x3+0.4821*x4

#2.3.4
summary(lm1)
# T检验：所有自变量都非常显著
# F检验：模型非常显著,p值小于2.2e-16
# 调整后的R^2：相关性为0.972，非常强
# 通过的回归参数的检验与回归方程的检验，得到了最后的多元线性回归方程：
# 螺纹钢=212.878+0.85423*焦炭+0.6672*焦煤-0.6674*铁矿石+0.4821*热卷

#2.3.5
par(mfrow=c(2,2))
plot(lm1)
#1.Residuals vs Fitted
# 残差和拟合值（左上），残差和拟合值之间数据点均匀分布在y=0两侧，呈现出随机的分布，
# 红⾊线呈现出⼀条平稳的曲线并没有明显的形状特征。

#2.Normal Q-Q
# 残差QQ图（右上），数据点按对⾓直线排列，趋于⼀条直线，并被对⾓直接穿过，直观上符合正态分布。

#3.Scale-Location
# 标准化残差平⽅根和拟合值（左下），数据点均匀分布在y=0两侧，呈现出随机的分布，
# 红⾊线呈现出⼀条平稳的曲线并没有明显的形状特征。

#4.Residuals vs Leverage
# 标准化残差和杠杆值（右下），没有出现红⾊的等⾼线，则说明数据中没有特别影响回归结果的异常点。

#综上：没有明显的异常点，残差符合假设条件

#2.3.6 
par(mfrow=c(1,1))

#预测计算
dfp<-predict(lm1,interval="prediction")
#打印预测值
head(dfp,10)
#合并数据
mdf<-merge(df$y,dfp)
#画图
draw<-function(df){
  df2<-data.frame(df)
  df2$id<-index(df2)
  df2$date<-index(df)
  df3<-melt(df2,id=c("id","date"))
  
  g<-ggplot(data=df3,aes_string(x='id',y='value',colour='variable'))
  g<-g+geom_line()
  g
}
draw(mdf)
# y：实际价格，红色线
# fit：预测价格，绿色线
# lwr：预测最低价，蓝色线
# upr：预测最高价，紫色线
# 从图中看出，实际价格y和预测价格fit在大多数时候都很贴近，一个模型就训练好了


#2.3.7

#各变量之间的关系
pairs(as.data.frame(df))
#从图中发现x2与y之间的关系是最偏离线性的，则尝试从原模型去掉x2重新建模
lm2<-update(lm1,.~.-x2) #x1+x3+x4
summary(lm2)

# 当把⾃变量x2去掉后，⾃变量x3的T检验值反⽽变⼤了，同时Adjusted R-squared变⼩了，所以我们这次调整是有问题的。

# 通过⽣产和原材料的内在逻辑分析，焦煤与焦炭属于上下游关系。
# 焦煤是⽣产焦炭的⼀种原材料，焦炭是焦煤与其他炼焦煤经过配煤焦化形成的产品，
# ⼀般⽣产1吨焦炭需要1.33吨炼焦煤，其中焦煤⾄少占30%。

# 把焦煤和焦炭的关系改变⼀下，增加x1*x2的关系并匹配到模型，看看效果。
lm3<-update(lm1,.~.+x1*x2)
summary(lm3)

# 从结果中发现，增加了x1*x2列后，原来的x1、x2和Intercept的T检验都不显著。
# 继续调整模型，从模型中去掉x1，x2两个⾃变量。
lm4<-update(lm3,.~.-x1-x2)
summary(lm4)
# 从调整后的结果来看，效果还不错。不过，也并没有⽐最初的模型有所提⾼。

# 对于模型调整的过程，在我们⼿动调整测试时，⼀般都会基于业务知识来操作。
# 如果是按照数据指标来计算，我们可以⽤R语⾔中提供的逐步回归的优化⽅法，
# 通过AIC指标来判断是否需要参数优化:对刚才的lm3模型做逐步回归的模型调整。
step(lm3)  #最终结果显示还是最初的模型好


#2.3.8：案例

#定义读文件函数
dailyData <- function(file) {
  df <- read.table(file = file, header = FALSE,sep = ',', na.strings = 'NULL')
  names(df)<-c('date','price')
  return(df)
}
#转型为xts类型
toXts<-function(data,format='%Y-%m-%d %H:%M:%S'){
  df<-subset(data,select=-c(date))
  xts(df,order.by=strptime(data$date, format))       
}
#分别从文件中读取数据
x1<-toXts(dailyData(file='D:/significant_data/R/b站分享/R语言量化投资/part 1 金融市场与金融理论 chapter 2 金融理论模型/j_daily.csv'),'%Y%m%d') 
x2<-toXts(dailyData(file='D:/significant_data/R/b站分享/R语言量化投资/part 1 金融市场与金融理论 chapter 2 金融理论模型/jm_daily.csv'),'%Y%m%d') 
x3<-toXts(dailyData(file='D:/significant_data/R/b站分享/R语言量化投资/part 1 金融市场与金融理论 chapter 2 金融理论模型/i_daily.csv'),'%Y%m%d') 
x4<-toXts(dailyData(file='D:/significant_data/R/b站分享/R语言量化投资/part 1 金融市场与金融理论 chapter 2 金融理论模型/hc_daily.csv'),'%Y%m%d') 
y<-toXts(dailyData(file='D:/significant_data/R/b站分享/R语言量化投资/part 1 金融市场与金融理论 chapter 2 金融理论模型/rb_daily.csv'),'%Y%m%d') 

#合并数据集（删除缺失值）
df<-na.omit(merge(x1,x2,x3,x4,y)) 
#对列重命名
names(df)<-c('x1','x2','x3','x4','y')

# 对上⾯5个品种的⽇K线数据进⾏多元回归分析建模：
lm_test<-lm(y~x1+x2+x3+x4,data=df)
summary(lm_test)

#查看数据集的基本统计信息
summary(df)
#最后，⽣成期货⽇K线的5个品种的配对关系展⽰图
pairs(as.data.frame(df))

#结论：对于⿊⾊系的5个品种的⽇K线数据，同样具有⾮常强的相关关系，那么我们就可以把这个结论应⽤到实际的交易中了。








