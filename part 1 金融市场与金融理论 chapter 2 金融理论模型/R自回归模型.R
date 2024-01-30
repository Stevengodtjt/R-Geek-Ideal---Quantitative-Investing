#2.4.2

#首先生成一个随机游走的数据集，满足平稳性的要求
set.seed(0) 
# 这个函数的主要目的，是让你的模拟能够可重复出现，因为很多时候我们需要取随机数，
# 但这段代码再跑一次的时候，结果就不一样了，如果需要重复出现同样的模拟结果的话，就可以用set.seed()

x<-w<-rnorm(1000) #生成符合正态分布N(0,1)的数据
for(t in 2:1000){
  x[t]<-x[t-1]+w[t]
}

tsx<-ts(x) # 生成ts时间序列的数据集
head(tsx,15) # 查看数据集
plot(tsx) # 生成可视化图形
a<-ar(tsx)# 进行自回归建模
a
#自相关系数为0.9879，非常强的自相关性，符合自相关性的特征
# R语⾔中ar（）函数提供了多种⾃相关系数的估计，包括“yule—walker”“burg”“ols”“mle”，默认是⽤yule-walker⽅法，
# 常⽤的⽅法还有最⼩⼆乘法（ols），极⼤似然法（mle）。我们⽤最⼩⼆乘法来进⾏参数估计。
b<-ar(tsx,method = "ols")
b
#用最小二乘法的计算结果，则自相关系数为0.9911，截距为-0.017，只有使用ols的时候才会有截距
#用极大似然估计：
d<-ar(tsx,method = "mle")
d
#⽤极⼤似然法计算结果，则⾃相关系统数为0.9904。对于上⾯3种估计⽅法，⾃相关系数的值都是很接近的。

#2.4.3

u<-mean(tsx) #均值
v<-var(tsx) #方差
#1阶滞后
p1<-sum((x[1:length(tsx)-1]-u)*(x[2:length(tsx)]-u))/((length(tsx)-1)*v)
p1
#2阶滞后
p2<-sum((x[1:(length(tsx)-2)]-u)*(x[3:length(tsx)]-u))/((length(tsx)-1)*v)
p2
#3阶滞后
p3<-sum((x[1:(length(tsx)-3)]-u)*(x[4:length(tsx)]-u))/((length(tsx)-1)*v)
p3
#同时可以用acf()函数来计算，打印前30个滞后的ACF值
acf(tsx)$acf

# ⽐较前3个滞后值的计算结果，与我们⾃⼰的计算结果是⼀样的
# 数据的ACF为拖尾，存在很严重的⾃相关性。接下来，这时候我们⽤偏⾃相关函数确定⼀下AR的阶数
pacf(tsx)$acf
# 从图中结果分析，当滞后为1时AR模型显著，滞后为其他值是PACF的值接近于0不显著。
# 所以，对于数据集tsx来说，数据满⾜AR（1）的⾃回归模型。对于上⽂中参数估计出的1阶⾃相关系数值是可以⽤的。

#2.4.4

#使⽤AR（1）模型进⾏预测，并保留前5个预测点：
predict(a,10,n.ahead=5L)
# 结果中，变量$pred表⽰预测值，变量$se为误差。
# 可以⽣成可视化的图，更直观的看到预测的结果
tsp<-predict(a,n.ahead = 50L) #生成50个预测值
#把原数据画出来
plot(tsx)
#把预测值和误差画出来
lines(tsp$pred,col='red')
lines(tsp$pred+tsp$se,col='blue')
lines(tsp$pred-tsp$se,col='blue')
# 从图中看到，⿊⾊线为原始数据，在右侧红⾊线为预测值，蓝⾊线为预测值的范围。
# 这样我们就利⽤AR（1）模型，实现了对规律的预测计算。

# 上⾯关于预测和可视化的过程，我们是通过原⽣的predict（）函数和plot（）函数完成的。
# 在R语⾔中，可以⽤forecast包来简化上⾯的操作过程，让代码更少，操作更便捷。
install.packages("forecast")
library(forecast)
#生成模型AR(1)：
a2<-arima(tsx,order = c(1,0,0))
tsp2<-forecast(a2,h=50)
plot(tsp2)
#查看forecast()计算后的预测结果
tsp2
#通过forecast（）函数，我们得到了直接⽣成的Forecast值、80%概率的预测值范围和95%概率的预测值范围。
#自回归模型done！



