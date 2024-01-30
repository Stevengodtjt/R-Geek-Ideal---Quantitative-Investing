#2.2.2

#加载类库
library(zoo)
library(xts)

#设置环境变量
options(stringsAsFactors=FALSE)

#读取数据
dat<-read.csv("D:/significant_data/R/b站分享/R语言量化投资/part 1 金融市场与金融理论 chapter 2 金融理论模型/zn.csv",sep=",")
#转型为xts格式
df<-xts(dat[,-1],order.by=as.POSIXct(dat[,1]))
#数据集已存在df变量中
head(df)

#分别给x,y赋值
x<-as.numeric(df[,1])
y<-as.numeric(df[,2])

#画图
plot(y~x)

#从散点图上发现X和Y的排列基本都是在一条直线附近，那么可以先假设X和Y的关系是线性的Y=a+b*X+c
# a:截距 
# b:自变量系数 
# a+b*X：Y随X的变化而线性变化的部分 
# c：随机误差（一切不确定因素影响的总和，服从正态分布）


#2.2.3

#建立线性回归模型
lm.ab<-lm(y~x)
#打印参数估计结果
lm.ab
#有了a、b的值就可以画出回归线了
plot(y~x)
abline(lm.ab)  #要评价这条回归线的好坏就需要对回归模型进行显著性检验

#2.2.4
summary(lm.ab)

#Call：列出了回归模型的公式
#Residuals：列出了残差的统计数据
#Coefficients：参数估计的计算结果
#通过查看模型的结果数据，发现通过t检验的截距和自变量x都是非常显著的；通过F检验判断整个模型的自变量是非常显著的；
#R^2的相关系数检验可以判断自变量和因变量是高度相关的

# 通过回归参数检验与回归方程的检验，得到的一元线性回归方程为Y=-349.5+1.029*X


#2.2.5

#残差
y.res<-residuals(lm.ab)

#打印前6条数据
head(y.res)

#正态分布检验
shapiro.test(y.res)  # W接近1，p值大于0.05证明数据集符合正态分布

#画出残差散点图
plot(y.res)

#4种用于模型诊断的图形

#1.Residuals vs Fitted
#对残差和拟合值作图，横坐标是拟合值，纵坐标是残差。
#残差和拟合值之间，数据点均匀分布在y=0两侧，呈现出随机的分布，
#红⾊线呈现出⼀条平稳的曲线并没有明显的形状特征，说明残差数据表现⾮常好。

#2.Normal Q-Q
# 残差QQ图，⽤来描述残差是否符合正态分
# 布。图中的数据点按对⾓直线排列，趋于⼀条直线，并被对⾓直接穿过，
# 直观上符合正态分布。对于近似服从正态分布的标准化残差，应该有95%的样本点落在[-2，2]区间内。

#3.Scale-Location
# 对标准化残差平⽅根和拟合值作图，横坐标是拟合值，纵坐标是标准化后的残差平⽅根。
# 与残差和拟合值对⽐图的判断⽅法类似，数据随机分布，红⾊线呈现出⼀条平稳的曲线，⽆明显的形状特征。

#4.Residuals vs Leverage
# 对标准化残差和杠杆值作图，虚线表⽰的cooks距离等⾼线，
# 通常⽤Cook距离度量的回归影响点。本图中没有出现红⾊的等⾼线，则说明数据中没有特别影响回归结果的异常点。

#把4张图放在一起展示
par(mfrow=c(2,2))
plot(lm.ab)
#每幅图中都有一些异常值点被标出来。可以删掉异常值点对模型进行优化
# eg：27号和192两个点在多个图出现，从数据中去掉这两个点再进行显著性检验和残差分析

df[c(27,192),] #查看27和192
df2<-df[-c(27,192),] #新建数据集删除27和192

#回归建模和显著性检验
x2<-as.numeric(df2[,1])
y2<-as.numeric(df2[,2])
lm.ab2<-lm(y2~x2)
summary(lm.ab2)  
#此案例中本身拟合效果就很好，因此看不出显著提升


#2.2.6
newX<-data.frame(x=14040)
lm.pred<-predict(lm.ab,newX,interval = "prediction",level=0.95)
#预测结果
lm.pred  #当x0=14040时，在预测区间为0.95的置信水平时，y0的值为14102，预测区间为(14093.73，14110.44)

#通过图形表示
par(mfrow=c(1,1))
plot(y~x)
abline(lm.ab,col='red')
points(rep(newX$x,3),y=lm.pred,pch=19,col=c('red','blue','green'))
#其中红色点为y0的值，蓝色点为预测区间的最小值，绿色点为预测区间的最大值



















