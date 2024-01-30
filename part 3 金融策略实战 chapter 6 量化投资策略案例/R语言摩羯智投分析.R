setwd("D:/significant_data/R/b站分享/R语言量化投资/part 3 金融策略实战 chapter 6 量化投资策略案例")
dfa<-read.csv(file="a.csv")
dfb<-read.csv(file="b.csv",encoding="utf-8",fileEncoding = "utf-8")
dfc<-read.csv(file="c.csv")

names(dfa)<-c("term","risk","ret","vol","fixed","cash","stock","alter","gains","loss")
names(dfb)<-c("name","code","create","type","first2014","first2015","first2016","last")
names(dfc)<-c("term","risk","type","code","weight")

head(dfa,10) #a.csv前10行
head(dfb,10) #b.csv前10行
head(dfc,20) #b.csv前20行

# 这里还需要特别说明的是，由于应用的数据可能会动态地发生变化，文中采集的是2016年12月8日“摩羯智投”应用中的数据。



#数据建模分析

# 1.分析一：只有2个输入项
# 由于只有2个输入项，即大致投资期限和风险承受能力。大致投资期限有3个选项，
# 风险承受能力有10个选项，那么实际的组合个数就是3×10=30个。
# 对于只有30个组合的输入项来说，并不能完全实现个性化，当有31个用户使用产品时，就会有2个人购买的组合是重复的。




# 2.分析二：只有17支标的基金
# 我们对30个组合进行配置尝试后，发现详细持仓方案中，只有17支基金，配置比例各不相同。
# 标的过少，可能导致⻛险不能足够地分散化，遇到极端行情会导致大的回撤。17只基金分别是：
paste(dfb$name,"(",dfb$code,")",sep="")




# 3.分析三：相关性分析
# 直接利用a.csv的数据集，查看输入项和输出项的相关性，发现相关关系。

#画出配对示意图
pairs(dfa)

# ·term列：和其他列的散点图完全呈现离散的分布，说明term列与其他列没有相关性。
# ·risk列：除了和alter列没有线性关系，和其他列呈现明显的线性关系。

#在图中再添加一些元素，如相关系数、拟合曲线、分布图等，重新画出相关性图（优化）
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(dfa, histogram=TRUE, pch="+")

# ·risk列：与模拟历史年化收益（ret）、模拟历史年化波动率（vol）、
# 拟历史收益（gains）呈现极度正相关，输出项的数字完全受risk值影响。

# ·risk列：与固定收益（fixed）和现金及货币（cash）极度负相关；与股票类（stock）极度正相关；
# 与另类及其他（alter）负相关。这种情况与资产的⻛险收益属性是匹配的。

# ·vol列：与亏损（loss）呈100%的线性相关性。

# ·ret列：与拟历史收益（gains）呈100%的线性相关性，这里可以获得公式：gains=10000*ret。




# 4.分析四：线性回归
# 通过相关性的检查，我们可以发现risk与很多列都是极度相关的。
# 那么我们可以用线性回归的方法把risk与有相关性的列的参数估计出来。

#由于vol和loss呈100%的线性相关性，以vol为x，loss为y，构建一元线性回归方程
lv<-lm(loss~vol,data=dfa)
summary(lv)
# 进行线性回归的统计检查：T检查、F检查都非常显著，同时R-squared为0.9963，具有极度相关性

#画出散点图和拟合曲线
plot(loss~vol,data=dfa)
abline(lv)
# 从图中看到，拟合的效果非常好，可以整理出公式：loss=-447.514+149.109*vol


#另外，由于risk决定vol，再让我们计算一下risk和loss的关系，以risk为x，loss为y，构建一元线性回归方程
lr<-lm(loss~risk,data=dfa)
summary(lr)
# t检查和F检查都非常显著；R-squared也比较高

#下面进行残差检查
par(mfrow=c(2,2))
plot(lr)
# 发现30号点偏离比较大，可能是离群值

#把30号点去掉，再进行显著性检查和残差分析
dfa2<-dfa[-30,]
lr2<-lm(loss~risk,data=dfa2)
summary(lr2)
# 在去掉30号点后，R-squared为0.9091，比之前的0.8947有所提升

plot(lr2)
# 在新残差图中，我们看到没有明显的离群值点，所以去掉30号点是符合统计提升标准的




# 5.分析五：关于30号点的金融思考
# 从数据中，我们发现30号点的最大亏损已经超过了收益，也就是说你可能承担了过大的风险，但是没有获得风险所给你带来的收益。
# 按照资本资产定价模型的理解，我们投资组合的收益来自两个部分，无⻛险收益和⻛险收益。
# 无⻛险收益可以用现金或货币类的基金获得，⻛险收益主要来自股票基金、债券基金、另类投资基金。
# 从直观上理解，⻛险收益比至少是1∶1，即损失100元时，要获得100元⻛险补偿。
# 对于私募业务来说，投资人的要求可能会更高，比如⻛险∶收益=1:2。




# 6.分析六：通过标的基金计算收益率
# 在“摩羯智投”的应用中，我们可以获得各个基金的配置比例，基金净值的数据又可以在公开市场中获得，
# 所以对于预期收益率，我们也可以自己计算一下，看看与“摩羯智投”提供的结果是否是一致的。
# 接下来，就利用到上文介绍的数据集，b.csv和c.csv

#分别计算2014，2015，2016的收益率
dfb$ret2014<-(dfb$first2015-dfb$first2014)/dfb$first2014
dfb$ret2015<-(dfb$first2016-dfb$first2015)/dfb$first2015
dfb$ret2016<-(dfb$last-dfb$first2016)/dfb$first2016

#把非法值赋值为0
dfb$ret2014[c(which(is.na(dfb$ret2014)),which(is.infinite(dfb$ret2014)))]<-0
dfb$ret2015[c(which(is.na(dfb$ret2015)),which(is.infinite(dfb$ret2015)))]<-0
dfb$ret2016[c(which(is.na(dfb$ret2016)),which(is.infinite(dfb$ret2016)))]<-0

#打印前6条
head(dfb)

#由于基金中，招商现金增值A（OF217004）为现金类基金，所以收益率需要直接取年化收益率，而不是按上面的计算方法。
#从wind查知2014年招商现金增值A的平均年化收益率为4.52%，2015年平均年化收益率为3.6%，2016年平均年化收益率为2.3%
dfb[which(dfb$code=='OF217004'),]$ret2014<-0.0452
dfb[which(dfb$code=='OF217004'),]$ret2015<-0.036
dfb[which(dfb$code=='OF217004'),]$ret2016<-0.0237

#再加载c.csv基金的详细配置方案
head(dfc)

#生成plan1的配置方案,当term=1且risk=1时
install.packages("plyr")
library(plyr)

#只保留term=1，risk=1时的数据
plan1<-dfc[dfc$term==1 & dfc$risk==1,]

#合并plan1数据集和dfb数据集
plan1m<-merge(plan1[,c("term","risk","code","type","weight")],dfb[,c("code","ret2014","ret2015","ret2016")],by="code")

#按分配比例计算收益率
plan1m$ret2014w<-plan1m$weight*plan1m$ret2014
plan1m$ret2015w<-plan1m$weight*plan1m$ret2015
plan1m$ret2016w<-plan1m$weight*plan1m$ret2016

#plan1的各基金分别在2014年、2015年、2016年贡献的收益率
plan1m


#将数据进行合并，分别计算plan1方案的收益率和plan1方案不同资产的收益率贡献

#plan1方案的收益率
plan1r<-ddply(plan1m,.(term,risk),summarise,ret2016=sum(ret2016w),ret2015=sum(ret2015w),ret2014=sum(ret2014w))
plan1r

#计算3年的累积收益率曲线
plan1r$cumret<-sum(c(plan1r$ret2016,plan1r$ret2015,plan1r$ret2014))
plan1r

#plan1方案不同资产的收益率贡献
plan1rm<-ddply(plan1m,.(term,risk,type),summarise,ret2016=sum(ret2016w),ret2015=sum(ret2015w),ret2014=sum(ret2014w))
plan1rm
#（回ppt）



#接下来，我们把30种组合的收益率都计算出来。再与“摩羯智投”给出的收益率进行比较
planAll<-merge(dfc[,c("term","risk","code","type","weight")],dfb[,c("code","ret2014","ret2015","ret2016")],by="code")
planAll$ret2014w<-planAll$weight*planAll$ret2014
planAll$ret2015w<-planAll$weight*planAll$ret2015
planAll$ret2016w<-planAll$weight*planAll$ret2016
planAll

resutltA1<-ddply(planAll,.(term,risk),summarise,ret2016=sum(ret2016w),ret2015=sum(ret2015w),ret2014=sum(ret2014w))
resutltA1$mean<-rowMeans(resutltA1[,c('ret2016','ret2015','ret2014')])
resutltA1

resutltA1M<-merge(dfa,resutltA1,by=c("term","risk"))
resutltA1M<-resutltA1M[,c("term","risk","ret","vol","ret2016","ret2015","ret2014","mean")]
resutltA1M$ret2016<-resutltA1M$ret2016*100
resutltA1M$ret2015<-resutltA1M$ret2015*100
resutltA1M$ret2014<-resutltA1M$ret2014*100
resutltA1M$mean<-resutltA1M$mean*100
resutltA1M

# ret列为“摩羯智投”界面上采集的数据；
# ret2016、ret2015、ret2014分别为我们根据基金的公开市场的数据，计算出来的百分比结果；
# mean为ret2016、ret2015、ret2014的算数平均数
# 从数据上看，ret列和mean列有部分值接近。再做一次相关性分析
chart.Correlation(resutltA1M[,-c(1,2,4)], histogram=TRUE, pch="+")

# 结果解读：
# ret与2016年的收益率是线性相关的，而mean与2014年的收益率是线性相关的，所以，ret和mean没有关系，
# “摩羯智投”对于收益率的预期，对于近1年的组合收益可能有更大的权重分配。
# 所以，我们并不能通过已知的数据计算出“摩羯智投”的预期收益和净值曲线，这些会涉及它的背景算法。




