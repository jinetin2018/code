source(file="tools.r",encoding="UTF-8")

#################################
# 下载数据
#################################
stock<-"IBM"
#download(stock,from='2010-01-01')
IBM<-read(stock)
head(IBM)

IBM<-IBM['2010/2014-06']

chartSeries(IBM)
chartSeries(IBM,TA = "addVo(); addSMA(); addEnvelope();addMACD(); addMomentum(); addROC()")

cdata<-IBM['2010/2012']$Close
title<-"Stock_IBM" #图片标题
sDate<-as.Date("2010-1-1") #开始日期
eDate<-as.Date("2012-1-1") #结束日期
#################################
# 均线图
#################################
ldata<-ma(cdata,c(5,20,60))  #选择滑动平均指标
drawLine(ldata,title,sDate,eDate) #画图

##################################################################
# 一条均线: 当股价上穿均线买入(红色)，下穿均线卖出(蓝色)
##################################################################
ldata<-ma(cdata,c(20))  #选择滑动平均指标
drawLine(ldata,title,sDate,eDate) #画图

pdata<-merge(ldata$ma20[which(ldata$Value-ldata$ma20>0)],ldata$ma20[which(ldata$Value-ldata$ma20<0)])
names(pdata)<-c("down","up")
pdata<-fortify(pdata,melt=TRUE)
pdata<-pdata[-which(is.na(pdata$Value)),]
drawPoint(ldata,pdata,title,sDate,eDate) #画图

tdata<-Signal(cdata,pdata)
tdata<-tdata[which(as.Date(row.names(tdata))<eDate),]

result1<-trade(tdata,100000)

result1$ticks
result1$rise
result1$fall

adata<-as.xts(result1$ticks[which(result1$ticks$op=='S'),]['cash'])
drawCash(ldata,adata,title,sDate,eDate)


##################################################################
# 二条均线: 当短均线ma5上穿长均线ma20买入(红色)，短均线ma5下穿长均线ma20卖出(蓝色)
##################################################################
ldata<-ma(cdata,c(5,20))  #选择滑动平均指标
drawLine(ldata,title,sDate,eDate) #画图

pdata<-merge(ldata$ma20[which(ldata$ma5-ldata$ma20>0)],ldata$ma20[which(ldata$ma5-ldata$ma20<0)])
names(pdata)<-c("down","up")
pdata<-fortify(pdata,melt=TRUE)
pdata<-pdata[-which(is.na(pdata$Value)),]
drawPoint(ldata,pdata,title,sDate,eDate) #画图

tdata<-Signal(cdata,pdata)
tdata<-tdata[which(as.Date(row.names(tdata))<eDate),]

result2<-trade(tdata,100000)

result2$ticks
result2$rise
result2$fall

adata<-as.xts(result2$ticks[which(result2$ticks$op=='S'),]['cash'])
drawCash(ldata,adata,title,sDate,eDate)


##################################################################
# 比较操作:一条均线 和 二条均线
##################################################################

#盈利
rise<-merge(as.xts(result1$rise[1]),as.xts(result2$rise[1]))
names(rise)<-c("plan1","plan2")

#亏损
fall<-merge(as.xts(result1$fall[1]),as.xts(result2$fall[1]))
names(fall)<-c("plan1","plan2")

#盈利区间
yrng <-range(ldata$Value)
plan1<-as.xts(result1$rise[c(1,2)])
plan1<-data.frame(start=as.Date(index(plan1)[which(plan1$op=='B')]),end=as.Date(index(plan1)[which(plan1$op=='S')]),plan='plan1')
plan2<-as.xts(result2$rise[c(1,2)])
plan2<-data.frame(start=as.Date(index(plan2)[which(plan2$op=='B')]),end=as.Date(index(plan2)[which(plan2$op=='S')]),plan='plan2')

plan<-rbind(plan1)
drawRange(ldata,plan,title,sDate,eDate) #画图plan1

plan<-rbind(plan1,plan2)
drawRange(ldata,plan,title,sDate,eDate) #画图plan2




