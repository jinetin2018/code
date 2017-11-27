# 先载入需要的扩展包
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(xts)

# 先获取S&P500的交易数据，然后根据其收盘价（由函数Cl()抽取）计算其5日均线值：
#getSymbols('^GSPC') #S&P500 OHLC data

setSymbolLookup(WK=list(name='000682.sz',src='yahoo'),auto.assign =TRUE)
getSymbols("WK")
close <- Ad(WK)  #每天最关键是收盘价格
mv10 <- SMA(close, 3)  #均线

# 策略是当收盘价大于5日均线，代表可以入市，取1，否则代表清仓，取0。（-1是代表卖空，不适用。）
sig <- ifelse(close < mv10, 1, 0)

# 使用Lag()将信号序列向“过去”推迟一天，代表将昨天的信号，应用到今天。
sig <- Lag(sig) #将该序列向“过去”延迟一天

# 计算收益序列
# discrete代表用离散方式计算当天收益率，即(Close-Close(-1))/Close(-1)
# continous代表用连续方式计算当天收益率，即ln(Close/Close(-1))
roc <- ROC(type='discrete',close) 
ret <- roc * sig

# 画出策略收益图
charts.PerformanceSummary(ret)