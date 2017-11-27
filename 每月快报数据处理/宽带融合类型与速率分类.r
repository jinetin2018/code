#集团宽带竞争分析材料里面，宽带按照速率、融合分类统计网上用户数。

library(ggplot2)   #图形处理
library(dplyr)        #数据处理
library(data.table)
#library(tidyr)
library(lubridate)   #日期快速处理
library(readr)       #快速读取数据
library(magrittr)  #管道操作
library(zoo)    #时间格式预处理
library(xts)    #扩展时间序列,时间格式预处理
library(TTR)
library(quantmod)   #金融数据读取与画图
library(formatR)   #formatR包是一个实用的包，提供了R代码格式化功能，自动设置空格、缩进、换行等代码格式，让代码看起来更友好。
library(readxl)
library("fortunes")  #fortunes库是一个R语言的语录集，截止到2012年4月，一共总结了316条R-help的留言，这些都是R语言智慧的精华。让R语言的后辈使用者，可以更了解R语言的本身，了解R的精神。
library(stringr)
library(reshape2)

setwd("D:/R/quickreport/month_report")

H_0_19M      <-c("0.5M","1M","2M","3M","4M","6M","8M","10M","12M","15M")
H_20M        <-c("20M")
H_21_49M    <-c("24M","30M","40M")
H_50_99M    <-c("50M","80M")
H_100_200M   <-c("100M")
H_200_500M   <-c("200M","300M","350M")
H_500_1000M  <-c("500M","600M")
H_1000_      <-c("1000M","1024M","1400M","2000M")

mobile<-read.table("201708_2.csv",sep=",",encoding="GBK",skip=4,header=TRUE)
mobile<-data.table(mobile)
mobile$class<- "0<x<20M"
mobile$class[mobile$宽带端口速率M %in% H_20M] <- "x=20M"
mobile$class[mobile$宽带端口速率M %in% H_21_49M] <- "20<x<50M"
mobile$class[mobile$宽带端口速率M %in% H_50_99M] <- "50<=x<100M"
mobile$class[mobile$宽带端口速率M %in% H_100_200M] <- "100<=x<200M"
mobile$class[mobile$宽带端口速率M %in% H_200_500M] <- "200<=x<500M"
mobile$class[mobile$宽带端口速率M %in% H_500_1000M] <- "500<=x<1000M"
mobile$class[mobile$宽带端口速率M %in% H_1000_] <- "x>=1000M"
mobile$class <- factor(mobile$class,labels = c("0<x<20M","x=20M","20<x<50M","50<=x<100M","100<=x<200M","200<=x<500M","500<=x<1000M","x>=1000M"),ordered = TRUE)

H_table <-mobile[业务类型=="互联网业务",.(网上用户=sum(网上用户数,na.rm=T)),by=c("融合分类","class")]
H_table1 <- reshape2::melt(H_table,value.name = "网上用户")
H_table2 <-reshape2::dcast(H_table1,融合分类~class,fun.aggregate =sum,margins=TRUE)
write.table(H_table2,"clipboard",row.names = FALSE,sep = ",",quote = TRUE)
