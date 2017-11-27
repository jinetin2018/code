library(dplyr)        #数据处理
library(data.table)
#library(ggplot2)   #图形处理
#library(tidyr)
#library(lubridate)   #日期快速处理
#library(readr)       #快速读取数据
#library(magrittr)  #管道操作
#library(zoo)    #时间格式预处理
#library(xts)    #扩展时间序列,时间格式预处理
#library(TTR)
#library(quantmod)   #金融数据读取与画图
#library(formatR)   #formatR包是一个实用的包，提供了R代码格式化功能，自动设置空格、缩进、换行等代码格式，让代码看起来更友好。
#library(readxl)
#library("fortunes")  #fortunes库是一个R语言的语录集，截止到2012年4月，一共总结了316条R-help的留言，这些都是R语言智慧的精华。让R语言的后辈使用者，可以更了解R语言的本身，了解R的精神。
library(stringr)

setwd("D:/R/quickreport/201709行业竞争数据分析")
Tele<-read.table("clipboard",sep = "\t",header = TRUE)
Tele$月份<-factor(Tele$月份)
Tele$单位<-NULL
Tele_melt<-melt(Tele)
Tele_cast<-dcast(Tele_melt,年度+月份+指标名称+variable~公司,sum)
#Tele_cast<-Tele_cast[c(-5,-7)]
write.table(Tele_cast,"clipboard",sep = ",",quote = TRUE,col.names = TRUE,row.names = FALSE)
