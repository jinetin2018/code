library(ggplot2)   #图形处理
library(dplyr)        #数据处理
library(data.table)
#library(tidyr)
library(lubridate)   #日期快速处理
library(readr)       #快速读取数据
library(magrittr)  #管道操作
library(zoo)    #时间格式预处理
library(xts)    #扩展时间序列,时间格式预处理
library(quantmod)   #金融数据读取与画图
library(formatR)   #formatR包是一个实用的包，提供了R代码格式化功能，自动设置空格、缩进、换行等代码格式，让代码看起来更友好。
library(readxl)
library("fortunes")  #fortunes库是一个R语言的语录集，截止到2012年4月，一共总结了316条R-help的留言，这些都是R语言智慧的精华。让R语言的后辈使用者，可以更了解R语言的本身，了解R的精神。
library(stringr)
#设置工作目录


#导入数据——dplyr处理
yitongka_df<-read.table("20170214103921-high卡-易通卡明细0215.csv",head=T,sep=",",fileEncoding="GBK")
yitongka_df<-data.table(yitongka_df)
high_df<-read.table("20170214101206-high卡订购流量包统计表.csv",head=T,sep=",",skip = 0,fileEncoding="GBK")
high_df<-data.table(high_df)
#保留带“加装”的流量包
加装包<-c("(49004822)19元High卡流量包(加装)","(49004823)39元High卡流量包(加装)","(49004824)79元High卡流量包(加装)")
high_df<-high_df[high_df$销售品 %in% 加装包,]
#套餐表与流量包表完全连接
high_liuliang <<- merge(yitongka_df,high_df,by=c("接入号","用户编号"))
#导出csv表

在网时长区间<-c("(0-1月]","(1-2月]","(2-3月]","(3-4月]","(4-5月]")
high_liuliang<- high_liuliang[在网时长分档 %in% 在网时长区间,]
high_result<-function(Col){
      high_N<-high_liuliang[,.(网上用户数=.N),by=Col]
      high_result_time<- high_liuliang[是否A口径出账=="是",.(出账用户数=.N,
                  应收=sum(优惠前应收,na.rm=T),
                  出账收入=sum(当月出账收入,na.rm=T),
                  赠款=sum(当月赠款使用金额,na.rm=T),
                  欠费=sum(欠费金额,na.rm=T),
                  流量=sum(增值流量不含WLAN,na.rm=T),
                  流量包内流量=sum(流量包内使用流量M,na.rm=T),
                  套餐溢出流量=sum(套餐溢出流量M,na.rm=T),
                  流量包溢出流量=sum(流量包溢出流量M,na.rm=T),
                  计费时长=sum(计费时长,na.rm=T),
                  本地时长=sum(本地计费时长,na.rm=T),
                  长途时长=sum(长途计费时长,na.rm=T),
                  漫游时长=sum(漫游计费时长,na.rm=T)),by=Col]
      high_result_time<-merge(high_result_time,high_N,by=Col,all=T)
      filename<-str_c("201701high",Col[1],".csv")
      write.table(high_result_time,filename,sep=",",row.names = FALSE)
      return(high_result_time)
}
Col <- c("地市.x")
high_result_time1<-high_result(Col)
Col <-c("在网时长分档","地市.x")
high_result_time2<-high_result(Col)
Col <-c("销售品","在网时长分档","地市.x","渠道视图_销售点一级")
high_result_time2<-high_result(Col)
