#���ſ�����������������棬����������ʡ��ںϷ���ͳ�������û�����

library(ggplot2)   #ͼ�δ���
library(dplyr)        #���ݴ���
library(data.table)
#library(tidyr)
library(lubridate)   #���ڿ��ٴ���
library(readr)       #���ٶ�ȡ����
library(magrittr)  #�ܵ�����
library(zoo)    #ʱ���ʽԤ����
library(xts)    #��չʱ������,ʱ���ʽԤ����
library(TTR)
library(quantmod)   #�������ݶ�ȡ�뻭ͼ
library(formatR)   #formatR����һ��ʵ�õİ����ṩ��R�����ʽ�����ܣ��Զ����ÿո����������еȴ����ʽ���ô��뿴�������Ѻá�
library(readxl)
library("fortunes")  #fortunes����һ��R���Ե���¼������ֹ��2012��4�£�һ���ܽ���316��R-help�����ԣ���Щ����R�����ǻ۵ľ�������R���Եĺ�ʹ���ߣ����Ը��˽�R���Եı����˽�R�ľ���
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
mobile$class[mobile$����˿�����M %in% H_20M] <- "x=20M"
mobile$class[mobile$����˿�����M %in% H_21_49M] <- "20<x<50M"
mobile$class[mobile$����˿�����M %in% H_50_99M] <- "50<=x<100M"
mobile$class[mobile$����˿�����M %in% H_100_200M] <- "100<=x<200M"
mobile$class[mobile$����˿�����M %in% H_200_500M] <- "200<=x<500M"
mobile$class[mobile$����˿�����M %in% H_500_1000M] <- "500<=x<1000M"
mobile$class[mobile$����˿�����M %in% H_1000_] <- "x>=1000M"
mobile$class <- factor(mobile$class,labels = c("0<x<20M","x=20M","20<x<50M","50<=x<100M","100<=x<200M","200<=x<500M","500<=x<1000M","x>=1000M"),ordered = TRUE)

H_table <-mobile[ҵ������=="������ҵ��",.(�����û�=sum(�����û���,na.rm=T)),by=c("�ںϷ���","class")]
H_table1 <- reshape2::melt(H_table,value.name = "�����û�")
H_table2 <-reshape2::dcast(H_table1,�ںϷ���~class,fun.aggregate =sum,margins=TRUE)
write.table(H_table2,"clipboard",row.names = FALSE,sep = ",",quote = TRUE)
