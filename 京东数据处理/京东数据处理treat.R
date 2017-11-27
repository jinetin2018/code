#------------------------
#------------------------
library(ggplot2)   #图形处理
library(dplyr)        #数据处理
library(data.table)
library(tidyr)
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
library(rJava)
library(Rwordseg)  #中文分词专用功能包
library(wordcloud)  #中文词频数统计包
library(htmltools)  #安装devtools包准备
library(devtools)
library(wordcloud2)  #中文词频数统计包
library(tm)   #中文词数据挖掘
library(jiebaR)
#input JData_User
JData_User <- read_csv("/Volumes/SD-64G/JD20170401/JData_User.csv")
#age频数
age_num<-table(JData_User$age)
age_prop<-prop.table(age_num)*100
age_prop<-data.frame(age_prop)




