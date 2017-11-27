#————————————————————————————————————————————————————————————————
#每日发展量统计表
#————————————————————————————————————————————————————————————————

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

#——————————————————————————————————————————————————
#数据源1：在网统计表
#数据源2：不在网且出账用户
#两表合并
#——————————————————————————————————————————————————
mobile<-read.table("20170526营销日报（to张一博，孙博）.csv",sep=",",header=T,fileEncoding  = "GBK",skip=4)
mobile<<-data.table(mobile)
#tmp<-read.table("20170310营销日报（to张一博，孙博）.csv",sep=",",header=T,fileEncoding  = "GBK",skip=4)
#tmp<-data.table(tmp)
#mobile<-rbind(mobile,tmp)
#rm(tmp)

#mobile<-mobile[,全省:="全省"]
################################################################
#定义维度统计函数，移动指标统计
宽带20M<<-c("1000M","100M","1024M","1400M","2000M","200M","20M","24M","300M","30M","350M","40M","500M","50M","600M","80M")
宽带50M<<-c("1000M","100M","1024M","1400M","2000M","200M","300M","350M","500M","50M","600M","80M")
宽带100M<<-c("1000M","100M","1024M","1400M","2000M","200M","300M","350M","500M","600M")
宽带8M<-c("0.5M","1M","2M","3M","4M","6M","8M")
宽带20M以下<-c("0.5M","1M","2M","3M","4M","6M","8M","10M","12M","15M")
天翼高清产品<-c("天翼高清超值版","天翼高清简约版","天翼高清融合超值版","天翼高清融合简约版","天翼高清融合尊享版A","天翼高清融合尊享版B","天翼高清尊享版")
IPTV产品<-c("政企IPTV","IPTV")
智慧家庭产品<-c("智慧家庭(100M标准版)","智慧家庭(100M手机版)","智慧家庭(50M标准版)","智慧家庭(50M手机版)","智慧家庭产品")
#定义维度统计函数，固网及宽带指标统计
report_guding<-function(Col){
  #固网收入
  #固网收入<-mobile[(业务类型!="CDMA后付" & 业务类型!="CDMA智能预付"),.(出账收入=sum(出账收入,na.rm=T)),by=Col]
  #宽带业务统计：计费、新发展
  mobile_dev<-mobile[业务类型=="CDMA智能预付" | 业务类型=="CDMA后付",
                         .(移动新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  宽带业务<-mobile[业务类型=="互联网业务",
                   .(宽带计费到达=sum(计费到达用户数,na.rm=T),
                           宽带新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  天翼高清用户数<-mobile[(产品 %in% 天翼高清产品) | (产品 %in% IPTV产品),
                  .(天翼高清到达= sum(计费到达用户数,na.rm=T),
                              天翼高清新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  老itv用户数<-mobile[产品 =="高清影视业务",
                  .(老itv到达= sum(计费到达用户数,na.rm=T),
                          老itv新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]  
  #宽带——家庭宽带：计费、新发展
  家庭宽带业务<-mobile[(业务类型=="互联网业务") & (细分市场=="家客市场 "),
                 .(家庭宽带计费=sum(计费到达用户数,na.rm=T),
                         家庭宽带新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  高校宽带业务<-mobile[业务类型=="互联网业务" & 细分市场=="高校市场",
                 .(高校宽带计费=sum(计费到达用户数,na.rm=T),
                         高校宽带新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  家庭智慧家庭<-mobile[(业务类型=="互联网业务") & (细分市场=="家客市场 ") & (融合类型 == "智慧家庭"),
                 .(智慧家庭计费=sum(计费到达用户数,na.rm=T),
                         智慧家庭新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  #宽带——速率统计：计费、新发展、20M、50M、100M
  宽带20M及以上<-mobile[业务类型=="互联网业务" & 宽带带宽 %in% 宽带20M ,
                       .(宽带20M到达=sum(计费到达用户数,na.rm=T),
                           宽带20M新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  宽带50M及以上<-mobile[业务类型=="互联网业务" & 宽带带宽 %in% 宽带50M ,
                       .(宽带50M到达=sum(计费到达用户数,na.rm=T),
                           宽带50M新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  宽带100M及以上<-mobile[业务类型=="互联网业务" & 宽带带宽 %in% 宽带100M,
                        .(宽带100M到达=sum(计费到达用户数,na.rm=T),
                            宽带100M新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  FTTB_LAN50M及以上<-mobile[业务类型=="互联网业务" & (宽带带宽 %in% 宽带50M & 宽带接入方式=="FTTB_LAN"),
                             .(FTTB_LAN50M及以上计费=sum(计费到达用户数,na.rm=T),
                               FTTB_LAN50M及以上新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  #宽带——FTTH、FTTB+LAN、速率50M以上统计
  家庭宽带FTTH光宽<-mobile[业务类型=="互联网业务" & 宽带接入方式=="FTTH/O" & 细分市场=="家客市场 ",
                     .(家庭宽带光宽到达=sum(计费到达用户数,na.rm=T),
                             家庭宽带光宽新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  #宽带——融合统计，带手机融合，不带手机融合
  宽带集团融合统计<-mobile[业务类型=="互联网业务" & 是否融合=="是",
                       .(宽带集团融合到达=sum(计费到达用户数,na.rm=T),
                                 宽带集团融合新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  宽带本省融合统计<-mobile[业务类型=="互联网业务" & (融合分类=="手机+宽带" | 融合分类=="手机+宽带+电视" | 融合分类=="手机+电视"),
                       .(宽带本省融合到达=sum(计费到达用户数,na.rm=T),
                                 宽带本省融合新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  宽带8M及以下<-mobile[业务类型=="互联网业务" & 宽带带宽 %in% 宽带8M,
                      .(宽带8M及以下到达=sum(计费到达用户数,na.rm=T),
                          宽带8M及以下新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  家庭宽带8M<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 宽带带宽 %in% 宽带8M, 
                     .(家庭宽带8M及以下到达=sum(计费到达用户数,na.rm=T),
                           家庭宽带8M及以下新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  家庭宽带20M以下<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 宽带带宽 %in% 宽带20M以下,
                        .(家庭宽带20M以下到达=sum(计费到达用户数,na.rm=T),
                              家庭宽带20M以下新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  县宽带业务<-mobile[业务类型=="互联网业务" & 城乡标识=="农村",
                 .(县宽带计费=sum(计费到达用户数,na.rm=T),
                         县宽带新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col] 
  县本省融合统计<-mobile[细分市场=="家客市场 " &   业务类型=="互联网业务" & 城乡标识=="农村" & (融合分类=="手机+宽带" | 融合分类=="手机+宽带+电视" | 融合分类=="手机+电视"),
                       .(县家庭宽带本省融合到达=sum(计费到达用户数,na.rm=T),
                                 县家庭宽带本省融合新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  县天翼高清用户数<-mobile[城乡标识=="农村" & (产品 %in% 天翼高清产品 | 产品 %in% IPTV产品),
                  .(县天翼高清到达= sum(计费到达用户数,na.rm=T),
                          县天翼高清新发展=sum(当月累计新发展用户数,na.rm=T)),by=Col]
  result<-merge(mobile_dev,宽带业务,by=Col,all=T)
  result<-merge(result,天翼高清用户数,by=Col,all=T)
  result<-merge(result,家庭宽带业务,by=Col,all=T)
  result<-merge(result,高校宽带业务,by=Col,all=T)
  result<-merge(result,家庭智慧家庭,by=Col,all=T)
  result<-merge(result,宽带20M及以上,by=Col,all=T)
  result<-merge(result,宽带50M及以上,by=Col,all=T)
  result<-merge(result,宽带100M及以上,by=Col,all=T)
  result<-merge(result,家庭宽带FTTH光宽,by=Col,all=T)
  result<-merge(result,宽带集团融合统计,by=Col,all=T)
  result<-merge(result,宽带本省融合统计,by=Col,all=T)
  result<-merge(result,家庭宽带20M以下,by=Col,all=T)
  result<-merge(result,县宽带业务,by=Col,all=T)
  result<-merge(result,县本省融合统计,by=Col,all=T) 
  result<-merge(result,县天翼高清用户数,by=Col,all=T)
  result<-merge(result,老itv用户数,by=Col,all=T)  
#写文件  
  fname<-str_c("quick_guding",Col[1],".csv")
  write.table(result,fname,sep = ",",row.names = FALSE)
  
  return(result)
}
#————————————————————————————————————————————————————————————————

Col<-c("地市")
y<-report_guding(Col)

Col<-c("细分市场")
y<-report_guding(Col)

Col<-c("城乡标识","经分区县")
y<-report_guding(Col)

#------------------------------------------------
#x<-melt(mobile,na.rm = T)
#y<-dcast(x,地市~宽带端口速率M,sum,subset=.(variable=="计费到达用户数" & 业务类型=="互联网业务"))
#write.table(y,"y.csv",sep = ",",row.names = FALSE)
