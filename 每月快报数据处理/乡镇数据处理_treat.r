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
mobile<-read.table("20170405085658-20170206月度快报报表（统计表）.csv",sep=",",header=T,fileEncoding  = "GBK",skip=4)
mobile<<-data.table(mobile)

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
非高校市场<-c("城市市场","行业市场","家客市场 ","农村市场","商业市场","中小市场")
地市<<-c("全省","长春","吉林","延边","四平","通化","白城","辽源","松原","白山")
factor(mobile$地市,levels = 地市)
report_guding<-function(Col){
  县宽带用户数<-mobile[业务类型=="互联网业务" & 城乡标识=="农村",
                   .(宽带计费到达=sum(计费到达用户数,na.rm=T),
                           宽带新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带集团融合统计<-mobile[业务类型=="互联网业务" & 是否融合=="是" & 城乡标识=="农村",
                       .(宽带集团融合到达=sum(计费到达用户数,na.rm=T),
                                 宽带集团融合新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带本省融合统计<-mobile[城乡标识=="农村" & 业务类型=="互联网业务" & (融合分类=="手机+宽带" | 融合分类=="手机+宽带+电视" | 融合分类=="手机+电视"),
                       .(宽带本省融合到达=sum(计费到达用户数,na.rm=T),
                                 宽带本省融合新发展=sum(新发展用户数,na.rm=T)),by=Col]
  家庭单宽arpu<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 是否融合=="否"  & 城乡标识=="农村",
                       .(家庭单宽arpu=sum(出账收入,na.rm=T)/sum(计费到达用户数,na.rm=T),
                             家庭单宽收入=sum(出账收入,na.rm=T),
                             家庭单宽计费用户=sum(计费到达用户数,na.rm=T)),by=Col]
  移动A口径arpu_非融合<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  是否A口径出账=="是"  & 城乡标识=="农村" & 是否融合=="否",
                    .(移动A口径arpu_非融合=sum(出账收入,na.rm=T)/sum(集团出账用户数,na.rm=T),
                        移动a口径出账收入_非融合=sum(出账收入,na.rm=T),
                        移动a口径出账用户数_非融合=sum(集团出账用户数,na.rm=T)),by=Col]
  移动A口径arpu_融合<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  是否A口径出账=="是"  & 城乡标识=="农村" & 是否融合=="是",
                        .(移动A口径arpu_融合=sum(出账收入,na.rm=T)/sum(集团出账用户数,na.rm=T),
                            移动a口径出账收入_融合=sum(出账收入,na.rm=T),
                            移动a口径出账用户数_融合=sum(集团出账用户数,na.rm=T)),by=Col]
  
  移动A口径arpu<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  是否A口径出账=="是"  & 城乡标识=="农村",
                      .(移动A口径arpu=sum(出账收入,na.rm=T)/sum(集团出账用户数,na.rm=T),
                            移动a口径出账收入=sum(出账收入,na.rm=T),
                            移动a口径出账用户数=sum(集团出账用户数,na.rm=T)),by=Col]
  市区非高校发展<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  城乡标识=="城市" & 细分市场 %in% 非高校市场,
                  .(市区非高校发展量=sum(新发展用户数,na.rm=T)),by=Col]
  市区非高校政企发展<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  城乡标识=="城市" & 渠道视图_销售点一级=="直销渠道",
                  .(市区非高校政企发展量=sum(新发展用户数,na.rm=T)),by=Col]
  县非高校发展<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  城乡标识=="农村" & 细分市场 %in% 非高校市场,
                  .(县非高校发展量=sum(新发展用户数,na.rm=T)),by=Col]
  县非高校政企发展<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  城乡标识=="农村"  & 渠道视图_销售点一级=="直销渠道",
                    .(县非高校政企发展量=sum(新发展用户数,na.rm=T)),by=Col]
  县实体渠道移动非融合arpu<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  是否A口径出账=="是"  & 城乡标识=="农村" & 渠道视图_销售点一级!="直销渠道" & 是否融合=="否",
                    .(县实体渠道arpu=sum(出账收入,na.rm=T)/sum(集团出账用户数,na.rm=T),
                        县实体渠道收入=sum(出账收入,na.rm=T),
                        县实体渠道出账用户数=sum(集团出账用户数,na.rm=T)),by=Col]
  县政企移动非融合arpu<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") &  是否A口径出账=="是"  & 城乡标识=="农村" & 渠道视图_销售点一级=="直销渠道" & 是否融合=="否",
                         .(县政企渠道arpu=sum(出账收入,na.rm=T)/sum(集团出账用户数,na.rm=T),
                                县政企渠道收入=sum(出账收入,na.rm=T),
                                县政企渠道出账用户数=sum(集团出账用户数,na.rm=T)),by=Col]
  result<-merge(县宽带用户数,宽带集团融合统计,by=Col,all=T)
  result<-merge(result,宽带本省融合统计,by=Col,all=T)
  result<-merge(result,家庭单宽arpu,by=Col,all=T)
  result<-merge(result,移动A口径arpu_非融合,by=Col,all=T)
  result<-merge(result,移动A口径arpu_融合,by=Col,all=T)
  result<-merge(result,移动A口径arpu,by=Col,all=T)
  result<-merge(result,市区非高校发展,by=Col,all=T)
  result<-merge(result,市区非高校政企发展,by=Col,all=T)
  result<-merge(result,县非高校发展,by=Col,all=T)
  result<-merge(result,县非高校政企发展,by=Col,all=T)
  result<-merge(result,县实体渠道移动非融合arpu,by=Col,all=T)
  result<-merge(result,县政企移动非融合arpu,by=Col,all=T)
  order(result$地市)
  
  fname<-str_c("quick_guding",Col[1],".csv")
  write.table(result,fname,sep = ",",row.names = FALSE)
  
  return(result)
}

Col<-"地市"
y<-report_guding(Col)

