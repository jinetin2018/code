# 处理每月快报数据

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
#library(openair)   #openair空气质量监控

# 1、读取数据 --------------------------------------------------------------------
setwd("D:/R/quickreport/month_report")

#mobile<-read.table("201802月度快报报表（统计表）.csv",sep=",",encoding="GBK",skip=4,header=TRUE)
mobile <- fread("201802月度快报报表（统计表）.csv",sep=",",skip=4,header=TRUE,colClasses=c(rep("factor",63)))
mobile[,45:63] <- lapply(mobile[,45:63], as.numeric)
#mobile<-data.table(mobile)

#names(mobile)[12] <- "部门一级"
# ~~~~定义维度 --------------------------------------------------------------------


#定义维度统计函数，移动指标统计
宽带20M<<-c("1000M","100M","1024M","1400M","2000M","200M","20M","24M","300M","30M","350M","40M","500M","50M","600M","80M")
宽带50M<<-c("1000M","100M","1024M","1400M","2000M","200M","300M","350M","500M","50M","600M","80M")
宽带100M<<-c("1000M","100M","1024M","1400M","2000M","200M","300M","350M","500M","600M")
宽带8M<-c("0.5M","1M","2M","3M","4M","6M","8M")
宽带200M<<-c("1000M","1024M","1400M","2000M","200M","300M","350M","500M","600M")
宽带20M以下<-c("0.5M","1M","2M","3M","4M","6M","8M","10M","12M","15M")
天翼高清产品<-c("天翼高清超值版","天翼高清简约版","天翼高清融合超值版","天翼高清融合简约版","天翼高清融合尊享版A","天翼高清融合尊享版B","天翼高清尊享版")
IPTV产品<-c("政企IPTV","IPTV")
智慧家庭产品<-c("智慧家庭(100M标准版)","智慧家庭(100M手机版)","智慧家庭(50M标准版)","智慧家庭(50M手机版)","智慧家庭产品")


# ~~~~定义函数report_mobile -------------------------------------------------------
#  地市、细分市场、部门一级、销售点一级、城乡标识作为有排序的factor
mobile$地市<-factor(mobile$地市,order = TRUE,levels = c("长春","吉林","延边","四平","通化","白城","辽源","松原","白山"))
mobile$细分市场<-factor(mobile$细分市场,order = TRUE,levels = c("行业市场","商业市场","高校市场","中小市场","家客市场 ","城市市场","农村市场"))
mobile$部门一级<-factor(mobile$部门一级,order = TRUE,levels = c("政企","实体","电子","未知"))
mobile$渠道视图_销售点一级<-factor(mobile$渠道视图_销售点一级,order = TRUE,levels = c("直销渠道","实体渠道","电子渠道","未知"))
mobile$城乡标识<-factor(mobile$城乡标识,order = TRUE,levels = c("城市","农村"))
mobile$用户类型<-factor(mobile$用户类型,order=TRUE,levels = c("普通用户","VPN客户","大客户","公免用户","测试用户"))
mobile$ARPU分档 <- factor(mobile$ARPU分档,order=TRUE,levels = c("(0-10元]","(10-20元]","(20-30元]","(30-40元]","(40-50元]","(50-60元]","(60-70元]","(70-80元]","(80-100元]","(100-150元]","(150-200元]","(200-500元]","(500-1000元]","1000元以上"))


mobile$区县分类 <- mobile$经分区县
mobile[地市 == "长春" & 城乡标识 == "城市"]$区县分类 <- "长春市区"
mobile[地市 == "吉林" & 城乡标识 == "城市"]$区县分类 <- "吉林市区"
mobile[地市 == "延边" & 城乡标识 == "城市"]$区县分类 <- "延边市区"
mobile[地市 == "四平" & 城乡标识 == "城市"]$区县分类 <- "四平市区"
mobile[地市 == "通化" & 城乡标识 == "城市"]$区县分类 <- "通化市区"
mobile[地市 == "白城" & 城乡标识 == "城市"]$区县分类 <- "白城市区"
mobile[地市 == "辽源" & 城乡标识 == "城市"]$区县分类 <- "辽源市区"
mobile[地市 == "松原" & 城乡标识 == "城市"]$区县分类 <- "松原市区"
mobile[地市 == "白山" & 城乡标识 == "城市"]$区县分类 <- "白山市区"

mobile$区县分类 <- factor(mobile$区县分类,order=TRUE,
                      levels = c("长春市区","德惠市","九台市","农安县","双阳区","榆树市",
                                 "吉林市区","磐石市","舒兰市","永吉县","桦甸市","蛟河市",
                                 "延边市区","敦化市","和龙市","龙井市","图们","汪清县","安图县","珲春市",
                                 "四平市区","梨树县","双辽市","公主岭市","伊通满族自治县",
                                 "通化市区","集安市","柳河县","梅河口市","辉南县","通化县",
                                 "白城市区","白城.大安","白城.通榆","白城.镇赉","白城.洮南",
                                 "辽源市区","东辽县","东丰县",
                                 "松原市区","扶余县","乾安县","长岭县",
                                 "白山市区","江源区","临江市","抚松县","靖宇县","长白朝鲜族自治县"))

report_mobile<-function(Col){
  移动经分1<-mobile[业务类型=="CDMA后付" | 业务类型=="CDMA智能预付",
                    .(移动新增=sum(新发展用户数,na.rm=T),
                          经分出账数=sum(经分出账用户数,na.rm=T),
                          应收含赠款=sum(优惠前应收收入,na.rm=T),
                          出账收入=sum(出账收入,na.rm=T),
                          欠费=sum(当月欠费金额,na.rm=T),
                          赠款余额=sum(赠款_总赠款金额,na.rm=T),
                          赠款销账金额=sum(赠款_当月划拔赠款销帐金额,na.rm=T),
                          新增赠款=sum(赠款_当月新增赠款金额,na.rm=T),
                          流失用户数=sum(出账流失用户数,na.rm=T),
                          回归用户数=sum(出账回归用户数,na.rm=T),
                          计费时长=sum(计费时长,na.rm=T),
                          流量=sum(增值流量不含WLAN)),by=Col]
  移动A<-mobile[是否A口径出账=="是" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付"),
                .(a出账用户=sum(经分出账用户数,na.rm=T),
                  a出账收入=sum(出账收入,na.rm=T),
                  a流量=sum(增值流量不含WLAN,na.rm=T),
                  a欠费=sum(当月欠费金额,na.rm=T),
                  a回归用户数=sum(A口径出账回归用户数,na.rm=T),
                  a流失用户数=sum(A口径出账流失用户数,na.rm=T),
                  a赠款销账金额=sum(优惠前应收收入-出账收入,na.rm=T)),by=Col]
  移动经分2<-mobile[是否三合一用户=="是" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付"),
                       .(新发展三合一=sum(新发展用户数,na.rm=T),
                               三合一出账数=sum(集团出账用户数,na.rm=T),
                               三合一用户流量=sum(增值流量不含WLAN,na.rm=T)),by=Col]
  终端4G出账指标<-mobile[是否A口径出账=="是" & 注册_终端类型=="4G手机" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付"),
                     .(新发展4G终端=sum(新发展用户数,na.rm=T),
                          终端4Ga口径出账数=sum(集团出账用户数,na.rm=T),
                          终端4G用户流量=sum(增值流量不含WLAN,na.rm=T)),by=Col]
  移动卡功能<-mobile[(业务类型 %in% c("CDMA后付","CDMA智能预付")) & 网络制式=="4G" & (卡类型 %in% c("集团4G卡","省内4G卡")) ,
                .(新发展4g卡功能=sum(新发展用户数,na.rm=T),
                     a口径卡功能出账=sum(集团出账用户数,na.rm=T),
                     卡功能总流量=sum(增值流量不含WLAN)),by=Col]
  手机上网流量<-mobile[(业务类型 %in% c("CDMA后付","CDMA智能预付")) & 是否无线宽带用户=="否",.(手机上网流量=sum(增值流量不含WLAN)),by=Col]
  手机上网4G流量<-mobile[(业务类型 %in% c("CDMA后付","CDMA智能预付")) & 是否无线宽带用户=="否" & 网络制式=="4G",.(手机4G上网流量=sum(增值流量不含WLAN)),by=Col]
  无线宽带流量<-mobile[(业务类型 %in% c("CDMA后付","CDMA智能预付")) & 是否无线宽带用户=="是",.(无线宽带流量=sum(增值流量不含WLAN)),by=Col]
  补贴<-c("购机折扣(包打)","购机折扣(标准)","集团合约","省内合约")
  补贴及购机折扣<-mobile[政策类型 %in% 补贴 & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付"),
                      .(新发展补贴及购机折扣=sum(新发展用户数,na.rm=T),
                                  补贴及购机折扣出账数=sum(集团出账用户数,na.rm=T)),by=Col]
  移动集团机卡匹配<-mobile[是否集团机卡匹配=="是" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付"),
                           .(集团新发展机卡用户数=sum(新发展用户数,na.rm=T),
                                       集团a口径机卡出账数=sum(集团出账用户数,na.rm=T),
                                       集团a口径机卡用户流量=sum(增值流量不含WLAN,na.rm=T)),by=Col]
  mobile_存量a口径<-mobile[是否A口径出账=="是" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 用户存增量标识=="存量",
                         .(存量a口径出账数=sum(集团出账用户数,na.rm=T)),by=Col]
  ARPU_融合A口径<-mobile[是否A口径出账=="是" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 是否融合=="是",
                       .(融合a口径出账数=sum(集团出账用户数,na.rm=T),
                           融合a出账收入=sum(出账收入,na.rm=T)),by=Col]
  ARPU_非融合a口径出账<-mobile[是否A口径出账=="是" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 是否融合=="否",
                          .(非融合a口径出账数=sum(集团出账用户数,na.rm=T),
                               非融合a出账收入=sum(出账收入,na.rm=T)),by=Col] 
  移动网上用户<-mobile[业务类型=="CDMA后付" | 业务类型=="CDMA智能预付",
                     .(网上用户=sum(网上用户数,na.rm=T)),by=Col]
  移动新发展出账率 <- mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 是否新发展 == "是",
                     .(移动新增=sum(新发展用户数,na.rm=T),新发展且出账=sum(集团出账用户数,na.rm=T),
                           新发展出账率=sum(集团出账用户数,na.rm=T)/sum(新发展用户数,na.rm=T) ),by = Col ]
  移动不限量新发展出账率 <- mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 是否新发展 == "是" & 是否不限量套餐 == "是",
                        .(不限量新增=sum(新发展用户数,na.rm=T),不限量新增出账=sum(集团出账用户数,na.rm=T),
                               不限量新发展出账率=sum(集团出账用户数,na.rm=T)/sum(新发展用户数,na.rm=T) ),by = Col ] 
  移动副卡新发展出账率 <- mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 是否新发展 == "是" & 乐享主副卡 == "副卡",
                       .(副卡新增=sum(新发展用户数,na.rm=T),副卡新增出账=sum(集团出账用户数,na.rm=T),
                             副卡新发展出账率=sum(集团出账用户数,na.rm=T)/sum(新发展用户数,na.rm=T) ),by = Col ]   
  增量出账用户 <-mobile[是否A口径出账=="是" & (业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 用户存增量标识=="增量",
                    .(增量a口径出账数=sum(集团出账用户数,na.rm=T),
                        增量出账收入 = sum(出账收入,na.rm=T)),by=Col]
  年累计发展 <- mobile[业务类型=="CDMA后付" | 业务类型=="CDMA智能预付",
                      .(移动年累计新增=sum(本年累计新发展用户数,na.rm=T)),by=Col]
  result<-merge(移动经分1,移动A,by=Col,all=T)
  result<-merge(result,移动经分2,by=Col,all=T)
  result<-merge(result,移动卡功能,by=Col,all=T)
  result<-merge(result,手机上网流量,by=Col,all=T)
  result<-merge(result,手机上网4G流量,by=Col,all=T)
  result<-merge(result,终端4G出账指标,by=Col,all=T)
  result<-merge(result,无线宽带流量,by=Col,all=T)
  result<-merge(result,补贴及购机折扣,by=Col,all=T)
  result<-merge(result,移动集团机卡匹配,by=Col,all=T)
  result<-merge(result,mobile_存量a口径,by=Col,all=T)
  result<-merge(result,ARPU_融合A口径,by=Col,all=T)
  result<-merge(result,ARPU_非融合a口径出账,by=Col,all=T)
  result<-merge(result,移动网上用户,by=Col,all=T)
  result<-merge(result,移动新发展出账率,by=Col,all=T)
  result<-merge(result,移动不限量新发展出账率,by=Col,all=T)
  result<-merge(result,移动副卡新发展出账率,by=Col,all=T)
  result<-merge(result,增量出账用户,by=Col,all=T)
  result<-merge(result,年累计发展,by=Col,all=T)
  
  fname<-str_c("quick_mobile",Col[1],".csv")
  write.table(result,fname,sep = ",",row.names = FALSE)
  
  return(result)
}

#  ~~~~定义函数report_guding ------------------------------------------------------


report_guding<-function(Col){
  #固网收入
  固网收入<-mobile[(业务类型!="CDMA后付" & 业务类型!="CDMA智能预付"),.(出账收入=sum(出账收入,na.rm=T)),by=Col]
  #宽带业务统计：计费、新发展
  宽带业务<-mobile[业务类型=="互联网业务",
                   .(宽带计费到达=sum(计费到达用户数,na.rm=T),
                           宽带新发展=sum(新发展用户数,na.rm=T)),by=Col]
  #宽带——家庭宽带：计费、新发展
  家庭宽带业务<-mobile[(业务类型=="互联网业务") & (细分市场=="家客市场 "),
                 .(家庭宽带计费=sum(计费到达用户数,na.rm=T),
                         家庭宽带新发展=sum(新发展用户数,na.rm=T)),by=Col]
  #宽带——速率统计：计费、新发展、20M、50M、100M
  宽带20M及以上<-mobile[业务类型=="互联网业务" & 宽带端口速率M %in% 宽带20M,
                       .(宽带20M到达=sum(计费到达用户数,na.rm=T),
                           宽带20M新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带50M及以上<-mobile[业务类型=="互联网业务" & 宽带端口速率M %in% 宽带50M,
                       .(宽带50M到达=sum(计费到达用户数,na.rm=T),
                           宽带50M新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带100M及以上<-mobile[业务类型=="互联网业务" & 宽带端口速率M %in% 宽带100M,
                        .(宽带100M到达=sum(计费到达用户数,na.rm=T),
                            宽带100M新发展=sum(新发展用户数,na.rm=T)),by=Col]
  FTTB_LAN50M及以上<-mobile[业务类型=="互联网业务" & (宽带端口速率M %in% 宽带50M & 宽带接入方式=="FTTB_LAN"),
                             .(FTTB_LAN50M及以上计费=sum(计费到达用户数,na.rm=T),
                               FTTB_LAN50M及以上新发展=sum(新发展用户数,na.rm=T)),by=Col]
  #宽带——FTTH、FTTB+LAN、速率50M以上统计
  宽带集团光宽<-mobile[业务类型=="互联网业务" & (宽带接入方式=="FTTH/O" |  (宽带端口速率M %in% 宽带50M & 宽带接入方式=="FTTB_LAN")),
                     .(集团光宽到达=sum(计费到达用户数,na.rm=T),
                             集团光宽新发展=sum(新发展用户数,na.rm=T)),by=Col]
  #宽带——融合统计，带手机融合，不带手机融合
  宽带集团融合统计<-mobile[业务类型=="互联网业务" & 是否融合=="是",
                       .(宽带集团融合到达=sum(计费到达用户数,na.rm=T),
                                 宽带集团融合新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带本省融合统计<-mobile[业务类型=="互联网业务" & (融合分类=="手机+宽带" | 融合分类=="手机+宽带+电视" | 融合分类=="手机+电视"),
                       .(宽带本省融合到达=sum(计费到达用户数,na.rm=T),
                                 宽带本省融合新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带8M及以下<-mobile[业务类型=="互联网业务" & 宽带端口速率M %in% 宽带8M,
                      .(宽带8M及以下到达=sum(计费到达用户数,na.rm=T),
                          宽带8M及以下新发展=sum(新发展用户数,na.rm=T)),by=Col]
  家庭宽带8M<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 宽带端口速率M %in% 宽带8M, 
                     .(家庭宽带8M及以下到达=sum(计费到达用户数,na.rm=T),
                           家庭宽带8M及以下新发展=sum(新发展用户数,na.rm=T)),by=Col]
  家庭宽带20M以下<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 宽带端口速率M %in% 宽带20M以下,
                        .(家庭宽带20M以下到达=sum(计费到达用户数,na.rm=T),
                              家庭宽带20M以下新发展=sum(新发展用户数,na.rm=T)),by=Col]
  天翼高清用户数<-mobile[(产品 %in% 天翼高清产品 | 产品 %in% IPTV产品),
                  .(天翼高清到达= sum(计费到达用户数,na.rm=T),
                          天翼高清新发展=sum(新发展用户数,na.rm=T)),by=Col]
  家庭单宽arpu<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 是否融合=="否",
                       .(家庭单宽arpu=sum(出账收入,na.rm=T)/sum(计费到达用户数,na.rm=T),
                             家庭单宽收入=sum(出账收入,na.rm=T),
                             家庭单宽计费用户=sum(计费到达用户数,na.rm=T)
                       ),by=Col]
  家庭单宽20M_arpu<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 是否融合=="否" & 宽带端口速率M=="20M",
                           .(家庭单宽20M_arpu=sum(出账收入,na.rm=T)/sum(计费到达用户数,na.rm=T),
                                 家庭单宽20M收入=sum(出账收入,na.rm=T),
                                 家庭单宽20M计费用户=sum(计费到达用户数,na.rm=T)),by=Col]
  家庭单宽50M_arpu<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 是否融合=="否" & 宽带端口速率M=="50M",
                           .(家庭单宽50M_arpu=sum(出账收入,na.rm=T)/sum(计费到达用户数,na.rm=T),
                                 家庭单宽50M收入=sum(出账收入,na.rm=T),
                                 家庭单宽50M计费用户=sum(计费到达用户数,na.rm=T)),by=Col]
  家庭单宽100M_arpu<-mobile[业务类型=="互联网业务" & 细分市场=="家客市场 " & 是否融合=="否" & 宽带端口速率M=="100M",
                            .(家庭单宽100M_arpu=sum(出账收入,na.rm=T)/sum(计费到达用户数,na.rm=T),
                                  家庭单宽100M收入=sum(出账收入,na.rm=T),
                                  家庭单宽100M计费用户=sum(计费到达用户数,na.rm=T)),by=Col]
  移动融合A口径arpu<-mobile[(业务类型=="CDMA后付" | 业务类型=="CDMA智能预付") & 是否融合=="是" & 是否A口径出账=="是",
                      .(移动融合A口径arpu=sum(出账收入,na.rm=T)/sum(集团出账用户数,na.rm=T),
                            移动融合a口径出账收入=sum(出账收入,na.rm=T),
                            移动融合a口径出账用户数=sum(集团出账用户数,na.rm=T)),by=Col]
  宽带存量计费用户数<-mobile[业务类型=="互联网业务" & 用户存增量标识=="存量",
                        .(存量宽带计费到达=sum(计费到达用户数,na.rm=T)),by=Col]  
  宽带网上用户<-mobile[业务类型=="互联网业务",
                     .(网上用户=sum(网上用户数,na.rm=T)),by=Col]
  存量单宽 <-mobile[业务类型=="互联网业务" & 用户存增量标识=="存量" & 是否融合=="否",
                    .(存量单宽=sum(计费到达用户数,na.rm=T)),by=Col]  
  实体天翼高清用户数<-mobile[(产品 %in% 天翼高清产品 | 产品 %in% IPTV产品) & 部门一级=="实体",
                    .(实体天翼高清到达= sum(计费到达用户数,na.rm=T),
                              实体天翼高清新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带200M及以上<-mobile[业务类型=="互联网业务" & 宽带端口速率M %in% 宽带200M,
                        .(宽带200M到达=sum(计费到达用户数,na.rm=T),
                            宽带200M新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带FTTB <- mobile[业务类型=="互联网业务" &  宽带接入方式=="FTTB_LAN",
                       .(FTTB_LAN计费=sum(计费到达用户数,na.rm=T),
                         FTTB_LAN新发展=sum(新发展用户数,na.rm=T)),by=Col]
  宽带年新发展<-mobile[业务类型=="互联网业务",
                   .(宽带年累计新发展=sum(本年累计新发展用户数,na.rm=T)),by=Col]
  result<-merge(固网收入,宽带业务,by=Col,all=T)
  result<-merge(result,宽带20M及以上,by=Col,all=T)
  result<-merge(result,宽带50M及以上,by=Col,all=T)
  result<-merge(result,宽带100M及以上,by=Col,all=T)
  result<-merge(result,宽带集团光宽,by=Col,all=T)
  result<-merge(result,FTTB_LAN50M及以上,by=Col,all=T)
  result<-merge(result,宽带集团融合统计,by=Col,all=T)
  result<-merge(result,宽带本省融合统计,by=Col,all=T)
  result<-merge(result,家庭宽带业务,by=Col,all=T)
  result<-merge(result,宽带8M及以下,by=Col,all=T)
  result<-merge(result,家庭宽带8M,by=Col,all=T)
  result<-merge(result,家庭宽带20M以下,by=Col,all=T)
  result<-merge(result,天翼高清用户数,by=Col,all=T)
  result<-merge(result,家庭单宽arpu,by=Col,all=T)
  result<-merge(result,家庭单宽20M_arpu,by=Col,all=T)
  result<-merge(result,家庭单宽50M_arpu,by=Col,all=T)
  result<-merge(result,家庭单宽100M_arpu,by=Col,all=T)
  result<-merge(result,移动融合A口径arpu,by=Col,all=T)
  result<-merge(result,宽带存量计费用户数,by=Col,all=T)
  result<-merge(result,宽带网上用户,by=Col,all=T)
  result<-merge(result,存量单宽,by=Col,all=T)
  result<-merge(result,实体天翼高清用户数,by=Col,all=T)
  result<-merge(result,宽带200M及以上,by=Col,all=T)
  result<-merge(result,宽带FTTB,by=Col,all=T)
  result<-merge(result,宽带年新发展,by=Col,all=T)

  fname<-str_c("quick_guding",Col[1],".csv")
  write.table(result,fname,sep = ",",row.names = FALSE)
  
  return(result)
}



# 2、生成地市维度报表 ----------------------------------------------------------------
Col<-"地市"
x<-report_mobile(Col)
y<-report_guding(Col)
# 3、生成细分市场维度 ----------------------------------------------------------------
Col<-"细分市场"
x<-report_mobile(Col)
y<-report_guding(Col)
# 4、生成部门一级报表 ----------------------------------------------------------------
#Col<-c("部门一级","部门二级")
#Col<-c("部门一级","部门二级","部门三级")
Col<-c("部门一级")
x<-report_mobile(Col)
y<-report_guding(Col)
# 5、生成渠道视图报表 ----------------------------------------------------------------
Col<-"渠道视图_销售点一级"
x<-report_mobile(Col)
y<-report_guding(Col)
# 6、生成城乡标识报表 ----------------------------------------------------------------
Col<-c("城乡标识")
x<-report_mobile(Col)
y<-report_guding(Col)
# 7、生成用户类型报表 ----------------------------------------------------------------
Col<-c("用户类型")
x<-report_mobile(Col)
y<-report_guding(Col)
Col<-c("ARPU分档")
x<-report_mobile(Col)
Col <- "区县分类"
x<-report_mobile(Col)
y<-report_guding(Col)

#Col <- c("部门一级","地市")
#x<-report_mobile(col)
#x<-melt(mobile,na.rm = T)
#y<-dcast(x,地市~宽带端口速率M,sum,subset=.(variable=="计费到达用户数" & 业务类型=="互联网业务"))
#write.table(y,"y.csv",sep = ",",row.names = FALSE)

############################################################################

