library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

#下载数据
download<-function(stock,from="2010-01-01"){
  df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  #下载数据
  names(df)<-c("Open","High","Low","Close","Volume","Adjusted")
  write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) #保存到本地
}

#本地读数据
read<-function(stock){  
  as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
}

#移动平均
ma<-function(cdata,mas=c(5,20,60)){ 
  ldata<-cdata
  for(m in mas){
    ldata<-merge(ldata,SMA(cdata,m))
  }
  ldata<-na.locf(ldata, fromLast=TRUE)
  names(ldata)<-c('Value',paste('ma',mas,sep=''))
  return(ldata)
}

# 均线图
drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  
  if(out) ggsave(g,file=paste(titie,".png",sep=""))
  else g
}

# 均线图+散点
drawPoint<-function(ldata,pdata,titie="Stock_2014",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+geom_point(aes(x=Index,y=Value,colour=Series),data=fortify(pdata,melt=TRUE))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  
  if(out) ggsave(g,file=paste(titie,".png",sep=""))
  else g
}

# 股价+现金流量
drawCash<-function(ldata,adata,titie="Stock_2014",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(x=as.Date(Index), y=Value,colour=Series),data=fortify(adata,melt=TRUE))
  g<-g+facet_grid(Series ~ .,scales = "free_y")
  g<-g+scale_y_continuous(labels = dollar)
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  
  if(out) ggsave(g,file=paste(titie,".png",sep=""))
  else g
}


# 均线图+交易区间
drawRange<-function(ldata,plan,titie="Stock_2014",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+geom_rect(aes(NULL, NULL,xmin=start,xmax=end,fill=plan),ymin = yrng[1], ymax = yrng[2],data=plan)
  g<-g+scale_fill_manual(values =alpha(c("blue", "red"), 0.2))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  
  if(out) ggsave(g,file=paste(titie,".png",sep=""))
  else g
}


#交易信号
Signal<-function(cdata,pdata){
  tmp<-''
  tdata<-ddply(pdata[order(pdata$Index),],.(Index,Series),function(row){
    if(row$Series==tmp) return(NULL)
    tmp<<-row$Series
  })
  tdata<-data.frame(cdata[tdata$Index],op=ifelse(tdata$Series=='down','B','S'))
  names(tdata)<-c("Value","op")
  return(tdata)
}

#模拟交易
trade<-function(tdata,capital=100000,position=1,fee=0.00003){#交易信号,本金,持仓比例,手续费比例
  amount<-0       #持股数量
  cash<-capital   #现金
  
  ticks<-data.frame()
  for(i in 1:nrow(tdata)){
    row<-tdata[i,]
    if(row$op=='B'){
      amount<-floor(cash/row$Value)
      cash<-cash-amount*row$Value
    }
    
    if(row$op=='S'){
      cash<-cash+amount*row$Value
      amount<-0
    }
    
    row$cash<-cash #现金
    row$amount<-amount #持股数量
    row$asset<-cash+amount*row$Value # 资产总值
    ticks<-rbind(ticks,row)
  }
  
  ticks$diff<-c(0,diff(ticks$asset)) # 资产总值差
  
  #赚钱的操作
  rise<-ticks[c(which(ticks$diff>0)-1,which(ticks$diff>0)),]
  rise<-rise[order(row.names(rise)),]
  
  #赔钱的操作
  fall<-ticks[c(which(ticks$diff<0)-1,which(ticks$diff<0)),]
  fall<-fall[order(row.names(fall)),]
  
  
  return(list(
    ticks=ticks,
    rise=rise,
    fall=fall
  ))
}