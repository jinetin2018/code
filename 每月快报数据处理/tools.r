library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

#��������
download<-function(stock,from="2010-01-01"){
  df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  #��������
  names(df)<-c("Open","High","Low","Close","Volume","Adjusted")
  write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) #���浽����
}

#���ض�����
read<-function(stock){  
  as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
}

#�ƶ�ƽ��
ma<-function(cdata,mas=c(5,20,60)){ 
  ldata<-cdata
  for(m in mas){
    ldata<-merge(ldata,SMA(cdata,m))
  }
  ldata<-na.locf(ldata, fromLast=TRUE)
  names(ldata)<-c('Value',paste('ma',mas,sep=''))
  return(ldata)
}

# ����ͼ
drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  
  if(out) ggsave(g,file=paste(titie,".png",sep=""))
  else g
}

# ����ͼ+ɢ��
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

# �ɼ�+�ֽ�����
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


# ����ͼ+��������
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


#�����ź�
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

#ģ�⽻��
trade<-function(tdata,capital=100000,position=1,fee=0.00003){#�����ź�,����,�ֱֲ���,�����ѱ���
  amount<-0       #�ֹ�����
  cash<-capital   #�ֽ�
  
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
    
    row$cash<-cash #�ֽ�
    row$amount<-amount #�ֹ�����
    row$asset<-cash+amount*row$Value # �ʲ���ֵ
    ticks<-rbind(ticks,row)
  }
  
  ticks$diff<-c(0,diff(ticks$asset)) # �ʲ���ֵ��
  
  #׬Ǯ�Ĳ���
  rise<-ticks[c(which(ticks$diff>0)-1,which(ticks$diff>0)),]
  rise<-rise[order(row.names(rise)),]
  
  #��Ǯ�Ĳ���
  fall<-ticks[c(which(ticks$diff<0)-1,which(ticks$diff<0)),]
  fall<-fall[order(row.names(fall)),]
  
  
  return(list(
    ticks=ticks,
    rise=rise,
    fall=fall
  ))
}