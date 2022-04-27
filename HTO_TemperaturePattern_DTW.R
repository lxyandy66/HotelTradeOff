#本脚本用于实现酒店空调室内温度聚类，基于DTW方法

usingBldgId<-c("SH_01","SH_02","SH_03","SH_04","SH_05")
data.htl.hour.ac.env<-data.htl.hour.ac[bldgId%in% usingBldgId]

data.htl.hour.ac.env<-data.htl.hour.ac.env[,c("deviceId","datetime","hour","onRatio","maxMode")]
data.htl.hour.ac.env[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.env[,":="(modiDate=date(modiDatetime),modiHour=hour(modiDatetime),labelDevDate=paste(deviceId,date(modiDatetime),sep="_"))]
####宽数据转换####
data.htl.hour.ac.env.wide<-
  dcast(data.htl.hour.ac.env[,c("labelDevDate","modiHour","onRatio")],
        formula = labelDevDate~modiHour,value.var = "onRatio")%>%as.data.table(.)
nn1<-data.htl.hour.ac.env[,.(maxMode=getMode(maxMode[onRatio>0.25],na.rm = TRUE)[1]
),by=labelDevDate]
data.htl.hour.ac.env.wide<-merge(x=data.htl.hour.ac.dtw.usage.wide,y=nn1,all.x=TRUE,by="labelDevDate")
names(data.htl.hour.ac.env.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""),"maxMode")
