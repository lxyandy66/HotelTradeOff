#���ű�����ʵ�־Ƶ�յ������¶Ⱦ��࣬����DTW����

##���ھƵ����ݲɼ�Ϊ�¼�������������δ�ڵ������޷��ɼ�
# ��Ҫ�����������ݲ岹

usingBldgId<-c("SH_01","SH_02","SH_03","SH_04","SH_05")
data.htl.hour.ac.env<-data.htl.hour.ac[bldgId%in% usingBldgId]

####���������ж�ӦLabelDevDate��Сʱ���ݣ�ƥ���������������ݣ��Բ�������####
asst.htl.hour<-data.table(datetime=seq.POSIXt(from = as.POSIXct("2018-01-01 00:00"),
                                              to=as.POSIXct("2020-12-31 00:00"),by = "hour"))%>%.[,":="(date=format(datetime,format="%Y-%m-%d"))]

asst.htl.hour.label<-data.htl.hour.ac.env[,c("labelDevHour","datetime")]%>%.[,":="(date=format(datetime,format="%Y-%m-%d"),
                                                                                   labelDevDate=substring(labelDevHour,1,20))]
asst.htl.hour.label<-asst.htl.hour.label[!duplicated(asst.htl.hour.label[,c("labelDevDate","date")])]
asst.htl.hour.label[,c("labelDevHour","datetime")]<-NULL
#ʵ����ǰ�漸��Ӧ���и��ŵķ���
asst.htl.hour.label.merge<-merge(x=asst.htl.hour.label,y=asst.htl.hour,all.x = TRUE,by="date",allow.cartesian=TRUE)#ע�����Ĳ���
rm(asst.htl.hour.label,asst.htl.hour)
asst.htl.hour.label.merge[,labelDevHour:=paste(labelDevDate,sprintf("%02d",hour(datetime)),sep = "_")]
asst.htl.hour.label.merge<-merge(asst.htl.hour.label.merge,data.htl.weather.sh[,c("datetime","outTemp")],by.x="datetime",by.y="datetime",all.x = TRUE)

data.htl.hour.ac.env<-merge(x=data.htl.hour.ac.env,y=asst.htl.hour.label.merge[,c("labelDevHour","outTemp")],by="labelDevHour",all.y = TRUE)


data.htl.hour.ac.env<-data.htl.hour.ac.env[,c("deviceId","datetime","hour","onRatio","maxMode","apprIntemp")]
data.htl.hour.ac.env[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.env[,":="(modiDate=date(modiDatetime),modiHour=hour(modiDatetime),labelDevDate=paste(deviceId,date(modiDatetime),sep="_"))]
####������ת��####
data.htl.hour.ac.env.wide<-
  dcast(data.htl.hour.ac.env[,c("labelDevDate","modiHour","apprIntemp")],
        formula = labelDevDate~modiHour,value.var = "apprIntemp")%>%as.data.table(.)
nn1<-data.htl.hour.ac.env[,.(maxMode=getMode(maxMode[onRatio>0.25],na.rm = TRUE)[1]),by=labelDevDate]

data.htl.hour.ac.env.wide<-merge(x=data.htl.hour.ac.env.wide,y=nn1,all.x=TRUE,by="labelDevDate")
names(data.htl.hour.ac.env.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""),"maxMode")

data.htl.hour.ac.env.wide$count<-apply(data.htl.hour.ac.env.wide[,c(paste("h+14_",0:23,sep = ""))],MARGIN = 1,FUN = function(x){sum(is.na(x),na.rm = TRUE)})
nchar("SH_01_68-7C_19-03-20")