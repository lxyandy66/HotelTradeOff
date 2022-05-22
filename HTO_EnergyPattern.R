

usingBldgId<-c("SH_01","SH_02","SH_03","SH_04","SH_05")
data.htl.hour.ac.energy<-data.htl.hour.ac[bldgId%in% usingBldgId]

#�����Ż�����ջ���
data.htl.hour.ac.energy[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.energy[,":="(modiDate=date(modiDatetime),
                              modiHour=hour(modiDatetime),labelDevModiDate=paste(deviceId,date(modiDatetime),sep="_"))]


#����HZNU�ķ�������������Ϊ�տյ��ܺģ��տյ�ʹ��ʱ������ʱ�ܺı�׼��
data.htl.day.ac.energy<-data.htl.hour.ac.energy[,.(date=format(modiDate[1],format="%Y-%m-%d"),
                                                   sumElec=sum(sumElec,na.rm=TRUE)/(3600*1000),
                                                   #runtime=sum(runtime,na.rm = TRUE), #runtime����Ϊ���ݼ���ֱ�Ӻϲ�
                                                   stdDevElec=sd(sumElec/(3600*1000),na.rm = TRUE),
                                                   logCount=length(datetime)
                                                   ),by=labelDevModiDate]
#ע�����label�Ļ���
#��Ϊ��SH_01_65-78_2019-11-18
#����Ϊ��labelDevDate��modiDate
data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,
                              y=data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","runtime","season","maxMode","seasonMode","month","isBizday","dtwUsageMode")],
                              all.x=TRUE,by.x = "labelDevModiDate",by.y="labelDevDate")

#�������Եı�׼��
data.htl.day.ac.energy[,":="(sdSumElec=as.numeric(NA),sdStdDevElec=as.numeric(NA),sdRuntime=as.numeric(NA))]
for(i in c("Summer","Winter")){
  data.htl.day.ac.energy[season==i]$
}