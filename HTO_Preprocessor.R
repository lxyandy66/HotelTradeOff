####���ű�����HTO��Ŀ�����ݵ��뼰ͳ��Ԥ����####


for(i in list.files()[grep(".csv",list.files(),fixed = TRUE)]){
  nn<-as.data.table(read.csv(file = i))
  nn$datetime<-as.POSIXct(nn$datetime)
  nn$deviceId<-as.character(nn$deviceId)
  # ÿ��������ͳ�Ƽ���
  stat.htl.bldg.base<-data.table(hotelName=i,
                                 tidCount=length(unique(nn$tid)),
                                 deviceCount=length(unique(nn$deviceId)),
                                 logCount=nrow(nn))%>%{
                                   if (exists("stat.htl.bldg.base")) {
                                     rbind(stat.htl.bldg.base,.)
                                   }else .
                                 }
  # �����ڿյ���ͳ�Ƽ���
  stat.htl.device.base<-data.table(hotelName=i,
                                   nn[,.(fromTime=min(datetime,na.rm = TRUE),
                                         toTime=max(datetime,na.rm = TRUE),
                                         logDuration=max(datetime,na.rm = TRUE)-min(datetime,na.rm = TRUE),
                                         deviceLogCount=length(tid),
                                         meanPower=mean(power[power>50],na.rm=TRUE)
                                   ),by=deviceId])%>%{
                                     if (exists("stat.htl.device.base")) {
                                       rbind(stat.htl.device.base,.)
                                     }else .
                                   }#Ϊʲô������Ŀ�����...
  gc()
}
#��һ��һ��¥���յ���������ʱ��ƽ��ֵ
ggplot(nn[power>50],aes(x=as.factor(deviceId),y=power,group=deviceId))+
  geom_boxplot(outlier.alpha = 0.3)+
  stat_summary(geom = "point",fun.y = mean,na.rm=TRUE,color="red")+
  stat_summary(geom = "line",fun.y = mean,na.rm=TRUE,group=1,color="red")