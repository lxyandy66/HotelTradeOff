####本脚本用于HTO项目的数据导入及统计预处理####


for(i in list.files()[grep(".csv",list.files(),fixed = TRUE)]){
  nn<-as.data.table(read.csv(file = i))
  nn$datetime<-as.POSIXct(nn$datetime)
  nn$deviceId<-as.character(nn$deviceId)
  # 每栋建筑的统计集合
  stat.htl.bldg.base<-data.table(hotelName=i,
                                 tidCount=length(unique(nn$tid)),
                                 deviceCount=length(unique(nn$deviceId)),
                                 logCount=nrow(nn))%>%{
                                   if (exists("stat.htl.bldg.base")) {
                                     rbind(stat.htl.bldg.base,.)
                                   }else .
                                 }
  # 建筑内空调的统计集合
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
                                   }#为什么不能三目运算符...
  gc()
}
#看一下一栋楼各空调正常工作时的平均值
ggplot(nn[power>50],aes(x=as.factor(deviceId),y=power,group=deviceId))+
  geom_boxplot(outlier.alpha = 0.3)+
  stat_summary(geom = "point",fun.y = mean,na.rm=TRUE,color="red")+
  stat_summary(geom = "line",fun.y = mean,na.rm=TRUE,group=1,color="red")
