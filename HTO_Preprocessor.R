####本脚本用于HTO项目的数据导入及统计预处理####

###从数据库读取####
#SQL 数据导入
connect<-dbConnect(MySQL(),dbname="hoteldata",user="root",password="141242343")
data.htl.raw<-as.data.table(dbReadTable(connect,"hotel_monitoring"))

length(unique(data.htl.raw$tid))
# 1125
length(unique(data.htl.raw$deviceId))
# 1131


####基本信息统计####
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
ggplot(data.htl.raw[power>50],aes(x=as.factor(deviceId),y=power,group=deviceId))+
  geom_boxplot(outlier.alpha = 0.3)+
  stat_summary(geom = "point",fun.y = mean,na.rm=TRUE,color="red")+
  stat_summary(geom = "line",fun.y = mean,na.rm=TRUE,group=1,color="red")

# 看一下室内温度分布
ggplot(data.htl.raw[totalElec<10000],aes(x=totalElec))+geom_density()

nrow(data.htl.raw[totalElec>8000])


stat.htl.device.hour<-data.htl.hour[,.(
  hourLogCount=length(labelDevHour),
  meanPower=mean(mPower[mPower>100],na.rm=TRUE),
  useRatio=sum(logCount,na.rm=TRUE)/sum(onCount,na.rm=TRUE)
),by=(deviceId=substr(labelDevHour,1,11))]

# 合并设备ID
info.htl.device.base<-read.xlsx(file="HTL_设备统计_tid.xlsx",sheetIndex = 1)
data.htl.raw<-merge(data.htl.raw,info.htl.device.base[,c("tid","deviceId")],
                    by.x="tid",by.y="tid",all.x=TRUE)
save(data.htl.raw,file="HTL_原始数据集合_仅分配ID.rdata")
s
# 清理部分异常值
# 功率>3000
# 室温>40
data.htl.raw<-data.htl.raw%>%{
  .[inTemp>40]$inTemp<-NA
  .[power>3000]$power<-NA
  .[totalElec>10000]$totalElec<-NA
  .
}

data.htl.raw$interval<-
data.htl.raw$city<-substr(data.htl.raw$deviceId,1,2)

#合并至小时
setorder(data.htl.raw,deviceId,datetime)
#注意广州的csv时间格式导入不同，需要重新单独进行时间的合并
#data.htl.raw$datetime<-as.POSIXct(data.htl.raw$datetime)
data.htl.raw$labelDevHour<-data.htl.raw%>%
  {paste(.$deviceId,format(.$datetime,format="%y-%M-%d_%H"),sep = "_")}
#此处已完成如上基本清洗
data.htl.hour<-data.htl.raw[,.(
    deviceId=deviceId[1],
    logCount=length(datetime),
    onCount=length(datetime[onOff==1]),
    maxMode=getMode(mode,na.rm = TRUE)[1],
    mFanspeed=mean(fanSpeed,na.rm=TRUE),
    mSetTemp=mean(setTemp[onOff==1]),
    mPower=mean(power[onOff==1],na.rm=TRUE),
    mLowPower=mean(power[onOff==1&power<100],na.rm=TRUE),
    mHighPower=mean(power[onOff==1&power>100],na.rm=TRUE),
    lowPowerCount=length(power[onOff==1&power<100]),
    highPowerCount=length(power[onOff==1&power>100]),
    totalElec=max(totalElec,na.rm=TRUE)[1]-min(totalElec,na.rm=TRUE)[1],
    mIntemp=mean(inTemp,na.rm=TRUE)
  ),by=(labelDevHour=paste(deviceId,format(datetime,format="%y-%M-%d_%H"),sep = "_"))]

names(data.htl.hour)[1]<-"labelDevHour"

data.htl.hour
