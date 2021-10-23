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
  if (exists("data.htl.raw.append")) {
    rbind(data.htl.raw.append,nn)
  }else {
    data.htl.raw.append<-as.data.table(nn)
  }
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
ggplot(data.htl.raw,aes(x=interval))+geom_density()

nrow(data.htl.raw[totalElec>8000])


stat.htl.device.hour<-data.htl.hour.ac[,.(
  hourLogCount=length(labelDevHour),
  meanOnPower=mean(mPower[mPower>100],na.rm=TRUE),
  useRatio=sum(onCount,na.rm=TRUE)/sum(logCount,na.rm=TRUE)
),by=deviceId]

# 合并设备ID
info.htl.device.base<-read.xlsx(file="HTL_设备统计_tid.xlsx",sheetIndex = 1)
data.htl.raw<-merge(data.htl.raw,info.htl.device.base[,c("tid","deviceId")],
                    by.x="tid",by.y="tid",all.x=TRUE)
save(data.htl.raw,file="HTL_原始数据集合_仅分配ID.rdata")


####合并追加数据用####
# 数据文件为data.htl.raw/data.htl.raw.append
# 去掉一些不需要的行

names(data.htl.raw.append)[2]<-"deviceNo"
# append  [1] "tid"       "deviceNo"  "datetime"  "onOff"     "mode"     
# [6] "fanSpeed"  "setTemp"   "power"     "totalElec" "inTemp"  

data.htl.raw[,c("city","labeDevHour","id","deviceId")]<-NULL
names(data.htl.raw)
data.htl.raw.merge<-rbind(data.htl.raw,data.htl.raw.append)
# 合并后删除原数据
data.htl.raw<-data.htl.raw.merge
rm(data.htl.raw.append)
rm(data.htl.raw.merge)

#
data.htl.raw<-data.htl.raw[!duplicated(data.htl.raw)]

data.htl.raw$month<-month(data.htl.raw$datetime)

nn1<-data.htl.raw[(month%in%c(12,1,2))&inTemp<5]$labelDevHour%>%{data.htl.raw[labelDevHour%in%.]}
ggplot(nn3[inTemp<40],aes(x=inTemp))+geom_density()


# 清理部分异常值
# 功率>3000
# 室温>40
data.htl.raw<-data.htl.raw%>%{
  .[inTemp>40]$inTemp<-NA
  .[inTemp<5]$inTemp<-NA
  .[power>3000]$power<-NA
  .[totalElec>10000]$totalElec<-NA
  .
}

setorder(data.htl.raw,labelDevHour)
data.htl.raw$previousTime<-c(NA,data.htl.raw[1:(nrow(data.htl.raw)-1)]$datetime)
data.htl.raw$interval<-as.numeric(data.htl.raw$datetime)-data.htl.raw$previousTime
data.htl.raw$city<-substr(data.htl.raw$deviceId,1,2)

#合并至小时
setorder(data.htl.raw,deviceId,datetime)
#注意广州的csv时间格式导入不同，需要重新单独进行时间的合并
#data.htl.raw$datetime<-as.POSIXct(data.htl.raw$datetime)
data.htl.raw$labelDevHour<-data.htl.raw%>%
  {paste(.$deviceId,format(.$datetime,format="%y-%m-%d_%H"),sep = "_")}
#此处已完成如上基本清洗
data.htl.hour.ac<-data.htl.raw[,.(
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
    mIntemp=mean(inTemp,na.rm=TRUE),
    mInterval=mean(interval[interval>0&interval<1800],na.rm=TRUE)
  ),by=labelDevHour]
+

####统计各小时的使用情况#####
data.htl.hour.ac[,':='(hour=substr(labelDevHour,22,23),
                    bldgId=substr(labelDevHour,1,5))]

data.htl.hour.ac[,datetime:=as.POSIXct(paste("20",substr(labelDevHour,13,20),hour,":00",sep = ""))]

data.htl.hour.bldg<-data.htl.hour.ac[,.(deviceCount=length(labelDevHour),
                                        hour=hour[1],
                                        onDevCount=length(labelDevHour[(onCount/logCount)>0.5]),
                                        mPower=sum(mPower[!is.nan(mPower)],na.rm = TRUE),
                                        totalElec=sum(totalElec[!is.nan(totalElec)],na.rm = TRUE)
                                        ),by=.(bldgId,datetime)]
data.htl.hour.bldg[,onRatio:=onDevCount/deviceCount]

data.htl.day.ac<-data.htl.hour.ac[,.(deviceCount=length(labelDevHour),
                                        onDevCount=length(labelDevHour[(onCount/logCount)>0.5]),
                                        mPower=sum(mPower[!is.nan(mPower)],na.rm = TRUE),
                                        totalElec=sum(totalElec[!is.nan(totalElec)],na.rm = TRUE)),
                                  by=.(deviceId,date(datetime))]

ggplot(data.htl.raw[substr(deviceId,1,5)=="SH_01"&power>100],aes(x=as.factor(mode),y=power))+
  geom_boxplot(width=0.5)+stat_summary(fun = mean,na.rm=TRUE,geom = "line",color="red",group=1)+
  stat_summary(fun = mean,na.rm=TRUE,geom = "point",color="red")+facet_wrap(.~as.factor(onOff))+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))

names(data.htl.hour.ac)[1]<-"labelDevHour"

ggplot(data.htl.hour.ac[substr(deviceId,1,5)=="SH_01"],aes(x=hour,y=length(unique(deviceId))))+geom_density()+#xlim(c(0,10000))+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))


outputCol<-c( "datetime","deviceId","onOff","mode","fanSpeed","setTemp","inTemp","power","totalElec")
data.htl.raw$bldgId<-substr(data.htl.raw$deviceId,1,5)
for(i in unique(data.htl.raw$bldgId)){
  write.csv(data.htl.raw[bldgId==i,..outputCol],file=paste(i,"Cleaned.csv",sep = "_"))
}

