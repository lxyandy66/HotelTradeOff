# 本脚本处理上海逐时数据
data.htl.weather.sh<-fread(file="上海逐时天气/Input_SH_Weather_2018.csv")%>%
  rbind(fread(file="上海逐时天气/Input_SH_Weather_2019.csv"))%>%
  rbind(fread(file="上海逐时天气/Input_SH_Weather_2020.csv"))
names(data.htl.weather.sh)<-c("datetime","city","windDir","windSpd","outTemp","ralHum")
data.htl.weather.sh[,":="(datetime=as.POSIXct(datetime),city="SH")]

#整理至小时级
setorder(data.htl.weather.sh,datetime)
data.htl.weather.sh[,labelDatetime:=format((datetime),format="%Y-%m-%d_%H")]
data.htl.weather.sh<-data.htl.weather.sh[,.(datetime=datetime[1],
                                            windDir=windDir[1],
                                            windSpd=mean(windSpd,na.rm=TRUE),
                                            outTemp=mean(outTemp,na.rm=TRUE),
                                            ralHum=mean(ralHum,na.rm=TRUE)),by=labelDatetime]

data.htl.weather.sh$labelDatetime<-NULL
                       
