#本脚本用于实现酒店空调室内温度聚类，基于DTW方法

##由于酒店数据采集为事件驱动，大量人未在的数据无法采集
# 需要根据气象数据插补

usingBldgId<-c("SH_01","SH_02","SH_03","SH_04","SH_05")
data.htl.hour.ac.env<-data.htl.hour.ac[bldgId%in% usingBldgId]

####整理出所有对应LabelDevDate的小时数据，匹配至室外气象数据，以补充完整####
asst.htl.hour<-data.table(datetime=seq.POSIXt(from = as.POSIXct("2018-01-01 00:00"),
                                              to=as.POSIXct("2020-12-31 00:00"),by = "hour"))%>%.[,":="(date=format(datetime,format="%Y-%m-%d"))]

asst.htl.hour.label<-data.htl.hour.ac.env[,c("labelDevHour","datetime")]%>%.[,":="(date=format(datetime,format="%Y-%m-%d"),
                                                                                   labelDevDate=substring(labelDevHour,1,20))]
asst.htl.hour.label<-asst.htl.hour.label[!duplicated(asst.htl.hour.label[,c("labelDevDate","date")])]
asst.htl.hour.label[,c("labelDevHour","datetime")]<-NULL


#实际上前面几步应该有更优的方法
asst.htl.hour.label.merge<-merge(x=asst.htl.hour.label,y=asst.htl.hour,all.x = TRUE,by="date",allow.cartesian=TRUE)#注意后面的参数
rm(asst.htl.hour.label,asst.htl.hour)
asst.htl.hour.label.merge[,labelDevHour:=paste(labelDevDate,sprintf("%02d",hour(datetime)),sep = "_")]
#合并空值，保留需要插值的缺失槽位
asst.htl.hour.label.merge<-merge(asst.htl.hour.label.merge,data.htl.weather.sh[,c("datetime","outTemp")],by.x="datetime",by.y="datetime",all.x = TRUE)
data.htl.hour.ac.env<-merge(x=data.htl.hour.ac.env,y=asst.htl.hour.label.merge[,c("labelDevHour","outTemp")],by="labelDevHour",all.y = TRUE)


####合并插值数据####
####接收热环境插值####
data.htl.hour.temp.appx<-fread(file="热环境聚类用插值/sh_01.csv")%>%
  rbind(fread(file="热环境聚类用插值/sh_02_03.csv"))%>%
  rbind(fread(file="热环境聚类用插值/sh_04_05.csv"))

names(data.htl.hour.temp.appx)<-c("V1","labelDevHour","deviceId","datetime","apprFullTemp","outTemp","hour")

data.htl.hour.ac.env<-merge(x=data.htl.hour.ac.env,y=data.htl.hour.temp.appx[,c("labelDevHour","apprFullTemp")],all.x = TRUE)

#注意原来的asst填充表里面很多标签没有，例如label datetime等
data.htl.hour.ac.env[,":="(datetime=as.POSIXct(substring(labelDevHour,13),format="%y-%m-%d_%H"),
                           deviceId=substring(labelDevHour,1,11),bldgId=substring(deviceId,1,5),
                           isAppr=is.na(apprIntemp))]
data.htl.hour.ac.env<-data.htl.hour.ac.env[,c("deviceId","datetime","onRatio","maxMode","apprFullTemp","isAppr")]
data.htl.hour.ac.env[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.env[,":="(modiDate=date(modiDatetime),modiHour=hour(modiDatetime),labelDevDate=paste(deviceId,date(modiDatetime),sep="_"))]

##按设备-日期统计基本情况（如插补比例、使用时长等）
stat.htl.hour.ac.env<-data.htl.hour.ac.env[]


####宽数据转换####
data.htl.hour.ac.env.wide<-
  dcast(data.htl.hour.ac.env[,c("labelDevDate","modiHour","apprFullTemp")],
        formula = labelDevDate~modiHour,value.var = "apprFullTemp")%>%as.data.table(.)
names(data.htl.hour.ac.env.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""))

#从行为宽数据中获取一些标签以合并
nn1<-data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","season","maxMode","seasonMode","runtime","month","isBizday","dtwUsageMode")]
data.htl.hour.ac.env.wide<-merge(x=data.htl.hour.ac.env.wide,y=nn1,all.x=TRUE,by="labelDevDate")
#计算有数据的时间
data.htl.hour.ac.env.wide$count<-apply(data.htl.hour.ac.env.wide[,c(paste("h+14_",0:23,sep = ""))],MARGIN = 1,FUN = function(x){sum(!is.na(x),na.rm = TRUE)})

#仅保留有使用模式的，即要求与使用模式相同
data.htl.hour.ac.env.wide<-data.htl.hour.ac.env.wide[!is.na(dtwUsageMode)]
#存在部分缺一天，但是前后两天均受影响（第二天的0-14h）直接被删除的情况，其实有些case可以挽救一下，占比加起来10%，直接去掉好了
data.htl.hour.ac.env.wide<-data.htl.hour.ac.env.wide[complete.cases(data.htl.hour.ac.env.wide[,c(paste("h+14_",0:23,sep = ""))])]

####并行计算的配置####
require(doParallel)
# Create parallel workers
cl <- makeCluster(detectCores())
invisible(clusterEvalQ(cl, library(dtw)))
registerDoParallel(cl)


#分别计算，仅计算一次
distTempDtwSummer<-dist(data.htl.hour.ac.env.wide[season=="Summer"&maxMode %in% conditionSelect[["Summer"]],
                                                  c(paste("h+14_",0:23,sep = ""))], method="dtw",window.type = "sakoechiba",window.size=2)
distTempDtwWinter<-dist(data.htl.hour.ac.env.wide[season=="Winter"&maxMode %in% conditionSelect[["Winter"]],
                                                  c(paste("h+14_",0:23,sep = ""))], method="dtw",window.type = "sakoechiba",window.size=2)


#聚类数评估
require(doParallel)
# Create parallel workers
cl <- makeCluster(detectCores())
invisible(clusterEvalQ(cl, library(cluster)))
registerDoParallel(cl)

clusterTestSummerWss<-fviz_nbclust(x=data.htl.hour.ac.env.wide[season==i&maxMode %in% conditionSelect[[i]]][,c(paste("h+14_",0:23,sep = ""))],
                                   FUN = cluster::pam, method = "wss", diss = distTempDtwSummer, k.max = 10)

clusterTestTempSummerMeans<-NbClust(data = data.htl.hour.ac.env.wide[season==i&maxMode %in% conditionSelect[[i]]][,c(paste("h+14_",0:23,sep = ""))], 
                                diss = distTempDtwSummer, distance = NULL,min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)

clusterTestTempSummer<-NbClust(data = data.htl.hour.ac.env.wide[season==i&maxMode %in% conditionSelect[[i]]][,c(paste("h+14_",0:23,sep = ""))], 
                                    diss = distTempDtwSummer, distance = NULL,min.nc = 2, max.nc = 10, method = "centroid", index = "all", alphaBeale = 0.1)

kSize<-c(3:7)
seasonSelect<-c("Summer")#,"Winter"
conditionSelect<-list(Summer=c(1,3,4),Winter=c(1,2))

data.htl.hour.ac.env.wide$tempMode<-as.numeric(NA)

for(i in seasonSelect){
  for(j in kSize){
    if(i=="Summer"){
      localDist<-distTempDtwSummer
    }else{
      localDist<-distTempDtwWinter
    }
    
    data.htl.hour.ac.env.wide[season==i&maxMode %in% conditionSelect[[i]]]$tempMode<-(pamk(localDist,diss=TRUE,krange = j,criter = "ch",usepam = TRUE))$pamobject$clustering
    
    merge(x=data.htl.hour.ac.env.wide[season==i&maxMode %in% conditionSelect[[i]],lapply(.SD,mean,na.rm=TRUE),
                                            .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime") ,by=tempMode],
          y=data.htl.hour.ac.env.wide[season==i&maxMode %in% conditionSelect[[i]],.(count=length(runtime)),by=tempMode],all.x = TRUE,
          by.x="tempMode",by.y = "tempMode")%>%{ 
            write.xlsx(.,file=paste(j,i,"overview_Temp_DTW.xlsx",sep = "_"))
            cat(paste(names(.),collapse = " "),"\n")#,"runtimeHr","occuTime"
            melt(.,id.var=c("stempMode","runtime","count"))%>%{
              cat(paste(i,j,paste(unique(.$runtime),collapse = " "),"\n"))
              ggsave(file=paste(j,i,"MeanValue_Temp_DTW.png",sep = "_"),
                     plot = ggplot(data=.,aes(x=variable,y=value,color=as.factor(tempMode),group=tempMode))+geom_line(), 
                     width=16,height = 5,dpi = 100)
            }
          }
    #热环境热图生成
    tmp.plot.heatMap<-data.htl.hour.ac.env.wide[season==i&
                                                  maxMode %in% conditionSelect[[i]]][,c("labelDevDate","tempMode",
                                                                                        paste("h+14_",0:23,sep = ""))]%>%
                      melt(.,id.var=c("labelDevDate","tempMode"))%>%as.data.table
    range<-boxplot.stats(tmp.plot.heatMap$value)
    for(k in unique(tmp.plot.heatMap$tempMode)){
      ggsave(file=paste(i,"kSize",j,"tempMode",k,"heatMap.png",sep="_"),
             plot=ggplot(data=tmp.plot.heatMap[tempMode==1],
                         aes(x=variable,y=labelDevDate,fill=value,group=as.factor(tempMode)))+
               geom_raster(interpolate = TRUE)+
               scale_fill_gradient(limits = c(20,32),low = "green",high = "red")+#range$stats[1],range$stats[5]
               facet_wrap(~ tempMode, nrow = 2)+theme_classic()+
               theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
             width=4,height=3,dpi=80)
    }
  }
}


#### 正式聚类 ####
data.htl.hour.ac.env.wide$tempMode<-as.numeric(NA)
data.htl.hour.ac.env.wide[season=="Winter"&maxMode %in% conditionSelect[["Winter"]]]$tempMode<-
  (pamk(distTempDtwWinter,diss=TRUE,krange = 5,criter = "ch",usepam = TRUE))$pamobject$clustering
data.htl.hour.ac.env.wide[season=="Summer"&maxMode %in% conditionSelect[["Summer"]]]$tempMode<-
  (pamk(distTempDtwSummer,diss=TRUE,krange = 4,criter = "ch",usepam = TRUE))$pamobject$clustering



####聚类特征分析####
unique(data.htl.hour.ac.env.wide$labelDevDate)[1:10]

####长数据还原####
####以data.htl.hour.ac.env为基准，补充内容以data.htl.hour.ac为主进行长数据合并
#行为模式合并至温度
data.htl.hour.ac.env<-merge(x=data.htl.hour.ac.env,y=data.htl.hour.ac.env.wide[,c("labelDevDate","tempMode")],
                            all.x=TRUE,by="labelDevDate")
data.htl.hour.ac.env<-merge(x=data.htl.hour.ac.env,y=data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","patternName")],
                            all.x=TRUE,by="labelDevDate")


#还原小时label
data.htl.hour.ac.env[,labelDevHour:=paste(deviceId,format(datetime,format="%y-%m-%d_%H"),sep = "_")]
data.htl.hour.ac.env<-merge(x=data.htl.hour.ac.env,y=data.htl.hour.ac[,c("labelDevHour","mSetTemp","mIntemp","onRatio","apprIntemp")],
                            all.x=TRUE,by="labelDevHour")
data.htl.hour.ac.env<-merge(x=data.htl.hour.ac.env,y=data.htl.weather.sh[,c("datetime","outTemp")],
                            all.x=TRUE,by="datetime")
data.htl.hour.ac.env$season<-apply(as.data.table(month(data.htl.hour.ac.env$modiDatetime)),MARGIN = 1,FUN = getHotelSeason)
data.htl.hour.ac.env$month<-month(data.htl.hour.ac.env$modiDatetime)
#长数据还原完成


#室内温度模式统计
stat.htl.hour.ac.env<-data.htl.hour.ac.env[season=="Summer"&!is.na(tempMode),
                                           .(mSetTemp=mean(mSetTemp[onRatio>0.25],na.rm=TRUE),
                                             apprFullTemp=mean(apprFullTemp,na.rm=TRUE),
                                             apprIntemp=mean(apprIntemp,na.rm=TRUE),
                                             outTemp=mean(outTemp,na.rm=TRUE)
                                             ),by=tempMode]

ggplot(data.htl.hour.ac.env[!is.na(tempMode)],aes(x=modiHour,y=(apprIntemp-outTemp),color=as.factor(tempMode),group=modiHour))+
  geom_boxplot()+facet_wrap(.~tempMode,nrow=5)

ggplot(data.htl.hour.ac.env.wide[!is.na(tempMode)&season=="Summer"],aes(x=tempMode,y=(mSetTemp),color=as.factor(tempMode)))+
  geom_boxplot()+stat_summary(fun.y = "mean",geom = "line",group=1)+ 
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))
#+facet_wrap(.~tempMode,nrow=5)+

#查看各温度模式的逐时箱形图
data.htl.hour.ac.env.wide[!is.na(tempMode)&season=="Summer",c("labelDevDate","tempMode",paste("h+14_",0:23,sep = ""))]%>%
  melt(.,id.var=c("labelDevDate","tempMode"))%>%{
    ggplot(.,aes(x=variable,y=value,color=as.factor(tempMode)))+geom_boxplot()+facet_wrap(.~tempMode,nrow=5)
  }


##统计日内设定温度
data.htl.hour.ac.env.wide<-merge(x=data.htl.hour.ac.env.wide,
                                 y = data.htl.hour.ac.env[,.(mSetTemp=mean(mSetTemp[onRatio>0.25],na.rm=TRUE)),by=labelDevDate],
                                 all.x = TRUE,by="labelDevDate")

####宽数据处理####
#宽数据的行为模式等合并
data.htl.hour.ac.env.wide$dtwUsageMode<-NULL#[,c(dtwUsageMode)]
#!data.htl.hour.ac.env.wide$labelDevDate %in% data.htl.hour.ac.dtw.usage.wide$labelDevDate # 温度模式都包括行为模式
data.htl.hour.ac.env.wide<-merge(x=data.htl.hour.ac.env.wide,y = data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","patternName","occuTime")],
                                 all.x = TRUE,by="labelDevDate")


#室外温度合并
data.htl.hour.ac.env.wide[,modiDate:=substring(labelDevDate,13)]
data.htl.hour.ac.env.wide<-merge(x=data.htl.hour.ac.env.wide,y=data.htl.weather.sh.modiDate,
                                 all.x = TRUE,by="modiDate")
ggplot(data.htl.hour.ac.env.wide[!is.na(tempMode)&season=="Winter"],aes(x=as.factor(tempMode),y=runtime))+geom_boxplot()

####温度聚类命名####
#聚类核实
data.htl.hour.ac.env.wide[,c("season","runtime","tempMode")][,.(runtime=mean(runtime,na.rm=TRUE)),by=paste(season,tempMode)]

info.htl.tempPattern.name<-data.table(seasonTemp=c("Winter_1","Winter_2","Winter_3","Winter_4","Winter_5",
                                                       "Summer_1","Summer_2","Summer_3","Summer_4"),
                                        tempPatternName=c("Mid","High","High_Night","Low_Night","Low",
                                                            "Low","Low_Night","Mid","High"))
data.htl.hour.ac.env.wide$seasonTemp<-paste(data.htl.hour.ac.env.wide$season,data.htl.hour.ac.env.wide$tempMode,sep="_")
data.htl.hour.ac.env.wide<-merge(x=data.htl.hour.ac.env.wide,y=info.htl.tempPattern.name,all.x=TRUE,by="seasonTemp")


data.htl.hour.ac.env.wide$seasonTemp<-NULL

table(data.htl.hour.ac.env.wide[,c("season","tempPatternName")])

