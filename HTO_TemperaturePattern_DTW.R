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
             plot=ggplot(data=tmp.plot.heatMap[tempMode==k],
                         aes(x=variable,y=labelDevDate,fill=value,group=as.factor(tempMode)))+
               geom_raster(interpolate = TRUE)+
               scale_fill_gradient(limits = c(range$stats[1],range$stats[5]),low = "green",high = "red")+
               facet_wrap(~ tempMode, nrow = 2)+theme_classic()+
               theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                     axis.ticks.y=element_blank()),width=4,height=3,dpi=80)
    }
    
    
  }
}


# 正式聚类
data.htl.hour.ac.env.wide$tempMode<-as.numeric(NA)
data.htl.hour.ac.env.wide[season=="Winter"&maxMode %in% conditionSelect[["Winter"]]]$tempMode<-
  (pamk(distTempDtwWinter,diss=TRUE,krange = 5,criter = "ch",usepam = TRUE))$pamobject$clustering
data.htl.hour.ac.env.wide[season=="Summer"&maxMode %in% conditionSelect[["Summer"]]]$tempMode<-
  (pamk(distTempDtwWinter,diss=TRUE,krange = 5,criter = "ch",usepam = TRUE))$pamobject$clustering

