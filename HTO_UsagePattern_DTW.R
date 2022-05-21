#本脚本用于实现酒店空调使用行为聚类，基于DTW方法

usingBldgId<-c("SH_01","SH_02","SH_03","SH_04","SH_05")
data.htl.hour.ac.conv.usage<-data.htl.hour.ac[bldgId%in% usingBldgId]

data.htl.hour.ac.conv.usage<-data.htl.hour.ac.conv.usage[,c("deviceId","datetime","hour","onRatio","maxMode")]
data.htl.hour.ac.conv.usage[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.conv.usage[,":="(modiDate=date(modiDatetime),modiHour=hour(modiDatetime),labelDevDate=paste(deviceId,date(modiDatetime),sep="_"))]
####宽数据转换####
data.htl.hour.ac.dtw.usage.wide<-
  dcast(data.htl.hour.ac.conv.usage[,c("labelDevDate","modiHour","onRatio")],
        formula = labelDevDate~modiHour,value.var = "onRatio")%>%as.data.table(.)
nn1<-data.htl.hour.ac.conv.usage[,.(maxMode=getMode(maxMode[onRatio>0.25],na.rm = TRUE)[1]
),by=labelDevDate]
data.htl.hour.ac.dtw.usage.wide<-merge(x=data.htl.hour.ac.dtw.usage.wide,y=nn1,all.x=TRUE,by="labelDevDate")
names(data.htl.hour.ac.dtw.usage.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""),"maxMode")

#data.htl.hour.ac.dtw.usage.wide<-merge(x=data.htl.hour.ac.dtw.usage.wide,
#                                       y=data.htl.hour.ac.conv.usage.wide[,c("labelDevDate","occuTime")],
#                                       all.x=TRUE,by = "labelDevDate")

####在室情况数据统计####
#此处label为"SH_01_65-78_2019-02-20
data.htl.hour.occ.wide<-
  dcast(data.htl.hour.ac.conv.usage[,c("labelDevDate","modiHour","onRatio")],
        formula = labelDevDate~modiHour,value.var = "onRatio")%>%as.data.table(.)
names(data.htl.hour.occ.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""))
data.htl.hour.occ.wide[,c(paste("h+14_",0:23,sep = ""))]<-
  data.htl.hour.occ.wide[,c(paste("h+14_",0:23,sep = ""))]%>%mutate_all(funs(ifelse(is.na(.),0, 1)))%>%as.data.table()
#合并对应的季节和使用模式
#使用模式数据集label为"SH_01_65-78_2019-02-27
data.htl.hour.occ.wide<-merge(data.htl.hour.occ.wide,data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","dtwUsageMode","maxMode","season")],
                              all.x = TRUE,by.x = "labelDevDate",by.y = "labelDevDate")
data.htl.hour.occ.wide<-data.htl.hour.occ.wide[!is.na(dtwUsageMode),-c("maxMode")][,lapply(.SD,mean,na.rm=TRUE),
                                .SDcols=c(paste("h+14_",0:23,sep = "")),by=paste(dtwUsageMode,season,sep = "_")]
names(data.htl.hour.occ.wide.winter.long)[3]<-"occ"



####统计使用时长及在室####
#空调使用时长精确统计
data.htl.hour.ac.dtw.usage.wide$runtime<-apply(data.htl.hour.ac.dtw.usage.wide[,c(paste("h+14_",0:23,sep = ""))],MARGIN = 1,sum,na.rm=TRUE)
#按大于半小时统计空调使用时长
data.htl.hour.ac.dtw.usage.wide$runtimeHr<-apply(data.htl.hour.ac.dtw.usage.wide[,c(paste("h+14_",0:23,sep = ""))],
                                                  MARGIN = 1,function(x){length(x[x>0.5&!is.na(x)])})
#在室统计，仅能精确到小时
data.htl.hour.ac.dtw.usage.wide$occuTime<-apply(data.htl.hour.ac.dtw.usage.wide[,c(paste("h+14_",0:23,sep = ""))],
                                                 MARGIN = 1,function(x){length(x[!is.na(x)])})

ggplot(data.htl.hour.ac.dtw.usage.wide[runtime>1],#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
       aes(x=runtime))+geom_density()

#NA值排除

data.htl.hour.ac.dtw.usage.wide[,c(paste("h+14_",0:23,sep = ""))]<-
  data.htl.hour.ac.dtw.usage.wide[,c(paste("h+14_",0:23,sep = ""))]%>%mutate_all(funs(ifelse(is.na(.),0, .)))%>%as.data.table()
nrow(data.htl.hour.ac.dtw.usage.wide[runtime<=1])

data.htl.hour.ac.dtw.usage.wide<-data.htl.hour.ac.dtw.usage.wide[runtime>1]

#时间轴查看
data.htl.hour.ac.dtw.usage.wide[c(1:2)]%>%melt(.,id.var=c("labelDevDate","runtime","runtimeHr","occuTime","maxMode"))%>%{
  ggplot(.,#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
         aes(x=variable,y=value,group=as.factor(labelDevDate),color=as.factor(labelDevDate),lty=as.factor(labelDevDate)))+geom_line()
}

####月份及其他####
data.htl.hour.ac.dtw.usage.wide[,month:=as.numeric(substr(labelDevDate,18,19))]
# stat.htl.hour.ac.usage.mode<-data.htl.hour.ac.dtw.usage.wide[.,()]
stat.htl.hour.ac.usage.mode<-table(data.htl.hour.ac.dtw.usage.wide[,c("maxMode","month")])
# data.htl.hour.ac.toy.wide[,lapply(.SD, mean,na.rm=TRUE),.SDcols=c(paste("h+14_",0:23,sep = ""),"runtime"),by=usageMode]
ggplot(data=data.htl.hour.ac[onRatio>0.5],aes(x=as.factor(maxMode),y=sumElec))+geom_boxplot()+ylim(c(0,20000000))
data.htl.hour.ac.dtw.usage.wide$season<-apply(data.htl.hour.ac.dtw.usage.wide[,"month"],MARGIN = 1,getHotelSeason)

####聚类指标分析####
#DTW版暂略

data.htl.hour.ac.dtw.usage.wide$dtwUsageMode<-as.numeric(NA)
season<-c("Summer","Winter")
kSize<-c(3:7)

pamkClusterEvaluate(data = distDtwWinter,startK = 2,endK = 10,criter = "ch",withPam = FALSE,isDistance = TRUE)


#直接dtwCluster试聚类
#作废
require(doParallel)
# Create parallel workers
cl <- makeCluster(detectCores())
invisible(clusterEvalQ(cl, library(dtwclust)))
registerDoParallel(cl)
library(bigmemory)

nn1<-tsclust(data.htl.hour.ac.dtw.usage.wide[,c(paste("h+14_",0:23,sep = ""))],type = "partitional",k=4,distance = "dtw", 
             centroid = "pam",#seed=711,
             control = partitional_control(iter.max = 200L),
             args = tsclust_args(dist = list(window.type = "sakoechiba",window.size=2)))#,window.type = "sakoechiba",window.size=2

data.htl.hour.ac.dtw.usage.wide$usageMode<-as.factor(nn@cluster)
data.htl.hour.ac.dtw.usage.wide[,lapply(.SD, mean,na.rm=TRUE),.SDcols=c(paste("h+14_",0:23,sep = ""),"runtime"),by=usageMode]%>%
  melt(.,id.var=c("usageMode","runtime"))%>%{
    ggplot(data=.,aes(x=variable,y=value,color=usageMode,group=usageMode))+geom_line()
  }

browser()

#分别计算，仅计算一次
distDtwSummer<-dist(data.htl.hour.ac.dtw.usage.wide[season=="Summer"&maxMode %in% conditionSelect[["Summer"]],c(paste("h+14_",0:23,sep = ""))], method="dtw",window.type = "sakoechiba",window.size=2)
distDtwWinter<-dist(data.htl.hour.ac.dtw.usage.wide[season=="Winter"&maxMode %in% conditionSelect[["Winter"]],c(paste("h+14_",0:23,sep = ""))], method="dtw",window.type = "sakoechiba",window.size=2)


#聚类数评估
require(doParallel)
# Create parallel workers
cl <- makeCluster(detectCores())
invisible(clusterEvalQ(cl, library(cluster)))
registerDoParallel(cl)

clusterTestSummerWss<-fviz_nbclust(x=data.htl.hour.ac.dtw.usage.wide[season==i&maxMode %in% conditionSelect[[i]]][,c(paste("h+14_",0:23,sep = ""))],
              FUN = cluster::pam, method = "wss", diss = distDtwSummer, k.max = 10)

clusterTestSummerMeans<-NbClust(data = data.htl.hour.ac.dtw.usage.wide[season==i&maxMode %in% conditionSelect[[i]]][,c(paste("h+14_",0:23,sep = ""))], 
                     diss = distDtwSummer, distance = NULL,min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)

#聚类
clusterType<-c("dtw","Euclidean")
kSize<-c(3:7)
seasonSelect<-c("Summer")#,"Winter"
conditionSelect<-list(Summer=c(1,3,4),Winter=c(1,2))
data.htl.hour.ac.dtw.usage.wide$dtwUsageMode<-as.numeric(NA)

pamk(localDist,diss=TRUE,krange = j,criter = "ch",usepam = FALSE)


for(i in seasonSelect){
  for(j in kSize){
    if(i=="Summer"){
      localDist<-distDtwSummer
    }else{
      localDist<-distDtwWinter
    }
    
    data.htl.hour.ac.dtw.usage.wide[season==i&maxMode %in% conditionSelect[[i]]]$dtwUsageMode<-(pamk(localDist,diss=TRUE,krange = j,criter = "ch",usepam = TRUE))$pamobject$clustering
    
    merge(x=data.htl.hour.ac.dtw.usage.wide[season==i&maxMode %in% conditionSelect[[i]],lapply(.SD,mean,na.rm=TRUE),
                                             .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime","runtimeHr","occuTime") ,by=dtwUsageMode],
          y=data.htl.hour.ac.dtw.usage.wide[season==i&maxMode %in% conditionSelect[[i]],.(count=length(runtime)),by=dtwUsageMode],all.x = TRUE,
          by.x="dtwUsageMode",by.y = "dtwUsageMode")%>%{ 
            write.xlsx(.,file=paste(j,i,"overview_DTW.xlsx",sep = "_"))
            cat(paste(names(.),collapse = " "),"\n")
            melt(.,id.var=c("dtwUsageMode","runtime","runtimeHr","occuTime","count"))%>%{
              cat(paste(i,j,paste(unique(.$runtime),collapse = " "),"\n"))
              ggsave(file=paste(j,i,"MeanValue_DTW.png",sep = "_"),
                     plot = ggplot(data=.,aes(x=variable,y=value,color=as.factor(dtwUsageMode),group=dtwUsageMode))+geom_line(), 
                     width=16,height = 5,dpi = 100)
            }
          }
    
    #类型过多，聚合绘图无法区分
    #data.htl.hour.ac.dtw.usage.wide[season==i,c(paste("h+14_",0:23,sep = ""),"dtwUsageMode")]%>%melt(.,id.var=c("labelDevDate","dtwUsageMode"))%>%{
    #  cat(table(.$dtwUsageMode),"\n")
    #  ggsave(file=paste(j,i,"convOverview.png",sep = "_"),
    #         plot = ggplot(data = .,aes(x=variable,y=value,color=dtwUsageMode,group=labelDevDate,alpha=0.05))+geom_line()+facet_wrap(.~dtwUsageMode,ncol=1), 
    #         width=6,height = j*3,dpi = 200)
    #}
    
  }
}

####正式聚类####
data.htl.hour.ac.dtw.usage.wide[,c("dtwUsageModeLess","dtwUsageModeMore")]<-as.numeric(NA)

data.htl.hour.ac.dtw.usage.wide[season=="Summer"&maxMode %in% conditionSelect[["Summer"]]]$dtwUsageMode<-
  (pamk(distDtwSummer,diss=TRUE,krange = 4,criter = "ch",usepam = TRUE))$pamobject$clustering
data.htl.hour.ac.dtw.usage.wide[season=="Summer"&maxMode %in% conditionSelect[["Summer"]]]$dtwUsageModeMore<-
  (pamk(distDtwSummer,diss=TRUE,krange = 5,criter = "ch",usepam = TRUE))$pamobject$clustering

data.htl.hour.ac.dtw.usage.wide[season=="Winter"&maxMode %in% conditionSelect[["Winter"]]]$dtwUsageModeLess<-
  (pamk(distDtwWinter,diss=TRUE,krange = 3,criter = "ch",usepam = TRUE))$pamobject$clustering
data.htl.hour.ac.dtw.usage.wide[season=="Winter"&maxMode %in% conditionSelect[["Winter"]]]$dtwUsageMode<-
  (pamk(distDtwWinter,diss=TRUE,krange = 4,criter = "ch",usepam = TRUE))$pamobject$clustering
data.htl.hour.ac.dtw.usage.wide[,c("dtwUsageModeLess","dtwUsageModeMore")]<-NULL

####聚类结束，进行特征统计####
range(as.Date(substring(data.htl.hour.ac.dtw.usage.wide$labelDevDate,13)))
# 日期范围 [1] "2018-12-31" "2020-01-31"
data.htl.holiday<-read.xlsx(file="HolidayList_201801-202001.xlsx",sheetIndex = 1)%>%as.data.table()
data.htl.holiday$date<-as.Date(data.htl.holiday$date)
#查看数据完整度
data.table(bldg=substring(data.htl.hour.ac.dtw.usage.wide$labelDevDate,1,5),
           month=data.htl.hour.ac.dtw.usage.wide$month)%>%table()
# month
# bldg       1    2    3    4    5    6    7    8    9   10   11   12
# SH_01 1759  929 1890 1194 1281 2181 2586 2685 1811 1092 1312 2142
# SH_02 1049  755 1400  780  992 1664 2034 2207 1400  924  958 1588
# SH_03    0  556 1008  696   99   87   88   87   84   40    0    0
# SH_04 1087  904  796  474  520  744 1077 1144  792  534  471  819
# SH_05 1431 1074  997  501  844 1016 1483 1524 1361  975  539  677

#合并节假日标签
#基本的周末定义
data.htl.hour.ac.dtw.usage.wide[,isBizday:=isWeekday(as.Date(substring(labelDevDate,13)))]
data.htl.hour.ac.dtw.usage.wide[as.Date(substring(labelDevDate,13))%in% data.htl.holiday[isWorkday==FALSE]$date]$isBizday<-FALSE
data.htl.hour.ac.dtw.usage.wide[as.Date(substring(labelDevDate,13))%in% data.htl.holiday[isWorkday==TRUE]$date]$isBizday<-TRUE

#除去数据不全的SH-03后，用于聚类的数据共44011
data.htl.hour.ac.dtw.usage.wide[substring(labelDevDate,1,5)!="SH_03"&!is.na(dtwUsageMode)]%>%{
  .$bldgId<-substring(.$labelDevDate,1,5)
  ggplot(data=.,aes(x=bldgId,color=dtwUsageMode))+geom_bar(position = 'fill')
  table(.[,c("season","dtwUsageMode","isBizday","bldgId")])%>%View
}

####看一看不同酒店的模式在不同情况的使用率####

# 合并聚类名称
data.htl.hour.ac.dtw.usage.wide[,seasonMode:=paste(season,dtwUsageMode,sep = "_")]
info.htl.hour.usage.patternName<-data.table(originalSeasonMode=c("Winter_1","Winter_2","Winter_3","Winter_4","Summer_1","Summer_2","Summer_3","Summer_4" ),
                                           patternName=c("Intermi","All-day","Evening","Night--","Intermi","Night--","Evening","All-day"))
info.htl.hour.usage.patternName<-data.table(originalSeasonMode=c("Win-1","Win-2","Win-3","Win-4","Sum-1","Sum-2","Sum-3","Sum-4" ),
                                           patternName=c("Intermittent","All-day","Evening","Night","Intermittent","Night","Evening","All-day"))
data.htl.hour.ac.dtw.usage.wide<-merge(x=data.htl.hour.ac.dtw.usage.wide,y=info.htl.hour.usage.patternName,all.x = TRUE,by.x = "seasonMode",by.y="originalSeasonMode")

#不同季节统计
stat.htl.hour.ac.usage.bldg.season<-data.htl.hour.ac.dtw.usage.wide[!is.na(patternName)&substring(labelDevDate,1,5)!="SH_03",lapply(.SD, mean,na.rm=TRUE),
                                                                     .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime","runtimeHr","occuTime"),
                                by=(labelSeasonModeBldg=paste(substring(season,1,3),patternName,substring(labelDevDate,1,5),sep="-"))]
stat.htl.hour.ac.usage.bldg.season[,":="(season=substring(labelSeasonModeBldg,1,3),bldgId=substring(labelSeasonModeBldg,13),
                                  dtwUsageMode=substring(labelSeasonModeBldg,5,11),seasonMode=substring(labelSeasonModeBldg,1,11))]
#是否工作日
stat.htl.hour.ac.usage.bldg.weekday<-data.htl.hour.ac.dtw.usage.wide[!is.na(patternName)&substring(labelDevDate,1,5)!="SH_03",lapply(.SD, mean,na.rm=TRUE),
                                                                    .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime","runtimeHr","occuTime"),
                                                                    by=(labelSeasonModeBldg=paste(substring(labelDevDate,1,5),patternName,isBizday,sep="-"))]
stat.htl.hour.ac.usage.bldg.weekday[,":="(bldgId=substring(labelSeasonModeBldg,1,5),
                                         dtwUsageMode=substring(labelSeasonModeBldg,7,13),isBizday=substring(labelSeasonModeBldg,15),bizdayMode=substring(labelSeasonModeBldg,7))]

stat.htl.hour.ac.usage.bldg.weekday<-data.htl.hour.ac.dtw.usage.wide[!is.na(patternName)&substring(labelDevDate,1,5)!="SH_03",
                                                                        .(bldgId=substring(labelDevDate,1,5)[1],
                                                                          isBizday=isBizday[1],
                                                                          season=season[1],
                                                                          count=length(runtime),patternName=patternName[1]),by=paste(substring(labelDevDate,1,5),patternName,isBizday,season)]

#不同季节和是否工作日
stat.htl.hour.ac.usage.bldg.season.weekday<-data.htl.hour.ac.dtw.usage.wide[!is.na(patternName)&substring(labelDevDate,1,5)!="SH_03",lapply(.SD, mean,na.rm=TRUE),
                                                                    .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime","runtimeHr","occuTime"),
                                                                    by=(labelSeasonModeBldg=paste(substring(season,1,3),dtwUsageMode,ifelse(isBizday,1,0),substring(labelDevDate,1,5),sep="-"))]
stat.htl.hour.ac.usage.bldg.season.weekday[,":="(season=substring(labelSeasonModeBldg,1,3),bldgId=substring(labelSeasonModeBldg,9),
                                         dtwUsageMode=substring(labelSeasonModeBldg,5,5),isBizday=as.logical(as.numeric(substring(labelSeasonModeBldg,7,7))),seasonMode=substring(labelSeasonModeBldg,1,5))]


stat.htl.hour.ac.usage.bldg.sh02[,-c("labelSeasonModeBldg","runtime","runtimeHr","occuTime","seasonMode","bizdayMode")]%>%#根据需要统计的数据确定标签
  melt(.,id.var=c("dtwUsageMode","isBizday"))%>%{#,lty=isBizday,"season""bldgId",
    ggplot(data = .,aes(x=variable,y=value,color=bldgId,group=bldgId))+geom_line()+facet_wrap(.~dtwUsageMode+isBizday,nrow=4)
  }

#SH_02大学城看看
统计数据
stat.htl.hour.ac.usage.bldg.sh02.count<-data.htl.hour.ac.dtw.usage.wide[!is.na(patternName)&substring(labelDevDate,1,5)=="SH_02",
                                                                        .(count=length(runtime),patternName=patternName[1],month=month[1]),by=paste(patternName,month)]
stat.htl.hour.ac.usage.bldg.sh02.count[,isSemester:=ifelse(month%in%c(1,2,7,8),FALSE,TRUE)]
stat.htl.hour.ac.usage.bldg.sh02<-data.htl.hour.ac.dtw.usage.wide[!is.na(patternName)&substring(labelDevDate,1,5)=="SH_02"]%>%
  .[,isSemester:=ifelse(month%in%c(1,2,7,8),FALSE,TRUE)]%>%.[!is.na(patternName),lapply(.SD, mean,na.rm=TRUE),
                                .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime","runtimeHr","occuTime"),
                                by=(labelSeasonModeBldg=paste(substring(season,1,3),patternName,ifelse(isSemester,1,0),sep="-"))]
stat.htl.hour.ac.usage.bldg.sh02[,":="(season=substring(labelSeasonModeBldg,1,3),
                                                 dtwUsageMode=substring(labelSeasonModeBldg,5,11),isBizday=as.logical(as.numeric(substring(labelSeasonModeBldg,13,13))))]

stat.htl.hour.ac.usage.bldg.sh02[,-c("labelSeasonModeBldg","runtime","runtimeHr","occuTime","seasonMode","bizdayMode")]%>%#根据需要统计的数据确定标签
  melt(.,id.var=c("dtwUsageMode","isBizday","season"))%>%{#,lty=isBizday,"season""bldgId",
    ggplot(data = .,aes(x=variable,y=value,color=isBizday,group=isBizday))+geom_line()+facet_wrap(.~dtwUsageMode+season,nrow=4)
  }

####酒店用季节获取，仅分春夏季####
getHotelSeason<-function(month){
  #由月份获取季节
  #异常部分
  month<-as.numeric(month)
  if(is.na(month)){
    warning("NA input, NA is returned",immediate. = TRUE)
    return(NA)
  }
  if(month<1|month>12){
    if(!is.numeric(month)){
      warning(paste(month," not a numeric, NA is returned",sep = ""),immediate. = TRUE)
      return(NA)
    }
    warning(paste(month," is out of month range, NA is returned",sep = ""),immediate. = TRUE)
    return(NA)
  }
  
  if(month %in% c(6:9))
    return("Summer")
  if(month %in% c(1:3,12))
    return("Winter")
  if(month %in% c(4,5))
    return("Spring")
  if(month %in% c(10,11))
    return("Autumn")
  warning(paste(month," general exception, NA is returned",sep = ""),immediate. = TRUE)
  return(NA)
  
}
