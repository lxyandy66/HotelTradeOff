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


####在室宽数据统计####
#此处label为"SH_01_65-78_2019-02-20
data.htl.hour.occ.wide<-
  dcast(data.htl.hour.ac.conv.usage[,c("labelDevDate","modiHour","onRatio")],
        formula = labelDevDate~modiHour,value.var = "onRatio")%>%as.data.table(.)
names(data.htl.hour.occ.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""))
data.htl.hour.occ.wide[,c(paste("h+14_",0:23,sep = ""))]<-
  data.htl.hour.occ.wide[,c(paste("h+14_",0:23,sep = ""))]%>%mutate_all(funs(ifelse(is.na(.),0, 1)))%>%as.data.table()
#合并对应的季节和使用模式
#使用模式数据集label为"SH_01_65-78_2019-02-27
data.htl.hour.occ.wide<-merge(data.htl.hour.occ.wide,data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","dtwUsageModeLess","maxMode","season")],
                              all.x = TRUE,by.x = "labelDevDate",by.y = "labelDevDate")
data.htl.hour.occ.wide.winter<-data.htl.hour.occ.wide[season=="Winter"&!is.na(dtwUsageModeLess),-c("maxMode","season")][,lapply(.SD,mean,na.rm=TRUE),
                                .SDcols=c(paste("h+14_",0:23,sep = "")),by=dtwUsageModeLess]
data.htl.hour.occ.wide.winter.long<-melt(data.htl.hour.occ.wide.winter,id.vars = "dtwUsageModeLess")
names(data.htl.hour.occ.wide.winter.long)[3]<-"occ"
#提取冬季使用模式
data.htl.hour.dtw.usage.winter<-data.htl.hour.ac.dtw.usage.wide[season=="Winter"&!is.na(dtwUsageModeLess),c(paste("h+14_",0:23,sep = ""),
                                                                                                            "dtwUsageModeLess")][,lapply(.SD,mean,na.rm=TRUE),
                                                                                                                         .SDcols=c(paste("h+14_",0:23,sep = "")),by=dtwUsageModeLess]
data.htl.hour.dtw.usage.winter.long<-melt(data.htl.hour.dtw.usage.winter,id.vars = "dtwUsageModeLess")
names(data.htl.hour.dtw.usage.winter.long)[3]<-"usage"
#合并
data.htl.hour.dtw.usage.winter.long[,mergeLabel:=paste(dtwUsageModeLess,variable,sep = "_")]
data.htl.hour.occ.wide.winter.long[,mergeLabel:=paste(dtwUsageModeLess,variable,sep = "_")]
data.htl.hour.occ.check<-merge(x=data.htl.hour.occ.wide.winter.long,y=data.htl.hour.dtw.usage.winter.long[,c("mergeLabel","usage")],
                               all.x = TRUE,by.x = "mergeLabel",by.y = "mergeLabel")
#删除多余的
rm(data.htl.hour.occ.wide.winter.long,data.htl.hour.dtw.usage.winter.long)

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
data.htl.hour.ac.dtw.usage.wide[c(1:20)]%>%melt(.,id.var=c("labelDevDate","runtime","runtimeHr","occuTime"))%>%{
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
require(doParallel)
# Create parallel workers
cl <- makeCluster(detectCores())
invisible(clusterEvalQ(cl, library(dtwclust)))
registerDoParallel(cl)

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

clusterType<-c("dtw","Euclidean")
kSize<-c(3:7)
season<-c("Summer","Winter")
conditionSelect<-list(Summer=c(1,3,4),Winter=c(1,2))
data.htl.hour.ac.dtw.usage.wide$dtwUsageMode<-as.numeric(NA)



for(i in season){
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
            write.xlsx(.,file=paste(j,i,"overview.xlsx",sep = "_"))
            cat(paste(names(.),collapse = " "),"\n")
            melt(.,id.var=c("dtwUsageMode","runtime","runtimeHr","occuTime","count"))%>%{
              cat(paste(i,j,paste(unique(.$runtime),collapse = " "),"\n"))
              ggsave(file=paste(j,i,"MeanValue.png",sep = "_"),
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
