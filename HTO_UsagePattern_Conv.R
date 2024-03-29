#本脚本用于实现酒店空调使用行为聚类，基于Li et.al. 2021的方法

usingBldgId<-c("SH_01","SH_02","SH_03","SH_04","SH_05")
data.htl.hour.ac.conv.usage<-data.htl.hour.ac[bldgId%in% usingBldgId]

data.htl.hour.ac.conv.usage<-data.htl.hour.ac.conv.usage[,c("deviceId","datetime","hour","onRatio","maxMode")]
data.htl.hour.ac.conv.usage[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.conv.usage[,":="(modiDate=date(modiDatetime),modiHour=hour(modiDatetime),labelDevDate=paste(deviceId,date(modiDatetime),sep="_"))]
####宽数据转换####
data.htl.hour.ac.conv.usage.wide<-
  dcast(data.htl.hour.ac.conv.usage[,c("labelDevDate","modiHour","onRatio")],
        formula = labelDevDate~modiHour,value.var = "onRatio")%>%as.data.table(.)
nn1<-data.htl.hour.ac.conv.usage[,.(maxMode=getMode(maxMode[onRatio>0.25],na.rm = TRUE)[1]
                               ),by=labelDevDate]
data.htl.hour.ac.conv.usage.wide<-merge(x=data.htl.hour.ac.conv.usage.wide,y=nn1,all.x=TRUE,by="labelDevDate")
names(data.htl.hour.ac.conv.usage.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""),"maxMode")

####统计使用时长及在室####
#空调使用时长精确统计
data.htl.hour.ac.conv.usage.wide$runtime<-apply(data.htl.hour.ac.conv.usage.wide[,c(paste("h+14_",0:23,sep = ""))],MARGIN = 1,sum,na.rm=TRUE)
#按大于半小时统计空调使用时长
data.htl.hour.ac.conv.usage.wide$runtimeHr<-apply(data.htl.hour.ac.conv.usage.wide[,c(paste("h+14_",0:23,sep = ""))],
                                                  MARGIN = 1,function(x){length(x[x>0.5&!is.na(x)])})
#在室统计，仅能精确到小时
data.htl.hour.ac.conv.usage.wide$occuTime<-apply(data.htl.hour.ac.conv.usage.wide[,c(paste("h+14_",0:23,sep = ""))],
                                                 MARGIN = 1,function(x){length(x[!is.na(x)])})

ggplot(data.htl.hour.ac.conv.usage.wide[runtime>1],#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
       aes(x=runtime))+geom_density()

#NA值排除
data.htl.hour.ac.conv.usage.wide[,c(paste("h+14_",0:23,sep = ""))]<-
  data.htl.hour.ac.conv.usage.wide[,c(paste("h+14_",0:23,sep = ""))]%>%mutate_all(funs(ifelse(is.na(.),0, .)))%>%as.data.table()
nrow(data.htl.hour.ac.conv.usage.wide[runtime<=1])

data.htl.hour.ac.conv.usage.wide<-data.htl.hour.ac.conv.usage.wide[runtime>1]

#时间轴查看
data.htl.hour.ac.conv.usage.wide[c(1:20)]%>%melt(.,id.var=c("labelDevDate","runtime","runtimeHr","occuTime"))%>%{
  ggplot(.,#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
         aes(x=variable,y=value,group=as.factor(labelDevDate),color=as.factor(labelDevDate),lty=as.factor(labelDevDate)))+geom_line()
}

####月份及其他####
data.htl.hour.ac.conv.usage.wide[,month:=as.numeric(substr(labelDevDate,18,19))]
# stat.htl.hour.ac.usage.mode<-data.htl.hour.ac.conv.usage.wide[.,()]
stat.htl.hour.ac.usage.mode<-table(data.htl.hour.ac.conv.usage.wide[,c("maxMode","month")])
# data.htl.hour.ac.toy.wide[,lapply(.SD, mean,na.rm=TRUE),.SDcols=c(paste("h+14_",0:23,sep = ""),"runtime"),by=usageMode]
ggplot(data=data.htl.hour.ac[onRatio>0.5],aes(x=as.factor(maxMode),y=sumElec))+geom_boxplot()+ylim(c(0,20000000))
data.htl.hour.ac.conv.usage.wide$season<-apply(data.htl.hour.ac.conv.usage.wide[,"month"],MARGIN = 1,getHotelSeason)

####聚类指标分析####
wssClusterEvaluate(data = data.htl.hour.ac.conv.usage.wide[season=="Winter",c(paste("h+14_",0:23,sep = ""))],
                   maxIter = 1000,
                   maxK = 10)
pamkClusterEvaluate(
  data = data.htl.hour.ac.conv.usage.wide[season=="Winter",c(paste("h+14_",0:23,sep = ""))],#distDtw,#8-22时+runtime
  criter = "ch",
  startK = 2,
  endK = 10,withPam = FALSE,isDistance = FALSE
)

data.htl.hour.ac.conv.usage.wide$convUsageMode<-as.numeric(NA)
seasonSelect<-c("Summer","Winter")
kSize<-c(3:7)
conditionSelect<-list(Summer=c(1,3,4),Winter=c(1,2))

for(i in seasonSelect){
  for(j in kSize){
    
    data.htl.hour.ac.conv.usage.wide[season==i&maxMode %in% conditionSelect[[i]]]$convUsageMode<-(pamk(data.htl.hour.ac.conv.usage.wide[season==i&maxMode %in% conditionSelect[[i]],c(paste("h+14_",0:23,sep = ""))],
                                                          krange = j,criter = "ch",usepam = TRUE))$pamobject$clustering
    
    merge(x=data.htl.hour.ac.conv.usage.wide[season==i&maxMode %in% conditionSelect[[i]],lapply(.SD,mean,na.rm=TRUE),
                                    .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime","runtimeHr","occuTime") ,by=convUsageMode],
          y=data.htl.hour.ac.conv.usage.wide[season==i&maxMode %in% conditionSelect[[i]],.(count=length(runtime)),by=convUsageMode],all.x = TRUE,
          by.x="convUsageMode",by.y = "convUsageMode")%>%{ 
            write.xlsx(.,file=paste(j,i,"overview.xlsx",sep = "_"))
            cat(paste(names(.),collapse = " "),"\n")
        melt(.,id.var=c("convUsageMode","runtime","runtimeHr","occuTime","count"))%>%{
          cat(paste(i,j,paste(unique(.$runtime),collapse = " "),"\n"))
        ggsave(file=paste(j,i,"MeanValue.png",sep = "_"),
               plot = ggplot(data=.,aes(x=variable,y=value,color=as.factor(convUsageMode),group=convUsageMode))+geom_line(), 
               width=16,height = 5,dpi = 100)
        }
        }
    
    #类型过多，聚合绘图无法区分
    #data.htl.hour.ac.conv.usage.wide[season==i,c(paste("h+14_",0:23,sep = ""),"convUsageMode")]%>%melt(.,id.var=c("labelDevDate","convUsageMode"))%>%{
    #  cat(table(.$convUsageMode),"\n")
    #  ggsave(file=paste(j,i,"convOverview.png",sep = "_"),
    #         plot = ggplot(data = .,aes(x=variable,y=value,color=convUsageMode,group=labelDevDate,alpha=0.05))+geom_line()+facet_wrap(.~convUsageMode,ncol=1), 
    #         width=6,height = j*3,dpi = 200)
    #}
    
  }
}

##看一看分类的情况
# 注意宽数据中的label格式"SH_01_66-6C_2019-08-20"
# 注意原始数据中的label格式"CD_01_10-37_19-03-10_22"

nn1<-data.htl.hour.ac.dtw.usage.wide[season==i&maxMode %in% conditionSelect[[i]]& dtwUsageMode==2]$labelDevDate#%>%{paste(substring(.$labelDevDate,1,12),substring(.$labelDevDate,15),sep = "")}
data.htl.hour.ac.conv.usage[labelDevDate %in% nn1]%>%{table(.$labelDevDate)}%>%as.data.table()%>%{mean(.$N)}

data.htl.hour.ac.dtw.usage.wide[season==i&maxMode %in% conditionSelect[[i]],lapply(.SD,mean,na.rm=TRUE),
                                 .SDcols=c(paste("h+14_",0:23,sep = ""),"runtime","runtimeHr","occuTime"),by=dtwUsageMode]%>%
  melt(.,id.var=c("dtwUsageMode","runtime","runtimeHr","occuTime"))%>%{
  # cat(paste(i,j,paste(unique(.$runtime),collapse = " "),"\n"))
  ggplot(data=.,aes(x=variable,y=value,color=as.factor(dtwUsageMode),alpha=0.0001,group=dtwUsageMode))+geom_line()
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
