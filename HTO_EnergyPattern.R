

usingBldgId<-c("SH_01","SH_02","SH_03","SH_04","SH_05")
data.htl.hour.ac.energy<-data.htl.hour.ac[bldgId%in% usingBldgId]

#采用优化后的日划分
data.htl.hour.ac.energy[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.energy[,":="(modiDate=date(modiDatetime),
                              modiHour=hour(modiDatetime),labelDevModiDate=paste(deviceId,date(modiDatetime),sep="_"))]


#按照HZNU的方法，聚类属性为日空调能耗，日空调使用时长，逐时能耗标准差
data.htl.day.ac.energy<-data.htl.hour.ac.energy[,.(date=format(modiDate[1],format="%Y-%m-%d"),
                                                   sumElec=sum(sumElec,na.rm=TRUE)/(3600*1000),
                                                   #runtime=sum(runtime,na.rm = TRUE), #runtime从行为数据集中直接合并
                                                   stdDevElec=sd(sumElec/(3600*1000),na.rm = TRUE),
                                                   logCount=length(datetime)
                                                   ),by=labelDevModiDate]

#合并一些标签

data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,y=data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","occuTime")],
                              all.x = TRUE,by.x = "labelDevModiDate",by.y="labelDevDate")


#注意这个label的划分
#行为是SH_01_65-78_2019-11-18
#且行为的labelDevDate是modiDate
data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,
                              y=data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","runtime","season","maxMode","seasonMode","month","isBizday","dtwUsageMode")],
                              all.x=TRUE,by.x = "labelDevModiDate",by.y="labelDevDate")


####聚类属性的归一化####
data.htl.day.ac.energy[,":="(sdSumElec=as.numeric(NA),sdStdDevElec=as.numeric(NA),sdRuntime=as.numeric(NA))]
for(i in c("Summer","Winter")){
  data.htl.day.ac.energy[season==i]$sdSumElec<-normalize(data.htl.day.ac.energy[season==i]$sumElec,upper = 0.9,lower = 0.1,intercept = 0.1)
  data.htl.day.ac.energy[season==i]$sdRuntime<-normalize(data.htl.day.ac.energy[season==i]$runtime,upper = 0.9,lower = 0.1,intercept = 0.1)
  data.htl.day.ac.energy[season==i]$sdStdDevElec<-normalize(data.htl.day.ac.energy[season==i]$stdDevElec,upper = 0.9,lower = 0.1,intercept = 0.1)
}


####聚类数评估####
pamkClusterEvaluate(data = data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")],
                    startK = 2,endK = 10,criter = "ch",withPam = FALSE,isDistance = FALSE)

wssClusterEvaluate(data = data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")],
                   maxIter = 1000,
                   maxK = 10)
clusterEnergySummerWss<-fviz_nbclust(x=data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")],
                                   FUN = cluster::pam, method = "wss", k.max = 10)

clusterEnergyEvaWinterMeans<-NbClust(data = data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")], 
                               min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)

clusterEnergyEvaWinter<-NbClust(data = data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")], 
                                     min.nc = 2, max.nc = 10, method = "centroid", index = "all", alphaBeale = 0.1)

####全部试聚类####
data.htl.day.ac.energy$energyPattern<-as.numeric(NA)

kSize<-c(3:5)
seasonSelect<-c("Summer")#,"Winter"
conditionSelect<-list(Summer=c(1,3,4),Winter=c(1,2))

for(i in seasonSelect){
  for(j in kSize){

    data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]]$energyPattern<-(pamk(data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")],
                                                                                             krange = j,criter = "ch",usepam = FALSE))$pamobject$clustering
    
    merge(x=data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]],lapply(.SD,mean,na.rm=TRUE),
                                            .SDcols=c("sumElec","stdDevElec","runtime","occuTime") ,by=energyPatternMore],#
          y=data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]],.(count=length(runtime)),by=energyPatternMore],all.x = TRUE,
          by.x="energyPatternMore",by.y = "energyPatternMore")%>%{ 
            write.xlsx(.,file=paste(j,i,"overview_EGY.xlsx",sep = "_"))
            cat(paste(names(.),collapse = " "),"\n")
            melt(.,id.var=c("energyPattern","runtime","occuTime","count"))%>%{
              cat(paste(i,j,paste(unique(.$runtime),collapse = " "),"\n"))
              
            }
          }
    
    
  }
}

####正式聚类####
data.htl.day.ac.energy$energyPattern<-as.numeric(NA)
data.htl.day.ac.energy$energyPatternMore<-data.htl.day.ac.energy$energyPattern

i<-"Summer"
data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]]$energyPattern<-(pamk(data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")],
                                                                                         krange = 4,criter = "ch",usepam = TRUE))$pamobject$clustering
i<-"Winter"
data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]]$energyPatternMore<-(pamk(data.htl.day.ac.energy[season==i&maxMode %in% conditionSelect[[i]]][,c("sdSumElec","sdStdDevElec","sdRuntime")],
                                                                                         krange = 5,criter = "ch",usepam = TRUE))$pamobject$clustering

####温度和行为聚类的合并####
data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,y=data.htl.hour.ac.dtw.usage.wide[,c("labelDevDate","patternName")],
                              all.x=TRUE,by.x="labelDevModiDate",by.y="labelDevDate")
data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,y=data.htl.hour.ac.env.wide[,c("labelDevDate","tempMode")],
                              all.x=TRUE,by.x="labelDevModiDate",by.y="labelDevDate")


####聚类特征分析####
ggplot(data.htl.day.ac.energy[!is.na(energyPattern)&season=="Winter"],aes(x=as.factor(energyPatternMore),y=sumElec/runtime))+geom_boxplot()+
  stat_summary(fun.y = "mean",geom = "line",group=1)+facet_wrap(.~season)

##统计日内设定温度
data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,
                                 y = data.htl.hour.ac.env[,.(mSetTemp=mean(mSetTemp[onRatio>0.25],na.rm=TRUE)),by=labelDevDate],
                                 all.x = TRUE,by.x="labelDevModiDate",by.y="labelDevDate")

table(data.htl.day.ac.energy[,c("energyPatternMore","patternName","season")])



data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,y=data.htl.weather.sh.modiDate,
                                 all.x = TRUE,by.x="date",by.y="modiDate")

####能耗聚类命名####
#聚类核实
data.htl.day.ac.energy[,c("season","sumElec","energyPatternMore")][,.(sumElec=mean(sumElec,na.rm=TRUE)),by=paste(season,energyPatternMore)]

info.htl.energyPattern.name<-data.table(seasonEnergy=c("Winter_1","Winter_2","Winter_3","Winter_4","Winter_5",
                                                       "Summer_1","Summer_2","Summer_3","Summer_4"),
                                        energyPatternName=c("HE_MT","ME_MT","ME_ST","LE","HE_LT",
                                                            "LE","ME","HE_LT","HE_MT"))
data.htl.day.ac.energy$seasonEnergy<-paste(data.htl.day.ac.energy$season,data.htl.day.ac.energy$energyPatternMore,sep="_")
data.htl.day.ac.energy<-merge(x=data.htl.day.ac.energy,y=info.htl.energyPattern.name,all.x=TRUE,by="seasonEnergy")


data.htl.day.ac.energy$seasonEnergy<-NULL



