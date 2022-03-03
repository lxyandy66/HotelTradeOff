#用于测试动态距离规划的聚类方法
#仅一栋
library(dtw)

data.htl.hour.ac.toy<-data.htl.hour.ac[bldgId=="SH_05"]#[deviceId=="SH_05_66-01"]#

data.htl.hour.ac.toy.wide<-data.htl.hour.ac.toy[,c("deviceId","datetime","hour","onRatio")]
data.htl.hour.ac.toy.wide[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.toy.wide[,":="(modiDate=date(modiDatetime),modiHour=hour(modiDatetime),labelDevDate=paste(deviceId,date(modiDatetime),sep="_"))]
data.htl.hour.ac.toy.wide<-
  dcast(data.htl.hour.ac.toy.wide[,c("labelDevDate","modiHour","onRatio")],formula = labelDevDate~modiHour)
names(data.htl.hour.ac.toy.wide)<-c("labelDevDate",paste("h+14_",0:23,sep = ""))
data.htl.hour.ac.toy.wide$runtime<-apply(data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))],MARGIN = 1,sum,na.rm=TRUE)

ggplot(data.htl.hour.ac.toy.wide[runtime>1],#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
       aes(x=runtime))+geom_density()


data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))]<-
  data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))]%>%mutate_all(funs(ifelse(is.na(.),0, .)))%>%as.data.table()
nrow(data.htl.hour.ac.toy.wide[runtime<=1])

data.htl.hour.ac.toy.wide<-data.htl.hour.ac.toy.wide[runtime>1]
data.htl.hour.ac.toy.wide<-as.data.table(data.htl.hour.ac.toy.wide)

data.htl.hour.ac.toy.wide[c(2,4)]%>%melt(.,id.var=c("modiDate","runtime"))%>%{
  ggplot(.,#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
         aes(x=variable,y=value,group=as.factor(modiDate),color=as.factor(modiDate),lty=as.factor(modiDate)))+geom_line()
}

####聚类评估#

#距离评估demo

dm<-(matrix(10,4,4)+diag(rep(1,4)))[1:3,]
dist(dm, method="Euclidean")
dist(dm, method="dtw",window.type = "sakoechiba",window.size=1)
al<-dtw(dm,k=T,step=symmetric2,window.type = "sakoechiba",window.size=2) 	
al$localCostMatrix
al$distance

#dtw距离
dist(data.htl.hour.ac.toy.wide[c(2,4),c(paste("h+14_",0:23,sep = ""))], method="Euclidean")
nn<-dtw(as.matrix(data.htl.hour.ac.toy.wide[c(2,4),c(paste("h+14_",0:23,sep = ""))]),window.type = "none",window.size=2)
dtwPlotTwoWay(nn)

idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;
reference<-cos(idx)

a<-(data.htl.hour.ac.toy.wide[c(2),c(paste("h+14_",0:23,sep = ""))])%>%as.numeric()%>%ts
b<-(data.htl.hour.ac.toy.wide[c(4),c(paste("h+14_",0:23,sep = ""))])%>%as.numeric()%>%ts
alignment<-dtw(a,b,window.type = "sakoechiba",window.size=2,keep.internals = TRUE)
alignment$distance
dtwPlotTwoWay(alignment, xts=a,yts=b)

fviz_nbclust(x=data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))],
             FUNcluster = kmeans, method = "wss", diss = distDtw, k.max = 10)




clusterType<-c("dtw","Euclidean")
kSize<-c(3:7)
for(i in clusterType){
  for(j in kSize){
    if(i=="dtw"){
      distDtw<-dist(data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))], method="dtw",window.type = "sakoechiba",window.size=2)
    }else{
      distDtw<-dist(data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))], method="Euclidean")
    }
    
    data.htl.hour.ac.toy.wide$usageMode<-(pamk(distDtw,diss=TRUE,krange = j,criter = "ch",usepam = TRUE))$pamobject$clustering%>%as.factor()
    #聚类均值及运行时间
    data.htl.hour.ac.toy.wide[,lapply(.SD, mean,na.rm=TRUE),.SDcols=c(paste("h+14_",0:23,sep = ""),"runtime"),by=usageMode]%>%
      melt(.,id.var=c("usageMode","runtime"))%>%{
        cat(paste(i,j,paste(unique(.$runtime),collapse = " "),"\n"))
        ggsave(file=paste(j,i,"MeanValue.png",sep = "_"),
               plot = ggplot(data=.,aes(x=variable,y=value,color=usageMode,group=usageMode))+geom_line(), 
               width=16,height = 5,dpi = 100)
      }
    
    data.htl.hour.ac.toy.wide[,-"runtime"]%>%melt(.,id.var=c("modiDate","usageMode"))%>%{
      cat(table(.$usageMode),"\n")
      ggsave(file=paste(j,i,"overview.png",sep = "_"),
             plot = ggplot(data = .,aes(x=variable,y=value,color=usageMode,group=modiDate,alpha=0.05))+geom_line()+facet_wrap(.~usageMode,ncol=1), 
             width=6,height = 8,dpi = 200)
    }
  }
}




clusterTest<-NbClust(data = data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))], 
        diss = distDtw, distance = NULL,min.nc = 2, max.nc = 8, method = "kmeans", index = "all", alphaBeale = 0.1)

wssClusterEvaluate(data = data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))],
                   maxIter = 1000,
                   maxK = 15)
pamkClusterEvaluate(
  data = distDtw,#data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))],#8-22时+runtime
  criter = "ch",
  startK = 2,
  endK = 10,withPam = TRUE,isDistance = TRUE
)

#传统k-medidos聚类
