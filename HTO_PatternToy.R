#用于测试动态距离规划的聚类方法
#仅一栋
library(dtw)

data.htl.hour.ac.toy<-data.htl.hour.ac[deviceId=="SH_05_66-01"]

data.htl.hour.ac.toy.wide<-data.htl.hour.ac.toy[,c("labelDevHour","datetime","hour","onRatio")]
data.htl.hour.ac.toy.wide[,modiDatetime:=datetime-(14*3600)]
data.htl.hour.ac.toy.wide[,":="(modiDate=date(modiDatetime),modiHour=hour(modiDatetime))]
data.htl.hour.ac.toy.wide<-
  dcast(data.htl.hour.ac.toy.wide[,c("modiDate","modiHour","onRatio")],formula = modiDate~modiHour)
names(data.htl.hour.ac.toy.wide)<-c("modiDate",paste("h+14_",0:23,sep = ""))
data.htl.hour.ac.toy.wide$runtime<-apply(data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))],MARGIN = 1,sum,na.rm=TRUE)

ggplot(data.htl.hour.ac.toy.wide,#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
       aes(x=runtime))+geom_density()


data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))]<-
  data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))]%>%mutate_all(funs(ifelse(is.na(.),0, .)))%>%as.data.table()
nrow(data.htl.hour.ac.toy.wide[runtime<1])

data.htl.hour.ac.toy.wide<-data.htl.hour.ac.toy.wide[runtime>1]
data.htl.hour.ac.toy.wide[c(2,4)]%>%melt(.,id.var=c("modiDate","runtime"))%>%{
  ggplot(.,#[as.character(date(datetime)) %in% c("2019-01-19","2019-01-20")]
         aes(x=variable,y=value,group=as.factor(modiDate),color=as.factor(modiDate),lty=as.factor(modiDate)))+geom_line()
}

####聚类评估#

#距离评估demo
#dtw距离
dist(data.htl.hour.ac.toy.wide[c(2,4),c(paste("h+14_",0:23,sep = ""))], method="Euclidean")
nn<-dtw(as.matrix(data.htl.hour.ac.toy.wide[c(2,4),c(paste("h+14_",0:23,sep = ""))]),window.type = "none",window.size=0)
dtwPlotTwoWay(nn)

idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;
reference<-cos(idx)

a<-(data.htl.hour.ac.toy.wide[c(2),c(paste("h+14_",0:23,sep = ""))])%>%as.numeric()%>%ts
b<-(data.htl.hour.ac.toy.wide[c(4),c(paste("h+14_",0:23,sep = ""))])%>%as.numeric()%>%ts
alignment<-dtw(a,b,window.type = "itakura",window.size=0);
alignment$distance
dtwPlotTwoWay(alignment, xts=a,yts=b)


wssClusterEvaluate(data = data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))],
                   maxIter = 1000,
                   maxK = 15)
pamkClusterEvaluate(
  data = data.htl.hour.ac.toy.wide[,c(paste("h+14_",0:23,sep = ""))],#8-22时+runtime
  criter = "ch",
  startK = 2,
  endK = 10,withPam = TRUE
)

#传统k-medidos聚类
