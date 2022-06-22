####���ű����ھƵ���Ϊ-�Ȼ���-�ܺĽ���####

data.htl.hour.decoupling<-data.htl.hour.ac.env.wide[,c("labelDevDate","modiDate","month","isBizday","season","maxMode","seasonMode",
                                                       "runtime","occuTime","mSetTemp","windSpd","outTemp","ralHum",
                                                       "patternName","tempPatternName")]
data.htl.hour.decoupling<-merge(x=data.htl.hour.decoupling,y=data.htl.day.ac.energy[,c("labelDevModiDate","energyPatternName")],all.x = TRUE,
                                by.x = "labelDevDate",by.y = "labelDevModiDate")

#����һ��ID�кű���ʮ��
data.htl.hour.decoupling$id<-c(1:nrow(data.htl.hour.decoupling))
#�����õ������
names(data.htl.hour.decoupling)[14:16]<-c("usagePattern","tempPattern","energyPattern")

####����ǰԤ����####

# �����ܺĽ����ϵʽ
decouplingAttr<-c("tempPattern","mSetTemp","outTemp","ralHum","windSpd","runtime","isBizday")#patternName
decouplingFormula<-as.formula(paste("energyPattern ~ ",paste(decouplingAttr,collapse = "+")))

# ���۽���������
data.htl.hour.decoupling.log<-data.table(labelDevDate="",season="",round=1,type="train",
                                         method="method",real="energyClusterName",pred="pred")[-1]

data.htl.hour.decoupling[,":="(treePred=as.character(NA),forestPred=as.character(NA))]

data.htl.hour.decoupling<-data.htl.hour.decoupling%>%{
  for(i in unique(.$season) ){
    #��Ϊ����׼ȷ�ȼ�����㷨�����Ļ���
    stat.hznu.decoupling.algoAcc<-data.table(algoName="",setType="",finalState="",usagePattern="",count=as.numeric(NA),acc=as.numeric(NA))[-1]
    
    for(j in unique(.[season==i]$patternName)){
      
      for(k in c(0:9)){
        
      }
      #������Ԥ����[,lapply(.SD,mean,na.rm=TRUE),
      .SDcols=c(paste("h+14_",0:23,sep = ""))
      data.htl.hour.decoupling[,c("tempPattern","energyPattern","isBizday")]<-data.htl.hour.decoupling[,lapply(.SD,as.factor),.SDcols=c("tempPattern","energyPattern","isBizday")] 
        .
      }
     
      
      #CART�������㷨
      if(TRUE){
        algo<-"CART_Tree"
        
        #10�۽�����֤��
        #train�����޷��Խ���һ������ı�������ѵ��
        # fit<-train(form=tenFoldFormula,na.action = "na.omit",
        #            data=data.hznu.teaching.decoupling.selected,method = "rpart",#tuneGrid=expand.grid(cp=seq(from= localInitCP,to=0.2,by=0.005)),
        #            trControl=trainControl(method = "cv",number = 10,savePredictions ="final",search = "random"))
        # plot(as.party(fit$finalModel))
        # list.hznu.decoupling.cart[[i]][[j]][["10Fold"]]<-fit$finalModel
        # outputImg(as.party(fit$finalModel),hit=900,wid = 1600,fileName =paste(i,j,algo,"10Fold_TreeMap.png",sep = "_"))
        # cmResult<-confusionMatrix(data=fit$pred$pred,reference = fit$pred$obs)
        # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
        #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="10-fold"))
        # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"10Fold_Result.txt",sep = "_"),
        #                 algoName = algo,tree = fit$finalModel , fmla = tenFoldFormula, logTitle =  paste(i,j,algo,"10Fold_Result",sep = "_"),
        #                 other = list(paste("Total node: ",length(as.party(fit$finalModel))),fit$finalModel$variable.importance))
        # rm(fit,cmResult)
        
        #hold-out��֤��
        tree.both<-rpart(decouplingFormula,cp=0.002,minbucket=20,
                       data=data.htl.hour.decoupling[id%%10!=k])#rpart,����������������붼Ϊfactor����,��char������...
        tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])#tree.both$cptable������������ѱ�֤һ�����
        rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party"
        plot(rpartTrue2)
        # par(mfrow=c(1,1))
        prp(tree.both,type=5,extra = 8,varlen=0,faclen=0,digits = 3,gap =0,tweak =1.05)
        
        
        # list.hznu.decoupling.cart[[i]][[j]][["holdOut"]]<-tree.both
        
        
        #����������Ԥ��ֵ�Լ�������ĸ���
        data.hznu.teaching.decoupling.log<-rbind(data.hznu.teaching.decoupling.log,
                                                 data.table(labelRoomDay=data.hznu.teaching.decoupling.test$labelRoomDay,
                                                            finalState=i,
                                                            clusterName=j,
                                                            method="CART",
                                                            type="test",
                                                            real=data.hznu.teaching.decoupling.test$energyClusterName,
                                                            pred=predict(list.hznu.decoupling.cart[[i]][[j]][["holdOut"]],
                                                                         data.hznu.teaching.decoupling.test,type="class"),
                                                            predict(list.hznu.decoupling.cart[[i]][[j]][["holdOut"]],
                                                                    data.hznu.teaching.decoupling.test,type="prob")),fill=TRUE)
        
        
        #���Լ���֤
        # cmResult<-data.hznu.teaching.decoupling.test%>%
        #           predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = rpartTrue2)
        #   # predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
        #   #             predictableModel = rpartTrue2)
        # #������
        # outputImg(rpartTrue2,hit=900,wid = 1600,fileName =paste(i,j,algo,"TreeMap.png",sep = "_"))
        # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
        #                 algoName = algo,tree = tree.both , fmla = decouplingFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"),
        #                 other = list(paste("Total node: ",length(rpartTrue2)),tree.both$variable.importance) )
        # #�ڴ�������
        # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
        #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
        # #ѵ�������д���ڴ�
        # cmResultTraining<-data.hznu.teaching.decoupling.training%>%
        #                   predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = rpartTrue2)
        # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
        #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
        #                                                acc=cmResultTraining$overall["Accuracy"],setType="training"))
        # rm(tree.both,rpartTrue2,cmResult,cmResultTraining)#��ʱ�������
      }
      
      
      # #���ɭ��
      
      if(TRUE){
        
        algo<-"RandomForest"
        #10-fold��֤
        # fit<-train(form=tenFoldFormula,na.action = "na.omit",
        #            data=data.hznu.teaching.decoupling.selected,method = "rf",tuneGrid=data.frame(mtry=2:5),importance=TRUE,ntree=1000,
        #            trControl=trainControl(method = "cv",number = 10,savePredictions ="final",search = "random"))#ntree�ܴ�����֪���ܲ��ܵ�
        # cmResult<-confusionMatrix(data=fit$pred$pred,reference = fit$pred$obs)
        # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
        #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="10-fold"))
        # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"10Fold_Result.txt",sep = "_"),
        #                 algoName = algo, fmla = tenFoldFormula, logTitle =  paste(i,j,algo,"10Fold_Result",sep = "_"),
        #                 other = list("nTree = 1000",importance(fit$finalModel,type = 1),importance(fit$finalModel,type = 2)))
        # rm(fit,cmResult)
        
        
        #hold-out��֤
        fit.forest<-randomForest(decouplingFormula,data=data.hznu.teaching.decoupling.training,
                                 ntree=1000,cp=localInitCP,mty=2,
                                 na.action = na.omit,importance=TRUE)
        
        data.hznu.teaching.decoupling.log<-rbind(data.hznu.teaching.decoupling.log,
                                                 data.table(labelRoomDay=data.hznu.teaching.decoupling.test$labelRoomDay,
                                                            finalState=i,
                                                            clusterName=j,
                                                            method="randomForest",
                                                            type="test",
                                                            real=data.hznu.teaching.decoupling.test$energyClusterName,
                                                            pred=predict(fit.forest,
                                                                         data.hznu.teaching.decoupling.test,type="class"),
                                                            predict(fit.forest,
                                                                    data.hznu.teaching.decoupling.test,type="prob")),fill=TRUE)
        # #���Լ���֤
        # cmResult<-data.hznu.teaching.decoupling.test%>%predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = fit.forest)
        # #������
        # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
        #                 algoName = algo, fmla = decouplingFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"),
        #                 other = list("nTree = 1000",importance(fit.forest,type = 1),importance(fit.forest,type = 2)))
        # outputImg(plottable = fit.forest,hit=480,wid=640,fileName = paste(i,j,algo,"Err.png",sep = "_"))
        # #�ڴ�������
        # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
        #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
        # #ѵ�������д���ڴ�
        # cmResultTraining<-data.hznu.teaching.decoupling.training%>%
        #                   predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = fit.forest)
        # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
        #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
        #                                                acc=cmResultTraining$overall["Accuracy"],setType="training"))
        rm(fit.forest,cmResult,cmResultTraining)
      } 
      
      
      
      # #�������ݱ�
      # outputImg(FUN = function(x){
      #   plot(x$error,type="o",pch=17,ann=FALSE)
      #   title(xlab = "Iteration times",ylab = "Error")
      # },plottable = errorevol(fit.boost,data.hznu.teaching.decoupling.training),
      # hit=480,wid = 640,fileName = paste(i,j,algo,"Err.png",sep = "_"))
      # 
      # #����ô�������ͳһ���ܲ���һ�µ㣿��
      # cmResult<-predict(fit.boost,data.hznu.teaching.decoupling.test)
      # outputValidRslt(cm=NA,logTitle =paste(i,j,algo,"Result",sep = "_"),algoName = algo,fileName=paste(i,j,algo,"Result.txt",sep = "_"),
      #                 FUN = function(fileName=paste(i,j,algo,"Result.txt",sep = "_")){
      #                   capture.output(decouplingFormula,file=fileName,append = TRUE)
      #                   capture.output(cmResult$error,file=fileName,append = TRUE)
      #                   capture.output(cmResult$confusion,file=fileName,append = TRUE)
      #                   capture.output(c("nIter = 200"),file=fileName,append = TRUE)
      #                   capture.output(fit.boost$importance,file=fileName,append = TRUE)
      #                 })
      # #�ڴ�������
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$confusion),acc=1-cmResult$error,setType="test"))
      # #ѵ�������д���ڴ�
      # cmResultTraining<-predict(fit.boost,data.hznu.teaching.decoupling.training)
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$confusion),
      #                                                acc=1-cmResultTraining$error,setType="training"))
      # 
      # rm(fit.boost,cmResult,cmResultTraining)
    }
    
    
  }
  ####��ȷ�Ȼ��ܽ�����####
  # stat.hznu.decoupling.algoAcc$correctCount<-stat.hznu.decoupling.algoAcc$acc*stat.hznu.decoupling.algoAcc$count
  # stat.hznu.decoupling.algoAcc$wrongCount<-stat.hznu.decoupling.algoAcc$count*(1-stat.hznu.decoupling.algoAcc$acc)
  # write.xlsx(stat.hznu.decoupling.algoAcc,file = paste("HZNU",i,"round_final.xlsx",sep = "_"))
}



}

data.hznu.area.predict.use<-data.hznu.area.predict.use%>%{
  for(i in unique(data.hznu.area.predict.use$modiSeason)){
    seasonalAttr<-c(predictUsageAttr[["constant"]],predictUsageAttr[[i]],"simpleKnnFullOnRatio","h1_errBase")#
    for(j in 0:9){
      .[modiSeason==i&complete.cases(.[,..seasonalAttr])]<-.[modiSeason==i&complete.cases(.[,..seasonalAttr])]%>%{
        fit.svm<-ksvm(x=as.formula( paste("fullOnRatio ~ ",paste(seasonalAttr,collapse = "+") ) ),
                      data=.[id%%10!=j],kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#Ϊɶ��ô��
        
        .[id%%10==j]$svmInitPred<-predict(fit.svm,.[id%%10==j])
        
        #������ʮ�۵Ľ�����д��棬�˴�ΪԤ�⼯
        data.hznu.area.predict.log<-.[id%%10==j]%>%
          data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                     target="fullOnRatio",method="svmInitPred",setType="test",
                     round=j,predValue=.$svmInitPred,realValue=.$fullOnRatio)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
        
        #������ʮ�۵Ľ�����д��棬�˴�Ϊѵ����
        data.hznu.area.predict.log<-.[id%%10!=j] %>%
          data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                     target="fullOnRatio",method="svmInitPred",setType="train",
                     round=j,predValue=as.numeric(predict(fit.svm,.)),realValue=.$fullOnRatio) %>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
        .
      }
    }
  }
  .
}%>%{
  .[svmInitPred== -999]$svmInitPred<-NA
  .
}