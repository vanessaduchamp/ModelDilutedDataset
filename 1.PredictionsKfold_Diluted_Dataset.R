# the script will split the dataset into 10 subset used during the k-fold crossvalidation
# It will export in the folder of your choice different documents: 
# -> the GLMM model summary
# -> the GLM model summary
# -> the resulting ANOVA test of the 2 models
# -> the ROC curve for the 10folds
# -> the confusion matrix for the 10folds
# -> it will also export a .csv document with all the predictions
# Before to run the script, go through it to complete the information

# Use the key "tab" for automatic completion to the folder of your dataset. 
# The documents listed above will be saved in the same folder
setwd("~/")

# This script will import the dataset that you transformed with the "Formatting Data" scripts.
data<-read.table("data.csv",sep=",",dec=".",
                 na.strings = "",stringsAsFactors=FALSE,
                 h=TRUE)

# update the date 
date<-c("2018_10_03")


# charge the different libraries needed in this script
library(caret)
library(lme4)
library(ROCR)

data$Type<-as.factor(data$Type)

# for replicable experiments, choose a number and replace XXX
set.seed(123456)

# number of k-fold and 
k=10

# folds are randomly created based on the number k chosen above
fold<-createFolds(data$DO.H1,k)

# preparation of an image file for the ROC curves. All k ROC curves will be saved
# on the same graph
jpeg(paste(date,"_ROC.jpeg"),
     width = 10, height = 7, units = "in",
     quality = 100,
     type="cairo",res=150)

# empty vector creation the different values
auc.val<-rep(0,k)
obs.1<-NULL
pred.1<-NULL
marker.1<-NULL
dye.1<-NULL
LogH.1<-NULL
logEst.bp1<-NULL
type.1<-NULL
s.name1<-NULL

# in the loop, the script will run the model on 9 folds and apply
# the resulting model on the last fold 
# at each iteration, predictions will be made, a summary with all the model parameters is 
# saved, the ROC curve is registred as well as the AUC value and a confusion matrix.

for(i in 1:k){
  training.fold <- data[-fold[[i]],]
  test.fold <-data[fold[[i]],]
  
  ###### choose if your dataset contains homozygous allele or not
  ###### remove the # before the appropriate mod.1 below
  
  # GLMM model WIHTOUT HOMOZYGOUS IN DATASET
  mod.1<-glmer(DO.H1~LogH+LogEstbp+(1|Sample.N),data=training.fold,family=binomial)
  
  # GLMM model WIHT HOMOZYGOUS IN DATASET
  # mod.1<-glmer(DO.H1~LogH+LogEstbp+Type+(1|Sample.N),data=training.fold,family=binomial)

  # curve ROC
  p<-predict(mod.1,test.fold,type="response")  
  pr <- prediction(p, test.fold$DO.H1)
  prf <- performance(pr, measure = "tnr", x.measure = "fnr")
  ifelse(i>1,par(new=TRUE),"")
  plot(prf)
  
  # save observations and predictions in the appropriate vector
  obs.1<-append(obs.1,test.fold$DO.H1)
  pred.1<-append(pred.1,round(p))
  marker.1<-append(marker.1,as.character(test.fold$Marker))
  dye.1<-append(dye.1,as.character(test.fold$Dye))
  LogH.1<-append(LogH.1,test.fold$LogH)
  logEst.bp1<-append(logEst.bp1,test.fold$LogEstbp)
  type.1<-append(type.1,test.fold$Type)
  s.name1<-append(s.name1,test.fold$Sample.N)

  # AUC ROC
  auc <- performance(pr, measure = "auc")
  auc.val[i] <- auc@y.values[[1]]
  
  # save the model summary
  capture.output(summary(mod.1) , file=paste(date,"_Model_summary.txt"),append = TRUE) 
  
  # confusion Matrix
  p.bis<-predict(mod.1,test.fold,type="response")
  p.bis<-as.factor(round(p.bis))
  capture.output(confusionMatrix(p.bis,as.factor(test.fold$DO.H1)), 
                 file=paste(date,"_Confusion_matrix.txt"),append = TRUE)
  
 
}

# the minimum and maximum values observed on all the k ROC curves will be display directly
# on the graph
minauct <- paste(c("min(AUC)  = "),round(min(auc.val),2),sep="")
maxauct <- paste(c("max(AUC) = "),round(max(auc.val),2),sep="")
legend(0.7,0.6,c(minauct,maxauct,"\n"),border="n",cex=1,box.col = "white")
abline(a=0,b=1,col="red")

dev.off()

# the results are compiled together in a file
results<-data.frame(marker.1,dye.1,logEst.bp1,obs.1,pred.1,LogH.1,type.1,s.name1)

# the difference bewteen observation and prediction is calculated
results$diff<-abs(results$obs.1-results$pred.1)

# results are exported in a .csv file
write.table(results,
            file=paste(date,"results_predictions.csv"),
            col.names=TRUE,
            row.names=FALSE,
            sep=",",
            dec=".",
            append = FALSE)



