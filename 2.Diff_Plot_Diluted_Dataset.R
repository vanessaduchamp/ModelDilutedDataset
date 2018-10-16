# this script follow 1.PredictionsKfold_Diluted_dataset.R and use the dataset exported on
# the last part of the script. 
# we did the substraction bewteen the observation of drop-out
# and its associated prediction.
# if the observation and the prediction are the same, there will be 0 in the colum "diff"
# if the observation and the prediction are different, there will be 1 in the colum "diff"

# Use the key "tab" for automatic completion to the folder of your dataset.
# the name is (date) 20XX_XX_XXresults_predictions.csv
# The different graphs produced in this script will be saved in the same folder
setwd("~/")
setwd("~/Dropbox/These/GitHub/ModelDiluteddataset/test_script1/")

#the data will be imported under the following variable "data.diff"
data.diff<-read.table("2018_10_03 results_predictions.csv",header = TRUE,sep=",",dec=".")

# update the date
date<-c("2018_08_20")


# we plot the "diff" column versus the logH, in order to see where the model failed
# the plot is exported in jpeg
# we rounded the LogH values and calculated the percentage of "diff"

jpeg(paste(date,"_DiffvslogH_DilutedDataset.jpeg"),
     width = 10, height = 5, units = "in",
     quality = 100,
     type="cairo",res=150)

data.diff$LogH.round<-round(data.diff$LogH.1,2)
test<-aggregate(diff~LogH.round+type.1,data.diff,sum)
test.length<-aggregate(diff~LogH.round+type.1,data.diff,length)
test<-merge(test,test.length,by=c("LogH.round","type.1"))
names(test)<-c("LogH.round","Type","Diff","Length")
test$prc<-round((test$Diff/test$Length)*100,2)

# plot the percentage of "diff" versus the LogH for each type of allele
# LOESS curve added to show the trend of the data

xyplot(prc~LogH.round,test,
       groups=Type,
       xlab=list(label="Log(H)", cex=1.5),
       ylab=list(label="Percentage of failed observations",cex=1.5),
       type=c("p","smooth"),
       span=0.5,
       scales=list(x=list(cex=1.5,tick.number=10),
                   y=list(cex=1.5,tick.number=10)),
       key=list(corner=c(1,1),
                points=list(col=c("black","darkgrey"),pch=1,cex=1.5),
                text=list(c("Homozygote","Heterozygote"),cex=1.5)),
       col=c("black","darkgrey"),
       col.line=c("black","darkgrey"),
       lwd=5,
       cex=1.5)

dev.off()

# plot the percentage of "Diff" versus the LogEstbp to see where the model failed
# the plot is exported in jpeg
# we rounded the LogEstbp values and calculated the percentage of "diff"

jpeg(paste(date,"_DiffvsLogEstbp_DilutedDataset.jpeg"),
     width = 10, height = 5, units = "in",
     quality = 100,
     type="cairo",res=150)

data.diff$LogEstbp.round<-round(data.diff$logEst.bp1,2)
test<-aggregate(diff~LogEstbp.round+type.1,data.diff,sum)
test.length<-aggregate(diff~LogEstbp.round+type.1,data.diff,length)
test<-merge(test,test.length,by=c("LogEstbp.round","type.1"))
names(test)<-c("LogEstbp","Type","Diff","Length")
test$prc<-round((test$Diff/test$Length)*100,2)

xyplot(prc~LogEstbp,test,
       groups=Type,
       xlab=list(label="Size (bp)", cex=1.5),
       ylab=list(label="Percentage of failed observations",cex=1.5),
       type=c("p","smooth"),
       span=0.5,
       scales=list(x=list(cex=1.5,tick.number=10),
                   y=list(cex=1.5,tick.number=10)),
       key=list(corner=c(1,1),
                points=list(col=c("black","darkgrey"),pch=1,cex=1.5),
                text=list(c("Homozygote","Heterozygote"),cex=1.5)),
       col=c("black","darkgrey"),
       col.line=c("black","darkgrey"),
       lwd=5,
       cex=1.5)
dev.off()




