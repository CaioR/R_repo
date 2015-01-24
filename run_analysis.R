run_analysis<-function()
{
library("dplyr")

##Read Test dataset.

test_x<-read.table(".\\getclean\\test\\x_test.txt")
test_y<-read.table(".\\getclean\\test\\y_test.txt")
test_subj<-read.table(".\\getclean\\test\\subject_test.txt")

##Read Train dataset.

train_x<-read.table(".\\getclean\\train\\x_train.txt")
train_y<-read.table(".\\getclean\\train\\y_train.txt")
train_subj<-read.table(".\\getclean\\train\\subject_train.txt")

## Put test and train labels together.

datasetyy<-rbind(test_y,train_y)
names(datasetyy)<-"id"

## Put test and train datasets together.
datasetxx<-rbind(test_x,train_x)

##Read list of variables.
features<-read.table(".\\getclean\\features.txt")

##Subset datasetxx and create other dataset only with standard and mean columns
dataset<-datasetxx[,grep("std|mean",features$V2)]

## Put datasetyy and dataset together.
dataset<-cbind(datasetyy,dataset)

##Read activity dataset
activity<-read.table(".\\getclean\\activity_labels.txt")
names(activity)<-c("id","desc")

##Merge activity and dataset datasets on final dataset.
final<-merge(activity,dataset,by.x="id",by.y="id")

##Clean features dataset and use to names messurements on final dataset.
feat<-subset(features,grepl("mean|std",features$V2),select= V2)
feat<-as.character(feat[,1])
feat<- gsub("()","",feat,fixed=TRUE)
feat<- gsub("-","",feat,fixed=TRUE)
names(final)[3:ncol(final)]<-feat


## Put test_subj and train_subj dataset together 
subject<-rbind(test_subj,train_subj)
names(subject)<-"Sbj"


## Put subject and final datasets together
final<-cbind(subject,final)

##Group final dataset on Sbj and desc columns
final<-group_by(final,Sbj,desc)

## Apply mean funcion on every columns of final dataset
summary<-summarise_each_(final,funs(mean),names(final)[4:82])
print(summary)
}