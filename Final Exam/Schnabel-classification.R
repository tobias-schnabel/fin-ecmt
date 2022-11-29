# Tobias Schnabel, UIN 833002303

# this.dir=getwd()
# setwd(this.dir)
# rm(list=ls())

set.seed(1)
CDTraining=read.table('Classification_Training.csv',sep=',',header=TRUE)
CDTest=read.table('Classification_Test.csv',sep=',',header=TRUE)

#Combine training dataset and test dataset
ExamDataClass=rbind(CDTraining,CDTest)

#attach(ExamDataClass)

#Index of training set and testing set
train=1:dim(CDTraining)[1]
test=(dim(CDTraining)[1]+1):dim(ExamDataClass)[1]

#Dimension of covariates
x=model.matrix(y~.,ExamDataClass)[,-1]
p=ncol(x)


