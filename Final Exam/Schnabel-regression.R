# Tobias Schnabel, UIN 833002303

# this.dir=getwd()
# setwd(this.dir)
# rm(list=ls())

set.seed(1)
RDTraining=read.table('Regression_Training.csv',sep=',',header=TRUE)
RDTest=read.table('Regression_Test.csv',sep=',',header=TRUE)

#Combine training dataset and test dataset
ExamDataReg=rbind(RDTraining,RDTest)

#attach(ExamDataReg)

#Index of training set and testing set
train=1:dim(RDTraining)[1]
test=(dim(RDTraining)[1]+1):dim(ExamDataReg)[1]

#Dimension of covariates
x=model.matrix(y~.,ExamDataReg)[,-1]
p=ncol(x)

#Linear Model
LM.fit=lm(y~.,data=ExamDataReg,subset=train)
LM.pred=predict(LM.fit,ExamDataReg[-train,])
LM.MSE=mean((y[-train]-LM.pred)^2)
print(c('Linear model',LM.MSE))



###### clear Data before executing Classification Script ######
# detach(ExamDataReg)
rm("ExamDataReg")
rm("RDTest")
rm("RDTraining")
rm("x")
rm("p")