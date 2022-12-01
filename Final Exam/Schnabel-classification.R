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

#gen test and validation set
c.x.train = as.matrix(CDTraining[-1])
c.y.train = as.factor(CDTraining$y)
c.x.test = as.matrix(CDTest[-1])
c.y.test = CDTest$y

# Logit
logit = glm(y ~ ., data = CDTraining, family = "binomial"(link="logit"))
logit.pred = predict.glm(logit, type = "response", newdata = CDTest)


# LDA
lda = lda(y ~ ., data = CDTraining)
lda.pred = predict(lda, newdata = CDTest)
lda.err = mean(c.y.test != lda.pred$class)

# QDA
qda = qda(y ~ ., data = CDTraining)
qda.pred = predict(qda, newdata = CDTest)
qda.err = mean(c.y.test != qda.pred$class)

# KNN
err_list_knn = rep(0,50)
for (i in 1:length(err_list_knn)) {
  fit = knn(CDTraining, CDTest, cl = c.y.test, k = i)
  err_list_knn[i] = mean(c.y.test != fit)
}

knn.best = knn(CDTraining, CDTest, cl = c.y.test, k = which(err_list_knn == min(err_list_knn)))
knn.err = mean(c.y.test != fit)

# Random Forest
c.rf = randomForest(as.factor(y) ~., data = CDTraining, mtry = sqrt(ncol(CDTraining)), 
                    proximity = T, importance=T, ntree = 1000)

c.rf.pred = predict(c.rf, newdata = CDTest)
c.rf.err = mean(as.factor(c.y.test) != c.rf.pred)

# SVM
gammagrid = 10 ^ seq(-4, 2, by=1)
# linear kernel
svm.lin = svm(y~., data=CDTraining, kernel="linear")
svm.lin.pred = predict(svm.lin, newdata = CDTest, type = "response")
svm.lin.pred.fit = as.factor(svm.lin.pred>0.5)
svm.lin.err = mean((as.factor(c.y.test) == 1) != svm.lin.pred.fit)
tune.lin = tune(svm, y~., data=CDTraining, validation.x = CDTest,
                kernel="linear", ranges = list(cost = c(0.01, 0.1, 1, 10),
                                               gamma = gammagrid))
svm.lin.err = tune.lin$best.performance


# polynomial kernel
svm.poly = svm(y~., data=CDTraining, kernel="polynomial")
svm.poly.pred = predict(svm.poly, newdata = CDTest, type = "response")
svm.poly.pred.fit = as.factor(svm.poly.pred>0.5)
svm.poly.err = mean((as.factor(c.y.test) == 1) != svm.poly.pred.fit)
tune.poly = tune(svm, y~., data=CDTraining, validation.x = CDTest,
                kernel="polynomial", ranges = list(cost = c(0.01, 0.1, 1, 10),
                                               gamma = gammagrid))
svm.poly.err = tune.poly$best.performance

# radial kernel
svm.rad = svm(y~., data=CDTraining, kernel="radial")
svm.rad.pred = predict(svm.rad, newdata = CDTest, type = "response")
svm.rad.pred.fit = as.factor(svm.rad.pred>0.5)
svm.rad.err = mean((as.factor(c.y.test) == 1) != svm.rad.pred.fit)
tune.rad = tune(svm, y~., data=CDTraining, validation.x = CDTest,
                kernel="radial", ranges = list(cost = c(0.01, 0.1, 1, 10),
                                               gamma = gammagrid))
svm.rad.err = tune.rad$best.performance

# K-means Clustering



#gather results
#cm.list = ls(pattern = "cm")

