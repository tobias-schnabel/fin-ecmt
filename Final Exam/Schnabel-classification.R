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

# #b) logistic reg Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5
# mod1 = glm(reference ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = data, family = "binomial"(link="logit"))

# mod1.pred = predict.glm(mod1, type = "response")

# #c) confusion matrix
# cm1 = confusionMatrix(data = as.factor(mod1.pred>0.5), reference = as.factor(data$reference==1), positive = "TRUE")


# #e) LDA 1990-2008 direction ~ Lag2,
# model3 = lda(Direction ~ Lag2, data = train)

# mod3.pred = predict(model3, newdata = test)

# #Confusion Matrix, Fraction of correct predictions for 2009-10
# cm3 = confusionMatrix(data = as.factor(mod3.pred$class), reference = as.factor(test$Direction))

# #f) QDA 1990-2008 direction ~ Lag2,
# model4 = qda(Direction ~ Lag2, data = train)

# mod4.pred = predict(model4, newdata = test)

# #Confusion Matrix, Fraction of correct predictions for 2009-10
# cm4 = confusionMatrix(data = as.factor(mod4.pred$class), reference = as.factor(test$Direction))

# #g) KNN K=1 1990-2008 direction ~ Lag2,
# ref = train$reference

# knn.train <- as.matrix(train$Lag2)
# knn.test <- as.matrix(test$Lag2)

# mod5.pred = knn(knn.train, knn.test, cl = ref ,  k = 1)

# #Confusion Matrix, Fraction of correct predictions for 2009-10
# cm5 = confusionMatrix(data = as.factor(mod5.pred), reference = as.factor(test$reference))
