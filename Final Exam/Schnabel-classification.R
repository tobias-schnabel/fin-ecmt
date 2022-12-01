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
c.y.train = CDTraining$y
c.x.test = as.matrix(CDTest[-1])
c.y.test = CDTest$y

# Logit
logit = glm(y ~ ., data = CDTraining, family = "binomial"(link="logit"))
logit.pred = predict.glm(logit, type = "response", newdata = CDTest)


# LDA
lda = lda(y ~ ., data = CDTraining)
lda.pred = predict(lda, newdata = CDTest)

# mod3.pred = predict(model3, newdata = test)


# #f) QDA 1990-2008 direction ~ Lag2,
# model4 = qda(Direction ~ Lag2, data = train)

# mod4.pred = predict(model4, newdata = test)

# #g) KNN K=1 1990-2008 direction ~ Lag2,
# ref = train$reference

# knn.train <- as.matrix(train$Lag2)
# knn.test <- as.matrix(test$Lag2)

# mod5.pred = knn(knn.train, knn.test, cl = ref ,  k = 1)


###################### NOTES ######################
#KNN
#Random Forest
#LDA
#QDA
#SVM
#K-means clustering

#rf = randomForest(y~., , mtry=, importance=T)
#for x: mtry = sqrt(p
