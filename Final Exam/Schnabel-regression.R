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
r.LM.MSE=mean((y[-train]-LM.pred)^2)
print(c('Linear model',LM.MSE))

#gen test and validation set
r.x.train = as.matrix(RDTraining[-1])
r.y.train = RDTraining$y
r.x.test = as.matrix(RDTest[-1])
r.y.test = RDTest$y

#gen squared test and validation sets
coln = colnames(r.x.train)
coln.sq = rep(NA, length(coln))
for (x in 1:length(coln)) {
  coln.sq[x] = paste(coln[x], "SQ", sep = '')
}

r.x.train.sq = RDTraining[-1]^2
colnames(r.x.train.sq) = coln.sq
r.x.test.sq = RDTest[-1]^2
colnames(r.x.test.sq) = coln.sq

r.x.train.squared = cbind(r.x.train, r.x.train.sq)
r.x.test.squared = cbind(r.x.test, r.x.test.sq)

RDTrain.SQ = cbind(r.y.train, r.x.train.sq)
RDTest.SQ = cbind(RDTest$y, r.x.test.sq)

# model1 = glm(Y ~ X)

# Ridge
grid = 10 ^ seq(4, -2, length=100)
ridge = cv.glmnet(r.x.train, r.y.train, type.measure="mse", alpha=0, lambda=grid, thresh=1e-12)
# RIDGE.MSE = ridge$cvm[ridge$lambda == ridge$lambda.min]

ridge.pred = predict(ridge, newx=r.x.test, s=ridge$lambda.min)
r.RIDGE.MSE = mean((r.y.test - ridge.pred)^2)

#LASSO
lasso = cv.glmnet(r.x.train, r.y.train, alpha=1, lambda=grid, thresh=1e-12)
lasso.pred = predict(lasso, newx=r.x.test, s=lasso$lambda.min)
r.LASSO.MSE = mean((r.y.test - lasso.pred)^2)

# PCR
pcr = pcr(y~., data=RDTraining, scale=T, validation="CV")
pcr.pred = predict(pcr, RDTest, ncomp=10)
r.PCR.MSE = mean((r.y.test - pcr.pred)^2)

# PLS
pls = plsr(y~., data=RDTraining, scale=T, validation="CV")
pls.pred = predict(pls, RDTest)
r.PLS.MSE = mean((r.y.test - pls.pred)^2)

#GAM
gam.fit.sq = gam(r.y.train ~ . + . , data = RDTrain.SQ)
gam.pred.sq = predict(gam.fit.sq, r.x.test.sq)
r.GAM.SQ.MSE = mean((r.y.test - gam.pred.sq)^2)

gam.fit = gam(y ~ ., data = RDTraining)
gam.pred = predict(gam.fit, RDTest)
r.GAM.MSE = mean((r.y.test - gam.pred)^2)


# forward stepwise selection
fss = regsubsets(y ~., data = RDTraining, method = "forward", nvmax = 50)

# plot shows 10 regressors optimal
#extract best 10
coefi = names(coef(fss, id = 10)[-1])
print(coefi)
#fit LM with these 10
fss.best = lm(y ~ x08 + x12 + x14 + x16 + x31 + x33 + x35 + x37 + x39 + x40, data = RDTraining)
fss.pred=predict(fss.best, newdata = RDTest)
r.FSS.MSE=mean((r.y.test - fss.pred)^2)

# backward stepwise selection
bss = regsubsets(y ~., data = RDTraining, method = "backward", nvmax = 50)

# plot shows 10 regressors optimal

#extract best 10
coefi.bw = names(coef(bss, id = 10)[-1])
print(coefi.bw)
# same regressors?
print(coefi == coefi.bw)
# -> no 
#fit LM with new 10
bss.best = lm(y ~ x08 + x02 + x13 + x14 + x16 + x19 + x31 + x35 + x37 + x40, data = RDTraining)
bss.pred=predict(fss.best, newdata = RDTest)
r.BSS.MSE=mean((r.y.test - fss.pred)^2)

# Boosting
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)

for (i in 1:length.lambdas) {
  boost = gbm(y ~ ., data = RDTraining, distribution = "gaussian",
              n.trees = 1000, shrinkage = lambdas[i])
  train.pred = predict(boost, newdata = RDTraining, n.trees = 1000)
  test.pred = predict(boost, newdata = RDTest, n.trees = 1000)
  train.errors[i] = mean((r.y.train - train.pred)^2)
  test.errors[i] = mean((r.y.test - test.pred)^2)
}

#select best model
boost.best = gbm(y ~ ., data = RDTraining, distribution = "gaussian",
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
boost.pred = predict(boost.best, newdata = RDTest, n.trees = 1000)

r.BOOST.MSE = mean((r.y.test - boost.pred)^2)

sq.train.errors = rep(NA, length.lambdas)
sq.test.errors = rep(NA, length.lambdas)
#again on squared data
for (i in 1:length.lambdas) {
  boost = gbm(y ~ ., data = RDTraining, distribution = "gaussian",
              n.trees = 1000, shrinkage = lambdas[i])
  train.pred = predict(boost, newdata = RDTraining, n.trees = 1000)
  test.pred = predict(boost, newdata = RDTest, n.trees = boost.best$n.trees)
  sq.train.errors[i] = mean((r.y.train - train.pred)^2)
  sq.test.errors[i] = mean((r.y.test - test.pred)^2)
}

#select best model
boost.sq.best = gbm(r.y.train ~ ., data = RDTrain.SQ, distribution = "gaussian",
                 n.trees = 1000, shrinkage = lambdas[which.min(sq.test.errors)])
boost.sq.pred = predict(boost.sq.best, newdata = RDTest.SQ, n.trees = boost.sq.best$n.trees)

r.BOOST.sq.MSE = mean((r.y.test - boost.sq.pred)^2)

#KNN regression
k.vec = seq(1, 25, by = 1)
length.k.vec = length(k.vec)
knnr.train.errors = rep(NA, length.k.vec)
knnr.test.errors = rep(NA, length.k.vec)

for (i in 1:length.k.vec) {
  knn.reg = knnreg(r.x.train, r.y.train, k = i)
  train.pred = predict(knn.reg, newdata = r.x.train)
  test.pred = predict(knn.reg, newdata = r.x.test)
  knnr.train.errors[i] = mean((r.y.train - train.pred)^2)
  knnr.test.errors[i] = mean((r.y.test - test.pred)^2)
}

knn.r.best = knnreg(r.x.train, r.y.train, k = k.vec[which.min(knnr.train.errors)])
knnreg.pred = predict(knn.r.best, newdata = r.x.test)

r.KNNREG.MSE = mean((r.y.test - knnreg.pred)^2)

#Random Forest

rf = randomForest(y~., data = RDTraining, mtry = ncol(r.x.train), importance=T)
rf.pred = predict(rf, newdata = RDTest)
r.RF.MSE = mean((r.y.test - rf.pred)^2)

#again on squared data
rf.sq = randomForest(r.y.train.sq ~., data = RDTrain.SQ, mtry = ncol(RDTrain.SQ), importance=T)
rf.sq.pred = predict(rf.sq, newdata = RDTest.SQ)
r.RF.SQ.MSE = mean((r.y.test - rf.sq.pred)^2)

# collect all MSEs
r.mse.mat = as.matrix(cbind(rbind(r.LM.MSE, r.FSS.MSE, r.BSS.MSE, r.GAM.SQ.MSE, 
                            r.LASSO.MSE, r.RIDGE.MSE, r.BOOST.MSE, r.KNNREG.MSE, 
                            r.RF.MSE, r.BOOST.sq.MSE),
                      rep("", 10)))
r.mse.mat[2,2] = "10"
r.mse.mat[3,2] = "10"
r.mse.mat[4,2] = "all predictors squared"
r.mse.mat[5,2] = paste("$\\lambda=$", toString(lasso$lambda.min))
r.mse.mat[6,2] = paste("$\\lambda=$", toString(ridge$lambda.min))
r.mse.mat[7,2] = paste("$\\shrinkage=$", toString(boost.best$shrinkage))
r.mse.mat[8,2] = paste("k=", toString(knn.r.best$k))
r.mse.mat[9,2] = paste("Number of Trees:", toString(rf$ntree))
r.mse.mat[10,2] = paste("$\\shrinkage=$", toString(boost.sq.best$shrinkage))
print(r.mse.mat)

###################### NOTES ######################
#splines
#nonlinearities (GAM?)
#interactions?

###### clear Data before executing Classification Script ######
# detach(ExamDataReg)
# rm("ExamDataReg")
# rm("RDTest")
# rm("RDTraining")
#rm("x")
rm("p")
