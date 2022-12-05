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

#gen test and validation set
r.x.train = as.matrix(RDTraining[-1])
r.y.train = RDTraining$y
r.x.test = as.matrix(RDTest[-1])
r.y.test = RDTest$y

# Linear Model
LM.fit=lm(y~.,data=ExamDataReg,subset=train)
LM.pred=predict(LM.fit,ExamDataReg[-train,])
r.LM.MSE=mean((r.y.test - LM.pred)^2)
print(c('Linear model',r.LM.MSE))

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

# scale data
preproctrain = preProcess(RDTraining, method = c("center", "scale"))
preproctest = preProcess(RDTest, method = c("center", "scale"))
r.train.scaled = predict(preproctrain, RDTraining)
r.test.scaled = predict(preproctest, RDTest)

r.train.y.scaled = r.train.scaled$y
r.train.x.scaled = as.matrix(r.train.scaled[-1])

r.test.y.scaled = r.test.scaled$y
r.test.x.scaled = as.matrix(r.test.scaled[-1])

# Ridge
grid = 10 ^ seq(4, -2, length=100)
ridge = cv.glmnet(r.train.x.scaled, r.train.y.scaled, type.measure="mse", alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge, newx=r.test.x.scaled, s=ridge$lambda.min)
r.RIDGE.MSE = mean((r.test.y.scaled - ridge.pred)^2)

# LASSO
lasso = cv.glmnet(r.train.x.scaled, r.train.y.scaled, alpha=1, lambda=grid, thresh=1e-12)
lasso.pred = predict(lasso, newx=r.test.x.scaled, s=lasso$lambda.min)
r.LASSO.MSE = mean((r.test.y.scaled - lasso.pred)^2)

# PCR
pcr = pcr(y~., data=RDTraining, scale=T, validation="CV")
pcr.pred = predict(pcr, RDTest, ncomp=10)
r.PCR.MSE = mean((r.y.test - pcr.pred)^2)

# PLS
pls = plsr(y~., data=RDTraining, scale=T, validation="CV")
pls.pred = predict(pls, RDTest)
r.PLS.MSE = mean((r.y.test - pls.pred)^2)

# GAM
gam.fit.sq = gam(r.y.train ~ . , data = RDTrain.SQ)
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
boost.pred = predict(boost.best, newdata = RDTest, n.trees = boost.best$n.trees)

r.BOOST.MSE = mean((r.y.test - boost.pred)^2)

sq.train.errors = rep(NA, length.lambdas)
sq.test.errors = rep(NA, length.lambdas)
#again on squared data
for (i in 1:length.lambdas) {
  boost = gbm(r.y.train ~ ., data = RDTrain.SQ, distribution = "gaussian",
              n.trees = 1000, shrinkage = lambdas[i])
  sq.train.pred = predict(boost, newdata = RDTrain.SQ, n.trees = 1000)
  sq.test.pred = predict(boost, newdata = RDTest.SQ, n.trees = 1000)
  sq.train.errors[i] = mean((r.y.train - sq.train.pred)^2)
  sq.test.errors[i] = mean((r.y.test - sq.test.pred)^2)
}

#select best model
boost.sq.best = gbm(r.y.train ~ ., data = RDTrain.SQ, distribution = "gaussian",
                 n.trees = 1000, shrinkage = lambdas[which.min(sq.test.errors)])
boost.sq.pred = predict(boost.sq.best, newdata = RDTest.SQ, n.trees = boost.sq.best$n.trees)

r.BOOST.sq.MSE = mean((r.y.test - boost.sq.pred)^2)

# KNN regression
k.vec = seq(1, 25, by = 1)
length.k.vec = length(k.vec)
knnr.train.errors = rep(NA, length.k.vec)
knnr.test.errors = rep(NA, length.k.vec)

for (i in 1:length.k.vec) {
  knn.reg = knnreg(r.train.x.scaled, r.train.y.scaled, k = i)
  train.pred = predict(knn.reg, newdata = r.train.x.scaled)
  test.pred = predict(knn.reg, newdata = r.test.x.scaled)
  knnr.train.errors[i] = mean((r.train.y.scaled - train.pred)^2)
  knnr.test.errors[i] = mean((r.test.y.scaled - test.pred)^2)
}

knn.r.best = knnreg(r.train.x.scaled, r.train.y.scaled, k = k.vec[which.min(knnr.train.errors)])
knnreg.pred = predict(knn.r.best, newdata = r.test.x.scaled)

r.KNNREG.MSE = mean((r.test.y.scaled - knnreg.pred)^2)

# Random Forest (Bagging with 500 trees)

rf = randomForest(y~., data = RDTraining, mtry = ncol(r.x.train)/3, 
                  importance=T)
rf.pred = predict(rf, newdata = RDTest)
r.RF.MSE = mean((r.y.test - rf.pred)^2)

#again on squared data
rf.sq = randomForest(r.y.train ~., data = RDTrain.SQ, mtry = ncol(RDTrain.SQ)/3, 
                     importance=T)
rf.sq.pred = predict(rf.sq, newdata = RDTest.SQ)
r.RF.SQ.MSE = mean((r.y.test - rf.sq.pred)^2)

# Splines
first10 = c(1:9)
last40 = c(10:50)
splineform = as.formula(
  paste0("y~",paste0("s(x0",first10,")",collapse="+"),"+",
         paste0("s(x",last40, ")", collapse="+"),collapse=""))

r.splines = gam(formula = splineform, data = RDTraining, family=gaussian)
r.splines.pred = predict(r.splines, newdata = RDTest)
r.splines.MSE = mean((r.y.test - r.splines.pred)^2)

splineform.cubic = as.formula(
  paste0("y~",paste0("s(x0",first10,", df = 3)",collapse="+"),"+",
         paste0("s(x",last40, ", df = 3)", collapse="+"),collapse=""))

splines.cubic = gam(formula = splineform.cubic, data = RDTraining, family=gaussian)
splines.cubic.pred = predict(splines.cubic, newdata = RDTest)
r.splines.cubic.MSE = mean((r.y.test - splines.cubic.pred)^2)

###### clear Data before executing Classification Script ######
# detach(ExamDataReg)
# rm("ExamDataReg")
# rm("RDTest")
# rm("RDTraining")
#rm("x")
rm("p")
