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

# model1 = glm(Y ~ X)

# #c Ridge
# train.mat = model.matrix(Apps~., data=train)
# test.mat = model.matrix(Apps~., data=test)
# grid = 10 ^ seq(4, -2, length=100)
# mod6.ridge = cv.glmnet(train.mat, train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
# lambda.best = mod6.ridge$lambda.min
# lambda.best

# mod6.pred = predict(mod6.ridge, newx=test.mat, s=lambda.best)
# mean((test[, "Apps"] - mod6.pred)^2)


# #d LASSO
# mod7.lasso = cv.glmnet(train.mat, train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
# lambda.best.lasso = mod7.lasso$lambda.min
# lambda.best.lasso

# mod7.pred = predict(mod7.lasso, newx=test.mat, s=lambda.best.lasso)
# mean((test[, "Apps"] - mod7.pred)^2)
# predict(mod7.lasso, s=lambda.best.lasso, type="coefficients")

# #e PCR
# mod8.pcr = pcr(Apps~., data=train, scale=T, validation="CV")

# setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW3")
# png("plot3.png", width = 800, height = 800, units = "px")
# validationplot(mod8.pcr, val.type="MSEP")
# dev.off()
# setwd("/Users/ts/Git/fin-ecmt")

# mod8.pred = predict(mod8.pcr, test, ncomp=10)
# mean((test[, "Apps"] - mod8.pred)^2)

# #f PLS
# mod9.pls = plsr(Apps~., data=train, scale=T, validation="CV")

# #g comp

# test.avg = mean(test[, "Apps"])
# lm.test.r2 = 1 - mean((test[, "Apps"] - mod5.pred)^2) /mean((test[, "Apps"] - test.avg)^2)
# ridge.test.r2 = 1 - mean((test[, "Apps"] - mod6.pred)^2) /mean((test[, "Apps"] - test.avg)^2)
# lasso.test.r2 = 1 - mean((test[, "Apps"] - mod7.pred)^2) /mean((test[, "Apps"] - test.avg)^2)
# pcr.test.r2 = 1 - mean((test[, "Apps"] - mod8.pred)^2) /mean((test[, "Apps"] - test.avg)^2)
# pls.test.r2 = 1 - mean((test[, "Apps"] - mod9.pred)^2) /mean((test[, "Apps"] - test.avg)^2)


# reg.fit = regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
# reg.summary = summary(reg.fit)
# par(mfrow = c(1, 3))

# gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) +
#                 s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College.train)


# #GAM
# gam.pred = predict(gam.fit, College.test)
# gam.err = mean((College.test$Outstate - gam.pred)^2)
# gam.err

# gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
# test.rss = 1 - gam.err/gam.tss
# test.rss
# #OLS
# ls.fit = lm(Outstate ~ Private + Room.Board + PhD + perc.alumni + Expend + Grad.Rate, data = College.train)
# ls.pred = predict(ls.fit, College.test)
# ls.err = mean((College.test$Outstate - ls.pred)^2)
# ls.err

# #GAM
# gam.pred = predict(gam.fit, College.test)
# gam.err = mean((College.test$Outstate - gam.pred)^2)
# gam.err

# gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
# test.rss = 1 - gam.err/gam.tss
# test.rss
# #OLS
# ls.fit = lm(Outstate ~ Private + Room.Board + PhD + perc.alumni + Expend + Grad.Rate, data = College.train)
# ls.pred = predict(ls.fit, College.test)
# ls.err = mean((College.test$Outstate - ls.pred)^2)
# ls.err

# set.seed(1849)
# x = model.matrix(Salary ~ ., data = Hitters.train)
# y = Hitters.train$Salary
# x.test = model.matrix(Salary ~ ., data = Hitters.test)
# lasso.fit = glmnet(x, y, alpha = 1)
# lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
# mean((Hitters.test$Salary - lasso.pred)^2)

# #f
# boost.best = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian",
#                  n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])

###### clear Data before executing Classification Script ######
# detach(ExamDataReg)
rm("ExamDataReg")
rm("RDTest")
rm("RDTraining")
rm("x")
rm("p")
