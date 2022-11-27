# Tobias Schnabel | 833002303
rm(list = ls())
library(stargazer)
library(xtable)
library(ggpubr)
library(boot)
library(caret)
library(class)
library(ISLR2)
library(MASS)
library(glmnet)
library(pls)
library(lmvar)
library(leaps)
library(gam)
library(gbm)
library(randomForest)
library(httpgd)

#Ch 7 Ex 3
x = seq(-2, 2, 0.001)
y = 1 + x + -2 * (x-1)^2 * I(x>1)

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot1.png", width = 800, height = 800, units = "px")
plot(x, y)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#Ch 7 Ex 10
#a
set.seed(1849)
attach(College)
train = sample(length(Outstate), length(Outstate)/2)
test = -train
College.train = College[train, ]
College.test = College[test, ]
reg.fit = regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
reg.summary = summary(reg.fit)
par(mfrow = c(1, 3))

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot3.png", width = 800, height = 800, units = "px")
par(mfrow=c(1,3))
plot(reg.summary$cp, xlab = "# of Variables", ylab = "Mallow's Cp", type = "l")
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)

plot(reg.summary$bic, xlab = "# of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)

plot(reg.summary$adjr2, xlab = "# of Variables", ylab = "Adjusted R2",
type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)

#b
gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) +
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College.train)

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot4.png", width = 800, height = 800, units = "px")
par(mfrow=c(3,2))
plot(gam.fit, se = T, col = "blue")
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#c
#GAM
gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err

gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss
#OLS
ls.fit = lm(Outstate ~ Private + Room.Board + PhD + perc.alumni + Expend + Grad.Rate, data = College.train)
ls.pred = predict(ls.fit, College.test)
ls.err = mean((College.test$Outstate - ls.pred)^2)
ls.err

ls.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
ls.rss = 1 - ls.err/ls.tss
ls.rss
#d
summary(gam.fit)


#Ch 8 Ex 3
p = seq(0, 1, 0.01)
gini = p * (1 - p) * 2
entropy = -(p * log(p) + (1 - p) * log(1 - p))
class.err = 1 - pmax(p, 1 - p)

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot2.png", width = 800, height = 800, units = "px")
matplot(p, cbind(gini, entropy, class.err), col = c("black", "red", "blue"),
     ylab = "Gini Coeff., Entropy, Cl. Error", pch = 1)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#Ch 8 Ex 10

#a
sum(is.na(Hitters$Salary))
Hitters = Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))
Hitters$Salary = log(Hitters$Salary) #log transform

#b
train = 1:200
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]

#c
set.seed(103)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)

for (i in 1:length.lambdas) {
  boost.hitters = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian",
                      n.trees = 1000, shrinkage = lambdas[i])
  train.pred = predict(boost.hitters, Hitters.train, n.trees = 1000)
  test.pred = predict(boost.hitters, Hitters.test, n.trees = 1000)
  train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
  test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
}

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot5.png", width = 800, height = 800, units = "px")
plot(lambdas, train.errors, type = "b", xlab = "Shrinkage Value", ylab = "Training Set MSE",
     col = "red", pch = 2)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#d
setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot6.png", width = 800, height = 800, units = "px")
plot(lambdas, test.errors, type = "b", xlab = "Shrinkage Value", ylab = "Validation Set MSE",
     col = "orange", pch = 1)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

min(test.errors)
lambdas[which.min(test.errors)]

#e
lm.fit = lm(Salary ~ ., data = Hitters.train)
lm.pred = predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)

set.seed(1849)
x = model.matrix(Salary ~ ., data = Hitters.train)
y = Hitters.train$Salary
x.test = model.matrix(Salary ~ ., data = Hitters.test)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Hitters.test$Salary - lasso.pred)^2)

#f
boost.best = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian",
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot7.png", width = 800, height = 800, units = "px")
summary(boost.best)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#g
set.seed(1849)
rf.hitters = randomForest(Salary ~ ., data = Hitters.train, ntree = 500, mtry = 19)
rf.pred = predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)
