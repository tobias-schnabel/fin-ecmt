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

#Ch 7 Ex 3
x = -2:2
y = 1 + x + -2 * (x-1)^2 * I(x>1)

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot1.png", width = 800, height = 800, units = "px")
plot(x, y)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#Ch 7 Ex 10
#a
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
gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err

gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss

#d
summary(gam.fit)


#Ch 8 Ex 3
p = seq(0, 1, 0.01)
gini = p * (1 - p) * 2
entropy = -(p * log(p) + (1 - p) * log(1 - p))
class.err = 1 - pmax(p, 1 - p)

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW4")
png("plot2.png", width = 800, height = 800, units = "px")
matplot(p, cbind(gini, entropy, class.err), col = c("red", "green", "blue"))
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#Ch 8 Ex 10

