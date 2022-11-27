#Tobias Schnabel | 833002303
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
library(e1071)


#Ch 9 Ex 7(a)
bin <- ifelse(Auto$mpg > median(Auto$mpg),1,0)
Auto$mpglevel<- as.factor(bin)

#(b)
set.seed(1849)
tune.out<-tune(svm, mpglevel~., data=Auto, kernel="linear",ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

#(c)
tune.outd<-tune(svm, mpglevel~.,data = Auto, kernel ="polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2,3,4)))
summary(tune.outd)

tune.outg<-tune(svm, mpglevel~., data = Auto, kernel ="radial",ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.outg)

#(d)
svm.linear <- svm(mpglevel~., data = Auto, kernel = "linear", cost=1)
svm.poly <- svm(mpglevel~., data = Auto, kernel = "polynomial",cost = 100, degree = 2)
svm.radial <- svm(mpglevel~., data = Auto, kernel = "radial", cost= 100, gamma = 0.01)
plotpairs = function(fit){
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel","name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep="")))
  }
}

#set wd for plots
setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW5")

png("plot1.png", width = 800, height = 800, units = "px")
plotpairs(svm.linear)
dev.off()

png("plot2.png", width = 800, height = 800, units = "px")
plotpairs(svm.poly)
dev.off()

png("plot3.png", width = 800, height = 800, units = "px")
plotpairs(svm.radial)
dev.off()



#reset wd
setwd("/Users/ts/Git/fin-ecmt")
