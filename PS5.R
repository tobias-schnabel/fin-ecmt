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

#Ch 12 Ex 2 (a)
d=as.dist(matrix(c(0, 0.3, 0.4, 0.7,
                   0.3, 0, 0.5, 0.8,
                   0.4, 0.5, 0.0, 0.45,
                   0.7, 0.8, 0.45, 0.0), nrow = 4))

png("plot4.png", width = 800, height = 800, units = "px")
plot(hclust(d, method = "complete"))
dev.off()

#(b)
png("plot5.png", width = 800, height = 800, units = "px")
plot(hclust(d, method = "single"))
dev.off()

#(e)
png("plot6.png", width = 800, height = 800, units = "px")
plot(hclust(d, method = "complete"), labels = c(2,1,4,3))
dev.off()

#Ch 12 Ex 7 (a)
dsc <- scale(USArrests)
d1 <- dist(dsc)^2
d2 <- as.dist(1 - cor(t(dsc)))
summary(d2 / d1)

#Ch 12 Ex 10 (a)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

#(b)
pca.out = prcomp(x)
summary(pca.out)
pca.out$x[,1:2]
plot(pca.out$x[,1:2], col=1:3, xlab="Z1", ylab="Z2", pch=19)

#(c)
km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#(d)
km.out = kmeans(x, 2, nstart=20)
km.out$cluster

#(e)
km.out = kmeans(x, 4, nstart=20)
km.out$cluster

#(f)
km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#(g)
km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster

#reset wd
setwd("/Users/ts/Git/fin-ecmt")
