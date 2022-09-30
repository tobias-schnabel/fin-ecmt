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

#Ch 5 Ex 2

#g)
prob = function(n){
  return(1 - (1 - 1/n)^n)
} 
x = 1:1e+05


setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW3")
png("plot1.png", width = 800, height = 800, units = "px")
plot(x, prob(x))
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#h)

store = rep(NA , 10000)
for(i in 1:length(store)){
  store[i] <- sum(sample (1:100 , rep=TRUE) == 4) > 0
  }

mean(store)

#Ch 5 Ex 8
#a
set.seed(1)
X = rnorm(100)
Y = X - 2 *X^2 + rnorm(100)

#b
setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW3")
png("plot2.png", width = 800, height = 800, units = "px")
plot(X, Y)
dev.off()
setwd("/Users/ts/Git/fin-ecmt")

#c
set.seed(1849)
X = rnorm(100)
Y = X - 2 *X^2 + rnorm(100)

data = data.frame(X,Y)

model1 = glm(Y ~ X)
model2 = glm(Y ~ poly(X,2))
model3 = glm(Y ~ poly(X,3))
model4 = glm(Y ~ poly(X,4))

cv.glm(data, model1)$delta[1]
cv.glm(data, model2)$delta[1]
cv.glm(data, model3)$delta[1]
cv.glm(data, model4)$delta[1]

#d
set.seed(5)

model1 = glm(Y ~ X)
model2 = glm(Y ~ poly(X,2))
model3 = glm(Y ~ poly(X,3))
model4 = glm(Y ~ poly(X,4))

cv.glm(data, model1)$delta[1]
cv.glm(data, model2)$delta[1]
cv.glm(data, model3)$delta[1]
cv.glm(data, model4)$delta[1]

stargazer(model4, out.header = F, table.placement = "H", 
          out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW3/t1")