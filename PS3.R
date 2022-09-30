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

#Ch 5 Ex 9

#a
attach(Boston)
data = Boston

mu_hat = mean(medv)

#b
se_hat = sd(medv)/sqrt(nrow(data))

#c
set.seed(1)
boots_mean <- function(data, i) {
  return(mean(data[i]))
}
boot(medv, boots_mean, 1000)

#d
ci <- c(mu_hat - 2 * se_hat, mu_hat + 2 * se_hat)
ci
t.test(medv)

#e
medv.med = median(medv)
medv.med

#f
set.seed(1)
boot_median <- function(data, i) {
  return(median(data[i]))
}

boot(medv, boot_median, 1000)

#g
dec <- quantile(Boston$medv, 0.1)

#h
boot_decile <- function(data, i) {
  return(quantile(data[i], 0.1))
}

boot(medv, boot_decile, 1000)

#Ch 6 Ex 9
attach(College)

#a






