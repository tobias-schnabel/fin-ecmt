# Tobias Schnabel | 833002303
rm(list = ls())
library(stargazer)
library(xtable)
library(summarytools)
library(caret)
library(ISLR2)
library(MASS)
library(class)

data = Weekly
dfSummary(data)

# add numerical reference column
data$reference = as.factor(data$Direction)
levels(data$reference) = c(0,1)

#a) Summaries


#b) logistic reg Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5
mod1 = glm(reference ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = data, family = "binomial"(link="logit"))

mod1.pred = predict.glm(mod1, type = "response")

#c) confusion matrix
# add numerical reference column
data$reference = as.factor(data$Direction)
levels(data$reference) = c(0,1)

cm1 = confusionMatrix(data = as.factor(mod1.pred>0.5), reference = as.factor(data$reference==1))
cm1.results = as.matrix(cm1$table)
mean(as.factor(mod1.pred>0.5) == as.factor(data$reference==1))

#d) logistic reg 1990-2008 direction ~ Lag2
#subset
train = data[data$Year < 2009,]
test = data[data$Year > 2008,]

mod2 = glm(Direction ~ Lag2, data = train, family = "binomial"(link="logit"))

mod2.pred = predict.glm(mod2, newdata = test, type = "response")

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm2 = confusionMatrix(data = as.factor(mod2.pred>0.5), reference = as.factor(test$reference==1))
cm2.results = as.matrix(cm2$table)

#e) LDA 1990-2008 direction ~ Lag2, 
model3 = lda(Direction ~ Lag2, data = train)

mod3.pred = predict(model3, newdata = test)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm3 = confusionMatrix(data = as.factor(mod3.pred$class), reference = as.factor(test$Direction))
cm3.results = as.matrix(cm3$table)


#f) QDA 1990-2008 direction ~ Lag2, 
model4 = qda(Direction ~ Lag2, data = train)

mod4.pred = predict(model4, newdata = test)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm4 = confusionMatrix(data = as.factor(mod3.pred$class), reference = as.factor(test$Direction))
cm4.results = as.matrix(cm4$table)

#f) KNN K=1 1990-2008 direction ~ Lag2, 
ref = train$reference

knn.train <- as.matrix(train$Lag2)
knn.test <- as.matrix(test$Lag2)

mod5.pred = knn(knn.train, knn.test, cl = ref ,  k = 1)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm5 = confusionMatrix(data = as.factor(mod5.pred), reference = as.factor(test$reference))
cm5.results = as.matrix(cm5$table)2

#i) compare results
