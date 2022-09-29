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

#d) logistic reg 1990-2008 direction ~ Lag2
#subset
train = data[data$Year < 2009,]
test = data[data$Year > 2008,]

mod2 = glm(Direction ~ Lag2, data = train, family = "binomial"(link="logit"))

mod2.pred = predict.glm(mod2, newdata = test, type = "response")

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm2 = confusionMatrix(data = as.factor(mod2.pred>0.5), reference = as.factor(test$reference==1))

#e) LDA 1990-2008 direction ~ Lag2, 
model3 = lda(Direction ~ Lag2, data = train)

mod3.pred = predict(model3, newdata = test)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm3 = confusionMatrix(data = as.factor(mod3.pred$class), reference = as.factor(test$Direction))

#f) QDA 1990-2008 direction ~ Lag2, 
model4 = qda(Direction ~ Lag2, data = train)

mod4.pred = predict(model4, newdata = test)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm4 = confusionMatrix(data = as.factor(mod3.pred$class), reference = as.factor(test$Direction))

#f) KNN K=1 1990-2008 direction ~ Lag2, 
ref = train$reference

knn.train <- as.matrix(train$Lag2)
knn.test <- as.matrix(test$Lag2)

mod5.pred = knn(knn.train, knn.test, cl = ref ,  k = 1)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm5 = confusionMatrix(data = as.factor(mod5.pred), reference = as.factor(test$reference))

#i) compare results


#j) experiment

###########export results
numdig = matrix(c(rep(0,3),rep(0,3), 0,3,3), byrow = T, nrow = 3)

#e.cm.list = ls(pattern = "e.cm")
cm.list = ls(pattern = "cm")

#clean up confusion matrices for export
for (i in cm.list) {
  aux <- get(i)
  aux.results <- as.matrix(aux$table)
  aux.supp <- aux$overall[c(1,6)]
  
  e.aux = rbind(aux.results, aux.supp)
  rownames(e.aux) = c("FALSE", "TRUE", "Accuracy | p-value")
  assign(i, e.aux)
}

#export models
for (i in cm.list) {
  aux <- get(i)
  index <- as.character(match(i, cm.list))
  print(xtable(aux,  caption = paste("Confusion Matrix for Model", index, sep = " "), 
               digits = numdig, label = index, caption.placement = 'top', table.placement = "H",
        type = "latex"), file = paste("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW2/cm", index, sep = ""))
}

