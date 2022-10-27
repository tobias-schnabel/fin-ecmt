# Tobias Schnabel | 833002303
rm(list = ls())
library(stargazer)
library(xtable)
library(ggpubr)

library(caret)
library(class)
library(ISLR2)
library(MASS)

data = Weekly

# add numerical reference column
data$reference = as.factor(data$Direction)
levels(data$reference) = c(0,1)

#a) at bottom of file

#b) logistic reg Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5
mod1 = glm(reference ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = data, family = "binomial"(link="logit"))

mod1.pred = predict.glm(mod1, type = "response")

#c) confusion matrix
cm1 = confusionMatrix(data = as.factor(mod1.pred>0.5), reference = as.factor(data$reference==1), positive = "TRUE")

#d) logistic reg 1990-2008 direction ~ Lag2
#subset
train = data[data$Year < 2009,]
test = data[data$Year > 2008,]

mod2 = glm(Direction ~ Lag2, data = train, family = "binomial"(link="logit"))

mod2.pred = predict.glm(mod2, newdata = test, type = "response")

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm2 = confusionMatrix(data = as.factor(mod2.pred>0.5), reference = as.factor(test$reference==1), positive = "TRUE")

#e) LDA 1990-2008 direction ~ Lag2, 
model3 = lda(Direction ~ Lag2, data = train)

mod3.pred = predict(model3, newdata = test)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm3 = confusionMatrix(data = as.factor(mod3.pred$class), reference = as.factor(test$Direction))

#f) QDA 1990-2008 direction ~ Lag2, 
model4 = qda(Direction ~ Lag2, data = train)

mod4.pred = predict(model4, newdata = test)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm4 = confusionMatrix(data = as.factor(mod4.pred$class), reference = as.factor(test$Direction))

#g) KNN K=1 1990-2008 direction ~ Lag2, 
ref = train$reference

knn.train <- as.matrix(train$Lag2)
knn.test <- as.matrix(test$Lag2)

mod5.pred = knn(knn.train, knn.test, cl = ref ,  k = 1)

#Confusion Matrix, Fraction of correct predictions for 2009-10
cm5 = confusionMatrix(data = as.factor(mod5.pred), reference = as.factor(test$reference))

#j) experiment
acc_list_knn = rep(0,250)

for (i in 1:length(acc_list_knn)) {
  fit = knn(knn.train, knn.test, cl = ref, k = i)
  acc_list_knn[i] = sum(test$reference == fit)/NROW(test)
}

knn.154 = knn(knn.train, knn.test, cl = ref, k = 154)
c.m154 = confusionMatrix(data = as.factor(knn.154), reference = as.factor(test$reference))

mod6 = glm(reference ~ Lag2 + Volume, data = train, family = "binomial"(link="logit"))

mod6.pred = predict.glm(mod6, newdata = test, type = "response")
cm6 = confusionMatrix(data = as.factor(mod6.pred>0.5), reference = as.factor(test$reference==1), positive = "TRUE")

mod7 = glm(reference ~ Lag1 + Volume, data = train, family = "binomial"(link="logit"))

mod7.pred = predict.glm(mod7, newdata = test, type = "response")
cm7 = confusionMatrix(data = as.factor(mod7.pred>0.5), reference = as.factor(test$reference==1), positive = "TRUE")

mod8 = glm(reference ~ Lag2 + (Lag2^2), data = train, family = "binomial"(link="logit"))

mod8.pred = predict.glm(mod8, newdata = test, type = "response")
cm8 = confusionMatrix(data = as.factor(mod8.pred>0.5), reference = as.factor(test$reference==1), positive = "TRUE")

cm.list = ls(pattern = "cm")
###########export results
#sumstats
stargazer(data, out.header = F, table.placement = "H", 
          out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW2/sumt")

#b)
stargazer(mod1, out.header = F, table.placement = "H", out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW2/reg")

#c)-g)
numdig = matrix(c(rep(0,3),rep(0,3), 0,3,3), byrow = T, nrow = 3)


#clean up confusion matrices for export
for (i in cm.list) {
  aux <- get(i)
  aux.results <- as.matrix(aux$table)
  aux.supp <- aux$overall[c(1,6)]
  
  e.aux = rbind(aux.results, aux.supp)
  rownames(e.aux) = c("Pred. FALSE", "Pred. TRUE", "Accuracy | p-value")
  assign(i, e.aux)
}

#export models
for (i in cm.list) {
  aux <- get(i)
  index <- as.character(match(i, cm.list))
  colnames(aux) = c("Ref. FALSE", "Ref. TRUE")
  
  print(xtable(aux,  caption = paste("Confusion Matrix for Model", index, sep = " "),
               digits = numdig,), 
               caption.placement = "top",
               label = index,  table.placement = "H",
        type = "latex", file = paste("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW2/cm", index, sep = ""))
}

#154NN export
aux = c.m154
aux.results <- as.matrix(c.m154$table)
aux.supp <- aux$overall[c(1,6)]

aux = rbind(aux.results, aux.supp)
rownames(aux) = c("Pred. FALSE", "Pred. TRUE", "Accuracy | p-value")
colnames(aux) = c("Ref. FALSE", "Ref. TRUE")

print(xtable(aux,  caption = "Confusion Matrix for 154NN",
             digits = numdig,), 
      caption.placement = "top",
      label = index,  table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW2/cm154")

aux.list = ls(pattern = "aux")
rm(aux.list)

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW2")

#a) Summaries

#plots
{
  p1 <- ~{
    plot(data$Direction, data$Today)
  }
  
  p2 <- ~{
    p2 <- plot(data$Direction, data$Lag1)
  }
  p3 <- ~{
    p3 = plot(data$Direction, data$Lag2)
  }
  p4 <- ~{
    p4 = plot(data$Direction, data$Lag3)
  }
  p5 <- ~{
    p5 = plot(data$Direction, data$Lag4)
  }
  p6 <- ~{
    p6 = plot(data$Direction, data$Lag5)
  }
  p7 <- ~{
    p7 = plot(data$Direction, data$Volume)
  }
  p8 <- ~{
    p8 = plot(data$Today, data$Year)
  }  
}

png("plot1.png", width = 800, height = 800, units = "px")
ggarrange(p1, p2, p3, p4, widths = c(1,1)) +  
  theme(plot.margin = margin(0,0,0,0, "cm")) 
dev.off()

png("plot2.png")
ggarrange(p5, p6, p7) +  
  theme(plot.margin = margin(0,0,0,0, "cm"))
dev.off()

png("knn.png")
plot(acc_list_knn, xlab = "K", ylab = "Accuracy")
dev.off()

plots = ls(pattern = "[p].*\\d")
rm(plots)

setwd("/Users/ts/Git/fin-ecmt")