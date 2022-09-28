# Tobias Schnabel | 833002303
library(stargazer)
library(xtable)
#a) 
x = rnorm(100, 0, 1)
eps = rnorm(100, 0, 0.25)
y = -1 + 0.5*x + eps

length(y) #100
# beta_0 = -1
# beta_1 = 0.5

setwd("/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Figures/HW1")

png("plot1.png")
plot(x,y)
dev.off()

model1 = lm(y ~ x)
summary(model1)

stargazer(model1, out.header = F, table.placement = "H", out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW1/t1")

png("plot2.png")
plot(x,y, main = "d)")
abline(lm(y ~ x), col = "blue", lty = 2)
abline(a=-1+eps, b=0.5, col = "red")
legend("bottomright",c("Fitted Line","Population Line"),
        col=c("blue","red"), lty=c(2,1))
dev.off()

#g
model2 = lm(y ~ x + I(x^2))
summary(model2)

stargazer(model2, out.header = F, table.placement = "H", out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW1/t2")

#h
eps2 = rnorm(100, 0, 0.1)
y2 = -1 + 0.5*x + eps2

length(y2) #100
# beta_0 = -1
# beta_1 = 0.5

png("plot3.png")
plot(x,y2)
dev.off()

model3 = lm(y2 ~ x)
summary(model3)

stargazer(model3, out.header = F, table.placement = "H", out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW1/t3")

png("plot4.png")
plot(x,y2, main = "h)")
abline(lm(y2 ~ x), col = "blue", lty = 2)
abline(a=-1+eps2, b=0.5, col = "red")
legend("bottomright",c("Fitted Line","Population Line"),
        col=c("blue","red"), lty=c(2,1))
dev.off()
#g
model4 = lm(y2 ~ x + I(x^2))
summary(model4)

stargazer(model4, out.header = F, table.placement = "H", out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW1/t4")

#h
eps3 = rnorm(100, 0, 2)
y3 = -1 + 0.5*x + eps3

length(y3) #100
# beta_0 = -1
# beta_1 = 0.5

png("plot5.png")
plot(x,y3)
dev.off()

model5 = lm(y3 ~ x)
summary(model5)

stargazer(model5, out.header = F, table.placement = "H", out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW1/t5")

png("plot6.png")
plot(x,y3, main = "i)")
abline(lm(y3 ~ x), col = "blue", lty = 2)
abline(a=-1+eps3, b=0.5, col = "red")
legend("bottomright",c("Fitted Line","Population Line"),
       cex=.8, col=c("blue","red"), lty=c(2,1))
dev.off()

#i
model6 = lm(y3 ~ x + I(x^2))
summary(model6)

stargazer(model6, out.header = F, table.placement = "H", out = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW1/t6")


#j
CI1 = confint.lm(model1)
CI2 = confint.lm(model2)
CI3 = confint.lm(model3)
CI4 = confint.lm(model4)
CI5 = confint.lm(model5)
CI6 = confint.lm(model6)


CImat = matrix(NA, 6, 4)
colnames(CImat) = c("Simple Mod. Lower", "Simple Mod. Upper",
                    "Quadr. Mod. Lower", "Quadr. Mod. Upper")
rownames(CImat) = c("DGP SD 1 Slope", "DGP SD 1 Intercept", "DGP SD 0.1 Slope", 
                    "DGP SD 0.1 Intercept", "DGP SD 4 Slope", "DGP SD 4 Intercept")

CImat[1,1:2] = CI1[2,]
CImat[1,3:4] = CI2[2,]
CImat[2,1:2] = CI1[1,]
CImat[2,3:4] = CI2[1,]

CImat[3,1:2] = CI3[2,]
CImat[3,3:4] = CI4[2,]
CImat[4,1:2] = CI3[1,]
CImat[4,3:4] = CI4[1,]

CImat[5,1:2] = CI5[2,]
CImat[5,3:4] = CI6[2,]
CImat[6,1:2] = CI5[1,]
CImat[6,3:4] = CI6[1,]

print(xtable(CImat,  caption = "Confidence Interals: Lower denotes 2.5\\%, Upper 97.5\\%", 
             digits = 3, label = "BSBY"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Tables/HW1/t7")

file.copy("/Users/ts/Library/Mobile Documents/com~apple~CloudDocs/Uni/UM/Year 3/Exchange Courses/FIN ECMT/Code/HW1.R",
          "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT HW/Code/HW1/HW1.R", overwrite = T)
