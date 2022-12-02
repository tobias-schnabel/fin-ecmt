# Tobias Schnabel, UIN 833002303

if (Sys.info()[7] == "ts") {
  setwd('/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Figures')
}

####################### Regression #######################
# Forward Stepwise Selection
png("fss.png", width = 800, height = 800, units = "px")
fss.summary = summary(fss)
par(mfrow = c(1, 3))
plot(fss.summary$cp, xlab = "# of Variables", ylab = "Mallow's Cp", type = "l")
min.cp = min(fss.summary$cp)
std.cp = sd(fss.summary$cp)
abline(h = min.cp + 0.1 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.1 * std.cp, col = "red", lty = 2)

plot(fss.summary$bic, xlab = "# of Variables", ylab = "BIC", type = "l")
min.bic = min(fss.summary$bic)
std.bic = sd(fss.summary$bic)
abline(h = min.bic + 0.1 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.1 * std.bic, col = "red", lty = 2)

plot(fss.summary$adjr2, xlab = "# of Variables", ylab = "Adjusted R2",
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(fss.summary$adjr2)
std.adjr2 = sd(fss.summary$adjr2)
abline(h = max.adjr2 + 0.1 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.1 * std.adjr2, col = "red", lty = 2)
dev.off()

# Backward Stepwise Selection
png("bss.png", width = 800, height = 800, units = "px")

bss.summary = summary(bss)
par(mfrow = c(1, 3))
plot(bss.summary$cp, xlab = "# of Variables", ylab = "Mallow's Cp", type = "l")
min.cp = min(bss.summary$cp)
std.cp = sd(bss.summary$cp)
abline(h = min.cp + 0.1 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.1 * std.cp, col = "red", lty = 2)

plot(bss.summary$bic, xlab = "# of Variables", ylab = "BIC", type = "l")
min.bic = min(bss.summary$bic)
std.bic = sd(bss.summary$bic)
abline(h = min.bic + 0.1 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.1 * std.bic, col = "red", lty = 2)

plot(bss.summary$adjr2, xlab = "# of Variables", ylab = "Adjusted R2",
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(bss.summary$adjr2)
std.adjr2 = sd(bss.summary$adjr2)
abline(h = max.adjr2 + 0.1 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.1 * std.adjr2, col = "red", lty = 2)
dev.off()

# Boosting
png("boosting.png", width = 800, height = 800, units = "px")
plot(lambdas, train.errors, type = "b", xlab = "Shrinkage Value", ylab = "Training Set MSE",
     col = "red", pch = 2)
plot(lambdas, test.errors, type = "b", xlab = "Shrinkage Value", ylab = "Test Set MSE",
     col = "orange", pch = 1)
summary(boost.best, cBars = 10)
dev.off()


####################### Classification #######################

#KNN
png("knn.png", width = 800, height = 800, units = "px")
plot(err_list_knn, col = "red", pch = 4, xlab = "k", ylab = "Test Error Rate")
abline(v = which(err_list_knn == min(err_list_knn)), lty = 2, col = "blue")
dev.off()



setwd("/Users/ts/Git/fin-ecmt")