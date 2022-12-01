# Tobias Schnabel, UIN 833002303

if (Sys.info()[7] == "ts") {
 # setwd('/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Tables')
}

####################### Regression #######################
# collect all MSEs
r.mse.mat = as.matrix(cbind(rbind(r.KNNREG.MSE, r.LM.MSE, 
                                  r.PLS.MSE, r.PCR.MSE,
                                  r.FSS.MSE, r.BSS.MSE, 
                                  r.RIDGE.MSE, r.LASSO.MSE, r.RF.MSE, 
                                  r.RF.SQ.MSE, r.BOOST.sq.MSE, 
                                  r.GAM.SQ.MSE,r.BOOST.MSE, 
                                  r.splines.cubic.MSE, r.splines.MSE),
                            rep("", 15)))

#compute min and max smothing df for auto spline
sdf.max = round(max(r.splines$smooth.frame), 2) +0.01
sdf.min = round(min(r.splines$smooth.frame), 2) -0.1
# paste("mtry = ", toString(rf.sq$mtry))
r.mse.mat[1,2] = paste("k = ", toString(knn.r.best$k))
r.mse.mat[2,2] = ""
r.mse.mat[3,2] = ""
r.mse.mat[4,2] = ""
r.mse.mat[5,2] = "x08,x12,x14,x16,x31,x33,x35,x37,x39,x40"
r.mse.mat[6,2] = "x08,x02,x13,x14,x16,x19,x31,x35,x37,x40"
r.mse.mat[7,2] = paste("lambda = ", toString(round(lasso$lambda.min, 2)))
r.mse.mat[8,2] = paste("lambda = ", toString(round(ridge$lambda.min, 2)))
r.mse.mat[9,2] = paste("mtry = ", toString(rf$mtry))
r.mse.mat[10,2] = paste("mtry = ", toString(rf.sq$mtry))
r.mse.mat[11,2] = paste("shrinkage", toString(round(boost.sq.best$shrinkage, 4)))
r.mse.mat[12,2] = ""
r.mse.mat[13,2] = paste("shrinkage", toString(round(boost.best$shrinkage, 4)))
r.mse.mat[14,2] = "df = 3"
r.mse.mat[15,2] = paste(toString(sdf.min), " < df < ", toString(sdf.max))

r.mse = as.data.frame(r.mse.mat)
r.mse = as.data.frame(lapply(r.mse, function(x) if (anyNA(y <- as.numeric(x))) x else y))
colnames(r.mse) = c("Test MSE", "Tuning Parameter Value")
rownames(r.mse) = c("KNN Regression",
                        "Linear Model (Baseline)",
                        "PLS", "PCR",
                        "Forward Stepwise Selection",
                        "Backward Stepwise Selection",
                        "Ridge Regression", "LASSO", 
                        "Random Forest",
                        "Random Forest with x^2",
                        "Boosted Regression with x^2",
                        "GAM with x^2",
                        "Boosted Regression",
                        "Spline Regression (Cubic)",
                        "Spline Regression (Automatic)")

print(xtable(r.mse,  caption = "Regression Test MSE and Tuning Parameters",
             digits=c(0,3,0), align = "r|c|l"), 
      caption.placement = "top",
      label = index,  table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Tables/regmse")

####################### Classification #######################
# collect all Test Errors
c.te.mat = as.matrix(cbind(rbind(knn.err, qda.err, c.rf.err, round(svm.poly.err, 3),
                                 round(svm.lin.err, 3)
                                 , lda.err, 
                                 round(svm.rad.err, 4)), 
                            rep("", 7)))
colnames(c.te.mat) = c("Test Error Rate", "Tuning Parameter Value")
rownames(c.te.mat) = c("KNN", "QDA", "Random Forest", "SVM (polyn. kernel)",
                       "SVM (lin. kernel)", "LDA", "SVM (radial kernel)")


c.te.mat[1,2] = paste("k=", toString(which(err_list_knn == min(err_list_knn))))
c.te.mat[2,2] = ""
c.te.mat[3,2] = paste("mtry=", toString(c.rf$mtry))
c.te.mat[4,2] = paste("gamma=", toString(tune.poly$best.parameters[2]), " ",
                      ", cost =", toString(tune.poly$best.parameters[1]))
c.te.mat[5,2] = paste("gamma=", toString(tune.lin$best.parameters[2]), " ",
                      ", cost =", toString(tune.lin$best.parameters[1]))
c.te.mat[6,2] = ""
c.te.mat[7,2] = paste("gamma=", toString(tune.rad$best.parameters[2]), " ",
                      ", cost =", toString(tune.rad$best.parameters[1]))

print(xtable(c.te.mat,  caption = "Classification Test Error Rate and Tuning Parameters",
             digits = c(0,4,0), align = "r|c|l"), 
      caption.placement = "top",
      label = index,  table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Tables/classter")


setwd(path)