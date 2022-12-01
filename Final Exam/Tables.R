# Tobias Schnabel, UIN 833002303

if (Sys.info()[7] == "ts") {
 # setwd('/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Tables')
}

#Regression
# collect all MSEs
r.mse.mat = as.matrix(cbind(rbind(r.RF.SQ.MSE, r.KNNREG.MSE, r.LM.MSE, 
                                  r.PLS.MSE, r.PCR.MSE,
                                  r.FSS.MSE, r.BSS.MSE, 
                                  r.RIDGE.MSE, r.LASSO.MSE, r.RF.MSE, r.BOOST.sq.MSE, 
                                  r.GAM.SQ.MSE,r.BOOST.MSE, 
                                  r.splines.cubic.MSE, r.splines.MSE),
                            rep("", 15)))
colnames(r.mse.mat) = c("Test MSE", "Tuning Parameter Value")
rownames(r.mse.mat) = c("Random Forest with x^2",
                        "KNN Regression",
                        "Linear Model (Baseline)",
                        "PLS", "PCR",
                        "Forward Stepwise Selection",
                        "Backward Stepwise Selection",
                        "Ridge Regression", "LASSO", 
                        "Random Forest",
                        "Boosted Regression with x^2",
                        "GAM with x^2",
                        "Boosted Regression",
                        "Spline Regression (Cubic)",
                        "Spline Regression (Automatic)")

#compute min and max smothing df for auto spline
sdf.max = round(max(splines$smooth.frame), 2)
sdf.min = round(min(splines$smooth.frame), 2)

r.mse.mat[1,2] = paste("mtry = ", toString(rf.sq$mtry))
r.mse.mat[2,2] = paste("k = ", toString(knn.r.best$k))
r.mse.mat[3,2] = ""
r.mse.mat[4,2] = ""
r.mse.mat[5,2] = ""
r.mse.mat[6,2] = "x08,x12,x14,x16,x31,x33,x35,x37,x39,x40"
r.mse.mat[7,2] = "x08,x02,x13,x14,x16,x19,x31,x35,x37,x40"
r.mse.mat[8,2] = paste("$\\lambda=$", toString(round(lasso$lambda.min, 2)))
r.mse.mat[9,2] = paste("$\\lambda=$", toString(round(ridge$lambda.min, 2)))
r.mse.mat[10,2] = paste("mtry = ", toString(rf$mtry))
r.mse.mat[11,2] = paste("$\\shrinkage=$", toString(round(boost.sq.best$shrinkage, 4)))
r.mse.mat[12,2] = ""
r.mse.mat[13,2] = paste("$\\shrinkage=$", toString(round(boost.best$shrinkage, 4)))
r.mse.mat[14,2] = "df = 3"
r.mse.mat[15,2] = paste(toString(sdf.min), " < df < ", toString(sdf.max))

print(xtable(r.mse.mat,  caption = "Regression Test MSE and Tuning Parameters",
             digits = c(0,4,0),),
      caption.placement = "top",
      label = index,  table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Tables/regmse")

setwd(path)