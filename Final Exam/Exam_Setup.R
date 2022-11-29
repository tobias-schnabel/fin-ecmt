# Tobias Schnabel, UIN 833002303

#########################HOUSEKEEPING########################

rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("ggplot2", "ggthemes", "scales", "strucchange", "xtable", 
              "stargazer", "ggpubr", "boot", "caret", "class", "ISLR2", "MASS",
              "glmnet", "pls", "lmvar", "leaps", "gam", "gbm", "randomForest")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages
invisible(lapply(packages, library, character.only = TRUE))

Paths = c("/Users/ts/Git/fin-ecmt/final-exam")
names(Paths) = c("ts")
setwd(Paths[Sys.info()[7]])

