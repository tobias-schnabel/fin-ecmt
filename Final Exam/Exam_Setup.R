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

###################### set paths ######################
setwd("/Users/ts/Git/fin-ecmt/Final Exam")

###################### execute main scripts ######################
source('Schnabel-regression.R')
source('Schnabel-classification.R')

########################CLEANUP AND EXPORT
{
  if (Sys.info()[7] == "ts") {
    
    ########################Do Plots & Tables################################
    source("Plots.R")
    source("Tables.R")
    ########################R File########################
    file.copy('Schnabel-classification.R', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
    file.copy('Schnabel-regression', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
    file.copy('Plots.R', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
    file.copy('Tables.R', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
  }
}
