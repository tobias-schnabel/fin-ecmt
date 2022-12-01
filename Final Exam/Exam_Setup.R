# Tobias Schnabel, UIN 833002303

#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("ggplot2", "ggthemes", "scales", "strucchange", "xtable", 
              "stargazer", "ggpubr", "boot", "caret", "class", "ISLR2", "MASS",
              "glmnet", "pls", "lmvar", "leaps", "gam", "gbm", "randomForest", "e1071")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages
invisible(lapply(packages, library, character.only = TRUE))

###################### set paths ######################
path = "/Users/ts/Git/fin-ecmt/Final Exam"
setwd(path)

###################### execute main scripts ######################
source('Schnabel-regression.R', local = T)
source('Schnabel-classification.R', local = T)
######################## CLEANUP AND EXPORT ########################
{
  if (Sys.info()[7] == "ts") {
    
    ########################Do Plots & Tables################################
    source("Plots.R")
    setwd(path)
    source("Tables.R")
    setwd(path)
    ########################R File########################
    file.copy('Schnabel-classification.R', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
    file.copy('Schnabel-regression.R', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
    file.copy('Plots.R', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
    file.copy('Tables.R', '/Users/ts/Dropbox/Apps/Overleaf/FIN ECMT Final Assignment/Code', overwrite = T)
  }
}
