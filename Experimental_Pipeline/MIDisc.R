# Informacion mutua Discreta para Tan
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("fitTANGauss.R")
load("../DataSets/foldD.RData")
library(infotheo)
library(doParallel)
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
i = 1
MID = foreach(i = 1:10,.packages = c("infotheo"))%dopar%{
  mutual_information_tan_d(data_new = folds[[i]]$Training,target = "DUP.")
}
stopCluster(cl)
# save(MID,file="../DataSets/MIDis.RData")

## Balanceado-------------------------
source("fitTANGauss.R")
load("../DataSets/balancedD40.RData")
library(infotheo)
library(doParallel)
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
i = 1
MIDB = foreach(i = 1:10,.packages = c("infotheo"))%dopar%{
  mutual_information_tan_d(data_new = balancedFold[[i]]$Training,target = "DUP.")
}
stopCluster(cl)

# save(MIDB,file="../DataSets/MIDisB40.RData")
