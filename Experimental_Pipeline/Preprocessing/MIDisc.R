# Informacion mutua Discreta para Tan
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages-----------------
library(infotheo)
library(doParallel)
library(MoTBFs)

# Not balanced--------------------------------
load("../DataSets/foldD.RData")

cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
i = 1
MID = foreach(i = 1:10,.packages = c("infotheo","MoTBFs"))%dopar%{
  mutual_information_tan(data = folds[[i]]$Training,target = "DUP.")
}
stopCluster(cl)
save(MID,file="../DataSets/MIDis.RData")

## Balanced-------------------------
load("../DataSets/balancedD40.RData")
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
i = 1
MIDB = foreach(i = 1:10,.packages = c("infotheo","MoTBFs"))%dopar%{
  mutual_information_tan(data = balancedFold[[i]]$Training,target = "DUP.")
}
stopCluster(cl)

save(MIDB,file="../DataSets/MIDisB40.RData")
