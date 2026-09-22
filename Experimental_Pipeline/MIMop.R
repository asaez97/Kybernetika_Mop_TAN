## Balanceados---------------------------------
source("fitTAN.R")
load("../DataSets/foldC.RData")
library(doParallel)
# cores = detectCores()
# cl <- makeCluster(cores[1]-1)
# registerDoParallel(cl)
i = 9
library("MoTBFs")
source("SeleccionMoPImputs.R")

MIMop = foreach(i = 1:10,.packages = c("MoTBFs"))%do%{
  mod = mutual_information_tan(data = folds[[i]]$Training,target = "DUP.",
                               fit.args,parallel = T)
  # save(mod,file=paste0("../DataSets/MIMop",i,".RData"))
  return(mod)
}
# stopCluster(cl)
# save(MIMop,file="../DataSets/MIMop.RData")


## Balanceados---------------------------------
source("fitTAN.R")
load("../DataSets/balancedMoP40.RData")
library(doParallel)
# cores = detectCores()
# cl <- makeCluster(cores[1]-1)
# registerDoParallel(cl)
i = 9
library("MoTBFs")
source("SeleccionMoPImputs.R")

MIMopB = foreach(i = 1:10,.packages = c("MoTBFs"))%do%{
  mod = mutual_information_tan(data = balancedFold[[i]]$Training,target = "DUP.",
                         fit.args,parallel = T)
  # save(mod,file=paste0("../DataSets/MIMopB40",i,".RData"))
  return(mod)
}
# stopCluster(cl)
# save(MIMopB,file="../DataSets/MIMopB40.RData")
