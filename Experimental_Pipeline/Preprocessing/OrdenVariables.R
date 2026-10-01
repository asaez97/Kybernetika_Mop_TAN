# Calculo Informacion mutua de las variables predictoras con la variable DUP

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Order MOP distributions----------------
load("../dataSets/foldC.RData")
library(MoTBFs)
ordenMop = function(data){
  
  fit.args = list(scale=F,POTENTIAL_TYPE = "MOP", numIntervals = 4, maxParam = 7)
  MI = sapply(colnames(data[,-1]), function(i){
      mutInfo = MoTBFs:::mut_information(data = data[,c("DUP.",i)],fit.args = fit.args)
  })
  MI = sort(MI,T)
  return(MI)
}

# orden = lapply(1:10,function(i){
#   ordenMop(folds[[i]]$Training)
# })

library(doParallel)
library(foreach)
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
# orden = foreach(i = 1:10,.packages = "MoTBFs")%dopar%{
#   ordenMop(folds[[i]]$Training)
# }
# stopCluster(cl)

# Guardamos el orden
# save(orden,file = "../DataSets/ordenM.RData")

## Balanced MoP--------------------------
# carga de la base de datos
load("../DataSets/balancedMoP40.RData")

registerDoParallel(cl)
orden = foreach(i = 1:10,.packages = "MoTBFs")%dopar%{
  ordenMop(balancedFold[[i]]$Training)
}
stopCluster(cl)

# Guardamos el orden
# save(orden,file = "../DataSets/ordenMB40.RData")

## Condicional Gaussiano-------------------------------------
ordenGauss = function(data){
  numLevels = sapply(data[,-1],nlevels)
  
  numericVars = names(numLevels[numLevels==0])
  discreteVars = names(numLevels[numLevels!=0])
  target = "DUP."
  
  infoMutuaDiscretas = sapply(discreteVars,function(i){
    mutinformation(data[[i]],data[[target]])
  })
  source("fitTANGauss.R")
  infoMutuaCont = sapply(numericVars,function(i){
    mi_cond_gauss(i,target = target,data = data[,c(target,i)])
  })
  orden = sort(c(infoMutuaDiscretas,infoMutuaCont),decreasing = T)
  return(orden)
}
load("../dataSets/foldC.RData")
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
i = 1
library(infotheo)
orden = foreach(i = 1:10,.packages = c("infotheo"))%dopar%{
  ordenGauss(folds[[i]]$Training)
}
stopCluster(cl)

# Guardamos el orden
# save(orden,file = "../DataSets/ordenCG.RData")
## Balanced gauss---------------------------
# carga de la base de datos
load("../DataSets/balancedMoP40.RData")

registerDoParallel(cl)
i = 1
orden = foreach(i = 1:10,.packages = c("infotheo"))%dopar%{
  ordenGauss(balancedFold[[i]]$Training)
}
stopCluster(cl)

# Guardamos el orden
# save(orden,file = "../DataSets/ordenCGB40.RData")


## Discreto----------------------------------------------------
library(infotheo)
ordenDisc = function(data){
  mutualinfo = sapply(data[,-1],mutinformation,Y=data$DUP.)
  orden = sort(mutualinfo,decreasing = T)
  return(orden)
}
load("../DataSets/datosD.RData")
folds = computeFolds(indexTest,data)
save(folds,file="../DataSets/foldD.RData")
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
orden = foreach(i = 1:10,.packages = c("infotheo"))%dopar%{
  ordenDisc(folds[[i]]$Training)
}
stopCluster(cl)
# Guardamos el orden
# save(orden,file = "../DataSets/ordenD.RData")
## Balanced discrete----------------------
load("../DataSets/balancedD40.RData")
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
orden = foreach(i = 1:10,.packages = c("infotheo"))%dopar%{
  ordenDisc(balancedFold[[i]]$Training)
}
stopCluster(cl)
# Guardamos el orden
# save(orden,file = "../DataSets/ordenDB40.RData")