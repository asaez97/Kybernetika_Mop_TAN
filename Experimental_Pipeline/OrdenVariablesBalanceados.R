# Calculo Informacion mutua de las variables predictoras con la variable DUP

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# carga de la base de datos
load("../DataSets/balancedMoP40.RData")
library(MoTBFs)
ordenMop = function(data){
  # browser()
  Y = data$DUP.
  data = data[,-1]
  
  fit.args = list(scale=F,POTENTIAL_TYPE = "MOP", numIntervals = 4, maxParam = 7)
  MI = sapply(colnames(data), function(i){
    source("mutualInformation.R")
    X = data[,i]
    if(nlevels(X)==0){
      mutInfo = (mut_information_cont_dis(data.frame(X,Y),fit.args = fit.args)+
                   mut_information_dis_cont(data.frame(X,Y),fit.args = fit.args))/2
      
    }else{
      mutInfo = mut_information_dis(data.frame(X,Y))
    }
  })
  MI = sort(MI,T)
  return(MI)
}

library(doParallel)
library(foreach)
cores = detectCores()
cl <- makeCluster(cores[1]-1)
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
  
  info_gauss = sort(c(infoMutuaDiscretas,infoMutuaCont),decreasing = T)
  return(info_gauss)
}
library(infotheo)
cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
i = 1
orden = foreach(i = 1:10,.packages = c("infotheo"))%dopar%{
  ordenGauss(balancedFold[[i]]$Training)
}
stopCluster(cl)

# Guardamos el orden
# save(orden,file = "../DataSets/ordenCGB40.RData")

## Discreto----------------------------------------------------
ordenDisc = function(data){
  mutualinfo = sapply(data[,-1],mutinformation,Y=data$DUP.)
  orden = sort(mutualinfo,decreasing = T)
  return(orden)
}
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
