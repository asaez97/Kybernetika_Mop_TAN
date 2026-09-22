# Calculo Informacion mutua de las variables predictoras con la variable DUP

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# carga de la base de datos
load("../DataSets/datos.RData")
load("../dataSets/indexTest.RData")
library(MoTBFs)
computeFolds=function(foldsIndex,data,k=0,seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  if(is.null(foldsIndex)){
    # Split dataset in k-train and test sets
    if(k == 0){
      folds = list(list(Training = data, Test = data))
      K = 1
    }else if(k == 1){
      if(is.null(loss.args$p.test)){
        p.test = 0.2
      }else{
        p.test = loss.args$p.test
      }
      
      folds = list(TrainingandTestData(data, percentage_test = p.test))
      K = 1
    }else{
      folds = splitFolds(data,k)
      K = k
    }
  }else{# Se han introducido los indices de test de cada fold
    K = length(foldsIndex)
    folds <- list()
    for(i in 1:(K-1)){
      folds[[i]] <- list(Test = data[foldsIndex[[i]],],
                         Validation = data[foldsIndex[[i+1]],],
                         Training = data[-c(foldsIndex[[i]],foldsIndex[[i]]),]
                         )
    }
    folds[[K]] = list(Test = data[foldsIndex[[K]],],
                      Validation = data[foldsIndex[[1]],],
                      Training = data[-c(foldsIndex[[1]],foldsIndex[[K]]),])
  }
  return(folds)
}
# folds = computeFolds(indexTest,data)
# save(folds,file="../DataSets/foldC.RData")

# library(doParallel)
# library(foreach)
# Variable a predecir
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

# orden = lapply(1:10,function(i){
#   ordenMop(folds[[i]]$Training)
# })

library(doParallel)
library(foreach)
# cores = detectCores()
# cl <- makeCluster(cores[1]-1)
# registerDoParallel(cl)
# orden = foreach(i = 1:10,.packages = "MoTBFs")%dopar%{
#   ordenMop(folds[[i]]$Training)
# }
# stopCluster(cl)

# Guardamos el orden
# save(orden,file = "../DataSets/ordenM.RData")
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
  
  bestdisc = which.max(infoMutuaDiscretas)
  orden = sort(c(infoMutuaDiscretas,infoMutuaCont),decreasing = T)
  return(orden)
}
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
save(orden,file = "../DataSets/ordenCG.RData")

## Discreto----------------------------------------------------
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
