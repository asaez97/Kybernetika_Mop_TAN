setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Balanceamiento de los folds

load("../DataSets/datos.RData")
load("../DataSets/indexTest.RData")
source("variableSelection.R")
folds = computeFolds(indexTest,data)
rm(data)
library(ROSE)

balancedData = function(data,target_index,seed){
  
  data[[target_index]]= as.character(data[[target_index]])
  data2 = ovun.sample(DUP.~., data, method="under", p=0.4,
                      seed = seed)
  numLevels = sapply(data2$data[,-target_index], nlevels)
  continuas = names(numLevels[numLevels==0])
  minimos = sapply(continuas,function(i){
    min(data2$data[i])
  })
  minimosO = sapply(continuas,function(i){
    min(data[i])
  })
  varMin = names(which(minimos-minimosO >0))
  indexMin = sapply(varMin, function(i){
    which.min(data[[i]])
  },USE.NAMES = F)
  maximos = sapply(continuas,function(i){
    max(data2$data[i])
  })
  
  maximosO = sapply(continuas,function(i){
    max(data[i])
  })
  varMax = names(which(maximos-maximosO <0))
  indexMax = sapply(varMax, function(i){
    which.max(data[[i]])
  },USE.NAMES = F)
  index = unique(c(indexMin,indexMax))
  # browser()
  dataNew = data2$data
  if(length(index)>0){
    dataNew = rbind(dataNew,data[index,])
  }
  return(dataNew)
}
balancedFold = folds
for(i in 1:length(folds)){
  seeds = c(154,356,4841,216,116,169,1223,748,9092,1002)
  balancedFold[[i]]$Training = balancedData(folds[[i]]$Training,1,seeds[i])
  balancedFold[[i]]$Training[[1]] = factor(balancedFold[[i]]$Training[[1]],
                                           levels = c("Yes","No"), ordered = T)
}
# save(balancedFold,file = "../DataSets/balancedMoP40.RData")

## Base de datos discreta--------------------
load("../DataSets/foldD.RData")
# folds = computeFolds(foldsIndex =indexTest,data)
balancedFold = folds
for(i in 1:length(folds)){
  seeds = c(154,356,4841,216,116,169,1223,748,9092,1002)
  folds[[i]]$Training[[1]]= as.character(folds[[i]]$Training[[1]])
  balancedFold[[i]]$Training = ovun.sample(DUP.~., folds[[i]]$Training, method="under", p=0.4,
                                          seed = seeds[i])$data
  balancedFold[[i]]$Training[[1]] = factor(balancedFold[[i]]$Training[[1]],
                                           levels = c("Yes","No"), ordered = T)
}
# save(balancedFold,file = "../DataSets/balancedD40.RData")

