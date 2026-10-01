library(MoTBFs)
load("../DataSets/datos.RData")
# compute test index
set.seed(13458)
folds = MoTBFs:::splitFolds(data,10)
indexTest <- c()
for (i in 1:10) {
  indexTest[[i]] <- attributes(folds[[i]])$index$test
}
save("../DataSets/indexTest.RData")


# compute the folds
computeFolds = function(indexTest,data){
  folds = list()
  
  for(i in 1:9){
    folds[[i]] <- list(Test = data[indexTest[[i]],],
                       Validation = data[indexTest[[i+1]],],
                       Training = data[-c(indexTest[[i]],indexTest[[i+1]]),]
    )
    folds[[K]] = list(Test = data[indexTest[[10]],],
                      Validation = data[indexTest[[1]],],
                      Training = data[-c(indexTest[[1]],indexTest[[10]]),])
  }
  return(folds)
}

folds = computeFolds(indexTest,data)
save(folds,file="../DataSets/foldC.RData")

# Discrete case--------------
load("../DataSets/datosD.RData")
folds = computeFolds(indexTest,data)
save(folds,file="../DataSets/foldD.RData")