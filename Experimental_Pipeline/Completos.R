# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Paquetes----------------------------------------------------------------
library(bnlearn)
library(MoTBFs)
library(logging) # Sirve para imprimir la consola en .txt
library(pROC) # AUC
library(doParallel)
# Direccion donde se va a almacenar los datos--------------------------
directory = "../Experimentacion/"

# Parametros introducidos ---------------------------------------------------
# argumentos <- commandArgs(trailingOnly = T)
argumentos <- c("TAN","Disc")
# Estructura del grafo
dag <- argumentos[1]

# Tipo de distibuciones que se van a emplear Mop o Disc
Tipo.Dist <- argumentos[2]

nameLoss <- "Completo"
time=Sys.time()

time = paste0("",time)

# Formateamos el nombre
time <- gsub(pattern = ":",replacement = "_",time)
time <- gsub(pattern = " ",replacement = "_",time)
time <- gsub(pattern = "\\.",replacement = "_",time)
time
Nombre.Red = paste(c(dag,Tipo.Dist,nameLoss,time),collapse = "+")
Nombre.Red
if(Tipo.Dist=="Mop"){
  source("SeleccionMoPImputs.R")
}else if(Tipo.Dist=="Disc"){
  source("SeleccionDiscImputs.R")
}else{# Condicional Gaussiano
  source("SeleccionGaussImputs.R")
}

listaAlmacenar$Nombre.Red <- Nombre.Red
listaAlmacenar$Red <- dag
listaAlmacenar$Criterio <- nameLoss
# loginfo(paste("Learning", dag, Tipo.Dist,"Target", target ,"Seed:", seeds,
#               "selectivo", nameLoss, collapse = " "))

if(Tipo.Dist=="Gauss"){
  if(dag=="NB"){
    dag2 = MoTBFs:::getStructure(data = folds[[1]]$Training,method = "NB",
                                 target = "DUP.")
    i = 1
    cl <- makeCluster(10)
    registerDoParallel(cl)
    bnCV = foreach(i = 1:10,
                   .packages = c("bnlearn"))%dopar%{
                     fitted= bn.fit(dag2,folds[[i]]$Training)
                     probs = sapply(1:nrow(folds[[i]]$Test),function(j){
                       cpquery(fitted,
                               event = (DUP.=="Yes"),
                               evidence = as.list(folds[[i]]$Test[j,-1]),
                               method = "lw")
                     })
                     pred = factor(ifelse(probs>=0.5,"Yes","No"),levels = c("Yes","No"))
                     attr(pred,"probs")<-probs
                     observations = folds[[i]]$Test[,target]
                     loss = lossFunctions("ACC",pred,observations)
                     return(list(fitted = fitted,predictions = pred,
                                 observations = observations,
                                 loss = loss))
                   }
    stopCluster(cl)
  }else{
    source("fitTANGauss.R")
    load("../Datasets/MIGauss.RData")
    load("../Datasets/OrdenCG.RData")
    i = 1
    cl <- makeCluster(10)
    registerDoParallel(cl)
    bnCV = foreach(i = 1:10,
                   .packages = c("bnlearn"))%dopar%{
                     source("fitTANGauss.R")
                     fitted= fit_tan_g(target=target,
                                       data=folds[[i]]$Training,all = F,
                                       mutualInfoCond = MIGauss[[i]],root = "INTE")
                     probs = sapply(1:nrow(folds[[i]]$Test),function(j){
                       cpquery(fitted,
                               event = (DUP.=="Yes"),
                               evidence = as.list(folds[[i]]$Test[j,-1]),
                               method = "lw")
                     })
                     pred = factor(ifelse(probs>=0.5,"Yes","No"),levels = c("Yes","No"))
                     attr(pred,"probs")<-probs
                     observations = folds[[i]]$Test[,target]
                     loss = lossFunctions("ACC",pred,observations)
                     return(list(fitted = fitted,predictions = pred,
                                 observations = observations,
                                 loss = loss))
                   }
    stopCluster(cl)
  }
}else if(Tipo.Dist=="Mop"){
  if(dag=="NB"){
    dag2 = MoTBFs:::getStructure(data = folds[[1]]$Training,method = "NB",
                                 target = "DUP.")
    
    i = 1
    cl <- makeCluster(10)
    registerDoParallel(cl)
    bnCV = foreach(i = 1:10,
                     .packages = c("logging", "MoTBFs","bnlearn"))%dopar%{
                       fit.args$data = folds[[i]]$Training
                       fit.args$graph = dag2
                       fitted= do.call(motbf.fit,fit.args)
                       pred = predict(fitted,target = target,
                                      data = folds[[i]]$Test,prob = T)
                       observations = folds[[i]]$Test[,target]
                       loss = lossFunctions("ACC",pred,observations)
                       return(list(fitted = fitted,predictions = pred,
                                   observations = observations,
                                   loss = loss))
                     }
    stopCluster(cl)
  }else{
      source("fitTAN.R")
      source("mutualInformation.R")
      load("../Datasets/MIMOP.RData")
      load("../Datasets/ordenM.RData")
      i = 1
      cl <- makeCluster(10)
      registerDoParallel(cl)
      bnCV = foreach(i = 1:10,
                     .packages = c("logging", "MoTBFs","bnlearn"))%dopar%{
                       fitted= fit_tan(target = target,data = folds[[i]]$Training,
                                       mutualInfoCond = MIMop[[i]]$MI,
                                       root = names(orden[[i]][1]))
                       pred = predict(fitted,target = target,
                                      data = folds[[i]]$Test,prob = T)
                       observations = folds[[i]]$Test[,target]
                       loss = lossFunctions("ACC",pred,observations)
                       return(list(fitted = fitted,predictions = pred,
                                   observations = observations,
                                   loss = loss))
                     }
      stopCluster(cl)
  }
}else{
  if(dag=="NB"){
    dag2 = MoTBFs:::getStructure(data = folds[[1]]$Training,method = "NB",
                                 target = "DUP.")
    
    i = 1
    cl <- makeCluster(10)
    registerDoParallel(cl)
    bnCV = foreach(i = 1:10,
                   .packages = c("logging", "MoTBFs","bnlearn"))%dopar%{
                     fit.args$data = folds[[i]]$Training
                     fit.args$graph = dag2
                     fitted= do.call(motbf.fit,fit.args)
                     pred = predict(fitted,target = target,
                                    data = folds[[i]]$Test,prob = T)
                     observations = folds[[i]]$Test[,target]
                     loss = lossFunctions("ACC",pred,observations)
                     return(list(fitted = fitted,predictions = pred,
                                 observations = observations,
                                 loss = loss))
                   }
    stopCluster(cl)
  }else{
    source("fitTAN.R")
    source("mutualInformation.R")
    load("../Datasets/MIDis.RData")
    load("../Datasets/ordenD.RData")
    i = 1
    cl <- makeCluster(10)
    registerDoParallel(cl)
    bnCV = foreach(i = 1:10,
                   .packages = c("logging", "MoTBFs","bnlearn"))%dopar%{
                     fitted= fit_tan(target = target,data = folds[[i]]$Training,
                                     mutualInfoCond = MID[[i]],
                                     root = names(orden[[i]][1]))
                     pred = predict(fitted,target = target,
                                    data = folds[[i]]$Test,prob = T)
                     observations = folds[[i]]$Test[,target]
                     loss = lossFunctions("ACC",pred,observations)
                     return(list(fitted = fitted,predictions = pred,
                                 observations = observations,
                                 loss = loss))
                   }
    stopCluster(cl)
  }
}
# save(bnCV,file = paste(directory,Nombre.Red,".RData",
#                        collapse = "",sep = ""))

## Almacenamiento de la informacion -------------------------------------------
# Bucle que pasa por cada fold
l = 1
for(l in 1:10){
  # Eliminamos training y test de cada fold
  listaAlmacenar$Variables[l] <- paste0(names(bnCV[[l]]$fitted),collapse = ";")
  listaAlmacenar$nVariables[l] <- length(bnCV[[l]]$fitted)
  listaAlmacenar$nCont[l] = sum(sapply(bnCV[[l]]$fitted,"[[","type")=="Continuous")
  # Parametro que indica el fold en el .csv
  listaAlmacenar$K[l] <- l
  
  # Compute matriz de confusion
  cm <- table(bnCV[[l]]$observations,bnCV[[l]]$predictions)
  names(dimnames(cm)) <- c('Observed', 'Predicted')
  listaAlmacenar$TP[l] <- cm[1,1]
  listaAlmacenar$FP[l] <- cm[2,1]
  listaAlmacenar$FN[l] <- cm[1,2]
  listaAlmacenar$TN[l] <- cm[2,2]
  # Almacenamos el AUC de cada fold
  # ?roc
  Obs = bnCV[[l]]$observations=="Yes"
  if(Tipo.Dist=="Gauss"){
    Pred = attr(bnCV[[l]]$predictions,"prob")
  }else{
    Pred = attr(bnCV[[l]]$predictions,"prob")[,1] 
  }
  ROC = roc(Obs,Pred)
  # plot(ROC,col="blue")
  listaAlmacenar$LossK[l]=bnCV[[l]]$loss
  listaAlmacenar$ACC[l] = sum(diag(cm))/sum(cm)
  listaAlmacenar$AUC[l] = auc(ROC)
}

# # Escritura en el fichero
# write.table(listaAlmacenar, file = paste0(directory,Nombre.Red,
#                                           ".csv",collapse = ""),
#             append = TRUE, sep = ";", row.names = F, col.names = T,
#             fileEncoding = "utf8") 
# # Guardar la red
# save(bnCV,file = paste(directory,Nombre.Red,".RData",
#                        collapse = "",sep = ""))