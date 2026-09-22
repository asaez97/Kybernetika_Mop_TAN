selective_tan = function(Nombre.Red,target, fold, nameLoss, method = "forward",
                         order, fit.args=NULL, verbose=T,root = NULL,MI=NULL,
                         parallel = T){
  if(is.numeric(order)){
    order=colnames(fold$Training)[order]
  }
  if(verbose){
    # directory = "../Experimentacion/"
    # InfoConsola <- paste0(directory,Nombre.Red,"InformacionConsola",
    #                       ".txt",collapse = "")
    # InfoConsola <- file(InfoConsola, open = "at")
    # sink(file = InfoConsola, append = T, type = "output")
    # sink(file = InfoConsola, append = T, type = "message")
  }
  ## Modelo inicial-------------------------------#
  varsModelo = order[1]
  
  trainData <- fold$Training # trainData is checked inside motbf.fit()
  testData <- MoTBFs:::check_data(fold$Validation)
  perfData = MoTBFs:::check_data(fold$Test)
  rm(fold)
  # browser()
  #Aprendemos el tan para el fold i
  MI_i = MI[varsModelo,varsModelo,drop=F]
  fitted = fit_tan(target=target,data=trainData[,c(target,varsModelo)],
                   all = F,mutualInfoCond = MI_i,root = root,
                   fit.args = fit.args,parallel = F)
  
  # Prediccion modelo
  pred = predict(fitted,target = target,
                 data = testData[,c(target,varsModelo)],prob = T)
  
  cv.mse<-list(fitted=fitted,
               loss = lossFunctions(nameLoss,pred,
                                    testData[,target]))
  
  # seleccion del criterio de seleccion de variables
  criterio = criterio_sel(nameLoss,"Discrete")
  maximize = criterio$maximize
  loss_info = criterio$loss_info
  rm(criterio)
  
  # Model accuracy
  as_actual = cv.mse$loss
  if(verbose){
    # loginfo(paste("Initial model:", paste0(c(varsModelo,target), collapse = ', '), "\n ", 
    #               loss_info ,"=", as_actual))
  }
  
  ## Wrapper approach -----------------------------------#
  globalImprove = T
  # browser()
  while(globalImprove){
    
    globalImprove = F
    i = 2
    for(i in 2:length(order)) {# Introducir 1 a 1 cada variable y calcular el accuracy.
      
      bestSet = NULL
      
      varsNew = order[i] 
      
      if(varsNew %in% varsModelo){
        next # saltar variable que ya está en el modelo
      }
      
      if(verbose){
        loginfo(paste('NEW variable:', varsNew))
      }
      ## Learning MI and distributions------------------------
      varsModeloNew = c(varsModelo,varsNew)
      # ADDITION
      ###############-
      MI_j = MI[varsModeloNew, varsModeloNew]
      fitted = fit_tan(target=target,data=trainData[,c(target,varsModeloNew)],
                       all = F,root = root, fit.args = fit.args,
                       mutualInfoCond = MI_j,
                       parallel = parallel&length(varsModeloNew)>10)
      
      # Prediccion modelo
      pred = predict(fitted,target = target,
                     data = testData[,c(target,varsModeloNew)],prob = T)
      
      cv.mse_new<-list(fitted=fitted,
                       loss = lossFunctions(nameLoss,pred,
                                            testData[,target]))
      
      # Model accuracy
      as_nuevo = cv.mse_new$loss
      
      
      # CHECK IF THE NEW MODEL OUTPERFORMS THE PREVIOUS ONE
      if(maximize){
        improve = as_nuevo > as_actual
      }else{
        improve = as_nuevo < as_actual
      }
      if(improve){
        outvar = NULL
        bestSet = varsModeloNew
        
        as_actual = as_nuevo
        cv.mse = cv.mse_new
      }
      
      
      if(!is.null(bestSet)){
        varsModelo = bestSet
        globalImprove = T
        
        if(verbose){
          if(!is.null(outvar)){ # replacement has improved more that addition
            # 
            # loginfo(paste("* Updated model:", colnames(data)[varsNew], "REPLACES", outvar,"\n",
            #               " Variables included in the current model: ", paste0(c(varsModelo, target), collapse = ', '),
            #               "\n ", loss_info ,"=", as_actual))
          }else{
            # 
            # loginfo(paste("* Updated model, variables:", paste0(c(varsModelo, target), collapse = ', '), "\n ", loss_info ,"=", as_actual))
            # 
          }
        }
      }else{
        if(verbose){
          # loginfo(paste("- Reject variable:", varsNew, "\n ", loss_info ,"=", as_nuevo))
        }
      }
    }
    # end for loop over ordered set of explanatory variables
    
    if(method %in% c('gf', 'iwsr')){
      if(verbose){
        # loginfo(paste("Improve:", globalImprove))
      }
    }else if(method == 'forward'){
      globalImprove = F
    }
  }
  if(verbose){
    # sink(type = "message")
    # sink()
    # close(InfoConsola, type = "at")
  }
  cv.mse$loss_v = cv.mse$loss
  # Performance con el test-------------
  pred = predict(cv.mse$fitted,target = target,
                 data = perfData[,c(target,varsModelo)],prob = T)
  cv.mse$observations = perfData[,target]
  cv.mse$loss = lossFunctions(nameLoss,pred,
                              perfData[,target])
  cv.mse$predictions=pred
  return(cv.mse=cv.mse)
}

library(MoTBFs)
library(logging)
library(pROC) # AUC
source("SeleccionDiscImputsBalanced.R")
source("fitTAN.R")
source("mutualInformation.R")
load("../DataSets/MIDisB40.RData")
dag <- "TAN"
# nameLoss <- commandArgs(trailingOnly = T)
nameLoss <- "ACC"
time=Sys.time()
time = paste0("",time)

# Formateamos el nombre
time <- gsub(pattern = ":",replacement = "_",time)
time <- gsub(pattern = " ",replacement = "_",time)
time <- gsub(pattern = "\\.",replacement = "_",time)
time
Nombre.Red = paste(c(dag,Tipo.Dist,"Balanced",nameLoss,time),collapse = "+")
Nombre.Red
listaAlmacenar$Nombre.Red <- Nombre.Red
listaAlmacenar$Red <- dag
listaAlmacenar$Criterio <- nameLoss
## Salida por consola de verificacion no en el documento-----------------------
# loginfo(paste("Learning", dag, Tipo.Dist,"Target", target ,"Seed:", seeds,
#               "selectivo", nameLoss, collapse = " "))
library(doParallel)
cores = detectCores()
cl <- makeCluster(10)
registerDoParallel(cl)
bnCV = foreach(i = 1:10,.packages = c("logging", "MoTBFs","bnlearn"))%dopar%{
  mod = selective_tan(paste0(Nombre.Red,"_",i),target,balancedFold[[i]],nameLoss,
                      order = names(orden[[i]]),
                      fit.args=fit.args,verbose = T,
                      MI = MIDB[[i]],root = names(orden[[i]])[1],parallel = F)
}
stopCluster(cl)

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
# rm(bnCV)
