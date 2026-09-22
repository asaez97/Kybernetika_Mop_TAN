selective_tan_g = function(Nombre.Red, target, fold, nameLoss, method = "forward",
                             order,MI, root=NULL, verbose=T){
  source("fitTANGauss.R")
  if(is.numeric(order)){
    order=colnames(fold$Training)[order]
  }
  
  # if(verbose){
  #   directory = "../Experimentacion/"
  #   InfoConsola <- paste0(directory,Nombre.Red,"InformacionConsola",
  #                         ".txt",collapse = "")
  #   InfoConsola <- file(InfoConsola, open = "at")
  #   sink(file = InfoConsola, append = T, type = "output")
  #   sink(file = InfoConsola, append = T, type = "message")
  # }
  
  # browser()
  ## Modelo inicial-------------------------------#
  varsModelo = order[1]
  
  trainData <- fold$Training # trainData is checked inside motbf.fit()
  testData <- MoTBFs:::check_data(fold$Validation)
  rm(fold)
  if(is.null(root)){
    root = order[1]
  }
  root_type =  ifelse(is.numeric(trainData[[root]]),"Continuous","Discrete")
  
  #Aprendemos el tan para el fold i
  MI_i = MI[varsModelo,varsModelo,drop=F]
  fitted = fit_tan_g(target=target,
            data=trainData[,c(target,varsModelo)],all = F,
            mutualInfoCond = MI_i,root = root)
  
  # Prediccion modelo
  pred = predict(fitted,target,testData[,varsModelo,drop=F],method="bayes-lw")
  cv.mse<-list(fitted=fitted,
               loss = lossFunctions(nameLoss,pred,testData[,target]))
  
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
        # loginfo(paste('NEW variable:', varsNew))
      }
      # browser()
      ## Learning MI and distributions------------------------
      varsModeloNew = c(varsModelo,varsNew)
      
      # Actualizacion raiz (si es necesario)
      if(!is.numeric(trainData[,varsNew])&root_type=="Continuous"){
        root_new = varsNew
        root_type_new = "Discrete"
      }else{# No es necesario
        root_new = root
        root_new_type = root_type
      }
      # ADDITION
      ###############-
      varsModeloNew = c(varsModelo,varsNew)
      MI_i = MI[varsModeloNew, varsModeloNew]
      fitted = fit_tan_g(target=target,
                         data=trainData[,c(target,varsModeloNew)],all = F,
                         mutualInfoCond = MI_i,root = root_new)
      pred = predict(fitted,target,testData[,varsModeloNew,drop=F],method="bayes-lw")
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
        # Actualizamos la raiz
        root = root_new
        root_type = root_type_new
      }
      
      
      if(!is.null(bestSet)){
        varsModelo = bestSet
        globalImprove = T
        
        if(verbose){
          if(!is.null(outvar)){ # replacement has improved more that addition
            
            # loginfo(paste("* Updated model:", colnames(data)[varsNew], "REPLACES", outvar,"\n",
            #               " Variables included in the current model: ", paste0(c(varsModelo, target), collapse = ', '),
            #               "\n ", loss_info ,"=", as_actual))
          }else{
            
            # loginfo(paste("* Updated model, variables:", paste0(c(varsModelo, target), collapse = ', '), "\n ", loss_info ,"=", as_actual))
            
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
  return(cv.mse=cv.mse)
}

# Proceso de seleccion-------------------------------------
library(bnlearn)
library(logging)
library(pROC) # AUC
source("SeleccionGaussImputs.R")
source("fitTANGauss.R")
load("../Datasets/MIGauss.RData")
dag <- "TAN"
nameLoss <- commandArgs(trailingOnly = T)
# nameLoss <- "PREN"
time=Sys.time()
time = paste0("",time)

# Formateamos el nombre
time <- gsub(pattern = ":",replacement = "_",time)
time <- gsub(pattern = " ",replacement = "_",time)
time <- gsub(pattern = "\\.",replacement = "_",time)
time
Nombre.Red = paste(c(dag,Tipo.Dist,nameLoss,time),collapse = "+")
Nombre.Red
listaAlmacenar$Nombre.Red <- Nombre.Red
listaAlmacenar$Red <- dag
listaAlmacenar$Criterio <- nameLoss
## Salida por consola de verificacion no en el documento-----------------------
loginfo(paste("Learning", dag, Tipo.Dist,"Target", target ,"Seed:", seeds,
              "selectivo", nameLoss, collapse = " "))


library(doParallel)
cores = detectCores()
cl <- makeCluster(10)
registerDoParallel(cl)
i = 1
bnCV = foreach(i = 1:k,.packages = c("bnlearn","logging"))%dopar%{
  selective_tan_g(paste0(Nombre.Red,"_",i),target,folds[[i]],nameLoss,
                 order = names(orden[[i]]),verbose = T,MI = MIGauss[[i]])
}
stopCluster(cl)
# # Guardar la red
# save(bnCV,file = paste(directory,Nombre.Red,".RData",
#                        collapse = "",sep = ""))
l = 1
for(l in 1:k){
  # Eliminamos training y test de cada fold
  varsModelo = names(bnCV[[l]]$fitted)
  listaAlmacenar$Variables <- paste0(varsModelo,collapse = ";")
  listaAlmacenar$nVariables <- length(bnCV[[l]]$fitted)
  varCont = strsplit("CDS.,S.GC,GFF.,R_Fn,F.Ed,E_Fn,E_Lh,X100t,GFF.F,G_Lh,I_Fn,I_Lh,F.St,GFF.T,PEP.,P_Lh,SMS.,SMS..1,t.D.,ANoe,SToe,PPoe,TM.Pn",",")
  listaAlmacenar$nCont = sum(varsModelo%in%varCont[[1]])
  # Parametro que indica el fold en el .csv
  listaAlmacenar$K[l] <- l
  
  probs = sapply(1:nrow(folds[[l]]$Test),function(j){
    cpquery(bnCV[[l]]$fitted,
            event = (DUP.=="Yes"),
            evidence = as.list(folds[[l]]$Test[j,setdiff(varsModelo,target)]),
            method = "lw")
  })
  pred = factor(ifelse(probs>=0.5,"Yes","No"),levels = c("Yes","No"))
  attr(pred,"probs")<-probs
  # Actualizamos predicciones
  bnCV[[l]]$predictions = pred
  bnCV[[l]]$loss = lossFunctions(nameLoss,pred,
                                 folds[[l]]$Test[,target])
  bnCV[[l]]$observations = folds[[l]]$Test[,target]
  
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

# Escritura en el fichero
# write.table(listaAlmacenar, file = paste0(directory,Nombre.Red,
#                                           ".csv",collapse = ""),
#             append = TRUE, sep = ";", row.names = F, col.names = T,
#             fileEncoding = "utf8")
# # Guardar la red
# save(bnCV,file = paste(directory,Nombre.Red,".RData",
#                        collapse = "",sep = ""))
# rm(bnCV)

