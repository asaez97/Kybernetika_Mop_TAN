
selective_nb_g = function(Nombre.Red,target, fold, nameLoss, method = "forward",
                          order, verbose=T){
  library(bnlearn)
  if(is.numeric(order)){
    order=colnames(fold$Training)[order]
  }
  # directory = "../Experimentacion/"
  # InfoConsola <- paste0(directory,Nombre.Red,"InformacionConsola",
  #                       ".txt",collapse = "")
  if(verbose){
    # InfoConsola <- file(InfoConsola, open = "at")
    # sink(file = InfoConsola, append = T, type = "output")
    # sink(file = InfoConsola, append = T, type = "message")
  }
  # browser()
  ## Modelo inicial-------------------------------#
  varsModelo = order[1]
  trainData <- fold$Training # trainData is checked inside motbf.fit()
  testData <- MoTBFs:::check_data(fold$Validation)
  rm(fold)
  cv.mse = list()
  
  #Aprendemos el NB 
  dag = empty.graph(c(target,varsModelo))
  adj_mat = matrix(0,nrow = length(varsModelo),ncol = length(varsModelo))
  adj_mat = rbind(adj_mat,1)
  adj_mat = cbind(adj_mat,0)
  dimnames(adj_mat) = list(c(varsModelo,target),c(varsModelo,target))
  amat(dag) = adj_mat
  fitted = bn.fit(dag,trainData[,c(target,varsModelo)])
  
  # Prediccion modelo
  pred = predict(fitted,target,testData[,varsModelo,drop=F],method="bayes-lw")
  cv.mse<-list(fitted=fitted,
               loss = lossFunctions(nameLoss,pred,testData[,target]))
  
  # seleccion del criterio de seleccion de variables
  criterio = criterio_sel( nameLoss,"Discrete")
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
        loginfo(paste('NEW variable:', varsNew))
      }
      ## Learning MI and distributions------------------------
      varsModeloNew = c(varsModelo,varsNew)
      # ADDITION
      ###############-
      varsModeloNew = c(varsModelo,varsNew)
      cv.mse_new = list()
      dag = empty.graph(c(target,varsModeloNew))
      adj_mat = matrix(0,nrow = length(varsModeloNew),ncol = length(varsModeloNew))
      adj_mat = rbind(adj_mat,1)
      adj_mat = cbind(adj_mat,0)
      dimnames(adj_mat) = list(c(varsModeloNew,target),
                               c(varsModeloNew,target))
      amat(dag) = adj_mat
      #Aprendemos el tan para el fold i
      fitted = bn.fit(dag,trainData[,c(target,varsModeloNew)])
      
      # Prediccion modelo
      pred = predict(fitted,target,testData[,varsModeloNew,drop=F],
                     method="bayes-lw")
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
  return(cv.mse)
}
# Proceso de seleccion-------------------------------------
library(logging)
library(pROC) # AUC
source("SeleccionGaussImputsBalanced.R")
library(bnlearn)

dag <- "NB"
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
i = 1
bnCV = foreach(i = 1:10,.packages = c("bnlearn","logging"))%dopar%{
  selective_nb_g(paste0(Nombre.Red,"_",i),target,balancedFold[[i]],nameLoss,
                 order = names(orden[[i]]),verbose = T)
}
stopCluster(cl)
# Guardar la red
# save(bnCV,file = paste(directory,Nombre.Red,".RData",
#                        collapse = "",sep = ""))
l = 1
for(l in 1:k){
  # Eliminamos training y test de cada fold
  varsModelo = names(bnCV[[l]]$fitted)
  listaAlmacenar$Variables[l] <- paste0(varsModelo,collapse = ";")
  listaAlmacenar$nVariables[l] <- length(bnCV[[l]]$fitted)
  varCont = strsplit("CDS.,S.GC,GFF.,R_Fn,F.Ed,E_Fn,E_Lh,X100t,GFF.F,G_Lh,I_Fn,I_Lh,F.St,GFF.T,PEP.,P_Lh,SMS.,SMS..1,t.D.,ANoe,SToe,PPoe,TM.Pn",",")
  listaAlmacenar$nCont[l] = sum(names(bnCV[[l]]$fitted)%in%varCont[[1]])
  # Parametro que indica el fold en el .csv
  listaAlmacenar$K[l] <- l
  
  probs = sapply(1:nrow(balancedFold[[l]]$Test),function(j){
    cpquery(bnCV[[l]]$fitted,
            event = (DUP.=="Yes"),
            evidence = as.list(balancedFold[[l]]$Test[j,setdiff(varsModelo,target)]),
            method = "lw")
  })
  pred = factor(ifelse(probs>=0.5,"Yes","No"),levels = c("Yes","No"))
  attr(pred,"probs")<-probs
  # Actualizamos predicciones
  bnCV[[l]]$predictions = pred
  bnCV[[l]]$loss = lossFunctions(nameLoss,pred,
                                 balancedFold[[l]]$Test[,target])
  bnCV[[l]]$observations = balancedFold[[l]]$Test[,target]
  
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
