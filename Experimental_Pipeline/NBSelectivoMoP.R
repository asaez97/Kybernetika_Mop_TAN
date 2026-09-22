criterio_sel <-function(lossName,target_type){
  if(lossName == 'logl'){
    loss_info = 'Log-likelihood'
    # the higher, the better
    maximize = T
  }else if(target_type=="Discrete"){
    loss_info = paste0("Classification ",lossName,collapse = "")
    maximize = !(lossName=="BS")
  }else{
    loss_info = 'Root mean squared error'
    # the lower, the better
    maximize = F
  }
  return(list(maximize=maximize,loss_info=loss_info))
}

distributions <- function(graph, data, numIntervals = 4, POTENTIAL_TYPE = 'MOP', 
                          maxParam = NULL, s = NULL, priorData = NULL,scale=F){
  # browser()
  childrenAndParents <- getChildParentsFromGraph(graph,colnames(data))
  MoTBFs <- c()
  for(i in 1:length(childrenAndParents)){
    
    # loginfo(paste(" Learning distribution ",
    #               paste0(childrenAndParents[[i]][1],"|",childrenAndParents[[i]][-1],
    #                      collapse = " ")))
    
    if(length(childrenAndParents[[i]])==1){
      
      ## Single variable
      Child <- childrenAndParents[[i]]
      if(Child%in%colnames(priorData)) priorParent <- priorData[,Child]
      else priorParent <- NULL
      
      if(is.numeric(data[,Child])){
        
        ## Numeric child
        if(is.null(priorParent)) PX <- univMoTBF(data[,Child], POTENTIAL_TYPE, 
                                                 maxParam=maxParam, 
                                                 scale = FALSE)
        # else PX <- learnMoTBFpriorInformation(priorParent, data[,Child], s, POTENTIAL_TYPE, maxParam=maxParam)$posteriorFunction 
        else PX <- learnMoTBFpriorInformation(priorParent, data[,Child], s, 
                                              POTENTIAL_TYPE, maxParam=maxParam,
                                              scale = FALSE)
        UnivMoTBFs <- list(PX)
        information <- list(Child=Child, functions=UnivMoTBFs,
                            varType = "Continuous")
        MoTBFs[[length(MoTBFs)+1]] <- information
      }else{
        
        ##Discrete child
        # states <- discreteVariablesStates(Child, data)[[1]]$states
        # prob <- probDiscreteVariable(states, data[,Child])
        prob <- probDiscreteVariable(data[,Child])
        UnivDisc <- list(prob)
        information <- list(Child=Child, functions=UnivDisc, varType = "Discrete")
        MoTBFs[[length(MoTBFs)+1]] <- information
      }
    } else{
      
      ## Conditional variables
      Child <- childrenAndParents[[i]][1]
      Parents <- childrenAndParents[[i]][2:length(childrenAndParents[[i]])]
      if((Child%in%colnames(priorData))&&(all(Parents%in%colnames(priorData)))){
        priorChild <- priorData[,childrenAndParents[[i]]]
        colnames(priorChild) <- childrenAndParents[[i]]
      } else if (Child%in%colnames(priorData)) {
        priorChild <- as.data.frame(priorData[,Child])
        colnames(priorChild) <- Child
      } else{ 
        priorChild <- NULL
      }
      varType <- ifelse(is.numeric(data[,Child]), "Continuous", "Discrete")
      result <- conditionalMethod(data, Parents, Child, numIntervals, 
                                  POTENTIAL_TYPE, maxParam=maxParam, s, 
                                  priorChild, scale = FALSE)
      information <- list(Child=Child, functions=result, varType = varType)
      MoTBFs[[length(MoTBFs)+1]] <- information
    }
  }
  return(MoTBFs)
}
selective_nb <- function(Nombre.Red,target, fold, nameLoss, method = "forward",
                         order, fit.args=NULL, verbose=T){
  if(is.numeric(order)){
    order=colnames(fold$Training)[order]
  }
  # browser()
  # Grafo
  directory = "../Experimentacion/"
  # InfoConsola <- paste0(directory,Nombre.Red,"InformacionConsola",
  #                       ".txt",collapse = "")
  # InfoConsola <- file(InfoConsola, open = "at")
  # sink(file = InfoConsola, append = T, type = "output")
  # sink(file = InfoConsola, append = T, type = "message")
  
  fit.args$graph = MoTBFs:::getStructure(fold$Training,"NB",target)
  # Aprendemos las distribuciones de cada uno de los balancedFold
  
  trainData <- fold$Training # trainData is checked inside motbf.fit()
  testData <- MoTBFs:::check_data(fold$Validation)
  perfData = MoTBFs:::check_data(fold$Test)
  rm(fold)
  fit.args$data = trainData
  
  distr = do.call(distributions,fit.args)
  ## Modelo inicial---------------------
  varsModelo = order[1]
  formatedNB<- function(varsModelo,distr){
    # Seleccionamos las variables del modelo
    var_i = c(target,varsModelo)
    node_idx <- which(lapply(distr, `[[`, "Child") %in% var_i)
    
    # Calculamos el modelo
    fitted<- MoTBFs:::getFormatedBN(distr[node_idx])
    # Hacemos la prediccion
    pred = predict(fitted,target = target,data = testData[,var_i,drop = F],prob = T)
    
    cv.mse<-list(fitted=fitted,
                 loss = lossFunctions(nameLoss,pred,testData[,target]))
    
    
    return(cv.mse)
  }
  cv.mse = formatedNB(varsModelo = varsModelo,distr = distr)
  as_actual = cv.mse$loss
  # seleccion del criterio de seleccion de variables
  criterio = criterio_sel(nameLoss,cv.mse$fitted[[target]]$type)
  maximize = criterio$maximize
  loss_info = criterio$loss_info
  rm(criterio)
  # Model accuracy
  if(verbose){
    # loginfo(paste("Initial model:", paste0(c(varsModelo,target), collapse = ', '), "\n ", 
    #               loss_info ,"=", as_actual))
  }
  
  ## Wrapper approach -----------------------------------
  globalImprove = T
  while(globalImprove){
    i = 2
    globalImprove = F
    for(i in 2:length(order)) {# Introducir 1 a 1 cada variable y calcular el accuracy.
      
      bestSet = NULL
      
      newVar = order[i] 
      
      if(newVar %in% varsModelo){
        next # saltar variable que ya está en el modelo
      }
      
      if(verbose){
        # loginfo(paste('NEW variable:', newVar))
      }
      # ADDITION
      ###############-
      varsModeloNew = c(varsModelo,newVar)
      
      
      # FIT NEW Naive Bayes
      
      cv.mse_new = formatedNB(varsModeloNew,distr)
      
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
            # loginfo(paste("* Updated model:", colnames(data)[newVar], "REPLACES", outvar,"\n",
            #               " Variables included in the current model: ", paste0(c(varsModelo, target_index), collapse = ', '),
            #               "\n ", loss_info ,"=", as_actual))
          }else{
            # loginfo(paste("* Updated model, variables:", paste0(c(varsModelo, target), collapse = ', '), "\n ", loss_info ,"=", as_actual))
          }
        }
      }else{
        if(verbose){
          # loginfo(paste("- Reject variable:", newVar, "\n ", loss_info ,"=", as_nuevo))
        }
      }
      
      
    }# end for loop over ordered set of explanatory variables
    if(method %in% c('gf', 'iwsr')){
      if(verbose){
        # loginfo(paste("Improve:", globalImprove))
      }
    }else if(method == 'forward'){
      globalImprove = F
    }
    
  }
  # sink(type = "message")
  # sink()
  # close(InfoConsola, type = "at")
  
  cv.mse$loss_v = cv.mse$loss
  # Performance con el test-------------
  pred = predict(cv.mse$fitted,target = target,
                 data = perfData[,c(target,varsModelo)],prob = T)
  cv.mse$observations = perfData[,target]
  cv.mse$loss = lossFunctions(nameLoss,pred,
                              perfData[,target])
  cv.mse$predictions=pred
  return(cv.mse)
}

library(logging)
library(pROC) # AUC
source("SeleccionMoPImputs.R")
# Naive Bayes Selectivo MOP

dag <- "NB"
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
listaAlmacenar$Nombre.Red <- Nombre.Red
listaAlmacenar$Red <- dag
listaAlmacenar$Criterio <- nameLoss
## Salida por consola de verificacion no en el documento-----------------------
# loginfo(paste("Learning", dag, Tipo.Dist,"Target", target ,"Seed:", seeds,
#               "selectivo", nameLoss, collapse = " "))


library(doParallel)
library(foreach)
cores = detectCores()
cl <- makeCluster(1)
registerDoParallel(cl)
i = 1
bnCV = foreach(i = 1:10,.packages = c("bnlearn", "MoTBFs","logging"))%dopar%{
  selective_nb(paste0(Nombre.Red,"_",i),target,folds[[i]],nameLoss,
               order = names(orden[[i]]),
               fit.args=fit.args,verbose = T)
}
stopCluster(cl)
# # Guardar la red
# save(bnCV,file = paste(directory,Nombre.Red,".RData",
#                        collapse = "",sep = ""))

## Almacenamiento de la informacion -------------------------------------------
# Bucle que pasa por cada fold
l = 1
for(l in 1:k){
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
