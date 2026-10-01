# valores comunes experimentos
# Funciones de perdidad para el aprendizaje de la red bayesianan----------

lossFunctions<-function(name,predictions,observations){
  # browser()
  if(name=="BS"){
    oi= ifelse(observations=="Yes",1,0)
    probs = ifelse(is.vector(attributes(predictions)$prob),
                   attributes(predictions)$prob,
                   attributes(predictions)$prob[,"Yes"])
    return(mean((probs-oi)^2))
  }else{
    if(all(levels(predictions)%in%c("Yes","No"))){
      predictions<- factor(predictions,levels=c("Yes","No"),ordered = T)
    }
    CM = table(observations,predictions)
    if(name == "ACC"){
      return(sum(diag(CM))/sum(CM))
    }else if(name == "MG"){
      nObs = rowSums(CM)
      return((1/length(nObs))*(prod(diag(CM)/nObs)))
    }else if(name == "PRE"){
      return(CM[1,1]/sum(CM[,1]))
    }else if(name=="MCC"){
      denominador = prod(c(rowSums(CM),colSums(CM)))
      if(denominador==0){
        return(-1)
      }
      return((CM[1,1]*CM[2,2]-CM[1,2]*CM[2,1])/sqrt(denominador))
    }else if(name=="PREN"){
      return(ifelse(sum(CM[,2])==0,0,CM[2,2]/sum(CM[,2])))
    }else if(name == "RECN"){
      return(ifelse(sum(CM[2,])==0,0,CM[2,2]/sum(CM[2,])))
    }
  }
}

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
# Direccion donde se va a almacenar los datos--------------------------
directory = "../Experimentacion/"

# Parametros fijos-------------------------------------------------------
seeds <- 13458
k = 10
target <- "DUP."
loss = "pred"
metodo <- "forward"
fit.args <- list(POTENTIAL_TYPE = "MOP", scale = FALSE, maxParam = 7,
                 numIntervals = 4)

nameData <- "balancedD40"

load(paste("../DataSets/","balancedMoP40", ".RData",collapse = "",sep = ""))
load("../DataSets/ordenMB40.RData")
Tipo.Dist = "Mop"
listaAlmacenar<- list()

## Introduccion de la informacion ya conocida---------------------------------
listaAlmacenar$Dataset <- nameData
listaAlmacenar$Tipo.Dist <- Tipo.Dist
listaAlmacenar$numIntervals = fit.args$numIntervals
# listaAlmacenar$seed <- seeds

listaAlmacenar$Fold<-1:10