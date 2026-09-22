# valores comunes experimentos

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

nameData <- "datos"

load(paste("../DataSets/","foldC", ".RData",collapse = "",sep = ""))
load("../DataSets/ordenM.RData")
Tipo.Dist = "Mop"
listaAlmacenar<- list()
source("lossFunctions.R")

## Introduccion de la informacion ya conocida---------------------------------
listaAlmacenar$Dataset <- nameData
listaAlmacenar$Tipo.Dist <- Tipo.Dist
listaAlmacenar$numIntervals = fit.args$numIntervals
listaAlmacenar$seed <- seeds

listaAlmacenar$Fold<-1:10