# valores comunes experimentos

# Direccion donde se va a almacenar los datos--------------------------
directory = "../Experimentacion/"

# Parametros fijos-------------------------------------------------------
seeds <- 13458
k = 10
target <- "DUP."
loss = "pred"
metodo <- "forward"

nameData <- "datos"

load(paste("../DataSets/","balancedMoP40", ".RData",collapse = "",sep = ""))
load("../DataSets/ordenCGB40.RData")
Tipo.Dist = "Gauss"
listaAlmacenar<- list()
source("lossFunctions.R")

## Introduccion de la informacion ya conocida---------------------------------
listaAlmacenar$Dataset <- nameData
listaAlmacenar$Tipo.Dist <- Tipo.Dist
# listaAlmacenar$seed <- seeds
listaAlmacenar$Fold<-1:10