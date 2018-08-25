# library(ggplot2)
########################## Directorios ###############################
# carpeta_general<-"~/Documents/DataBase/Gravity/03 Proyecto 3"
# carpeta_resultados<-paste(carpeta_general,"/06 Resultados/Final",sep="")

# setwd(carpeta_resultados)
yrs <- 1992:2015
# yrs <- yrs[1:12]
load("data/CentralitySims.RData")


colnames(replicas.sum)[1] <- "value"
colnames(replicas.sd)[1] <- "value"
colnames(base)[1] <- "value"

replicas$indicator[which(replicas$indicator=="ev.sol" )] = "Eigen"
replicas$indicator[which(replicas$indicator=="SOL.i.r")] = "Elasticidad i"
replicas$indicator[which(replicas$indicator=="SOL.j.r")] = "Elasticidad j"

replicas.sd$indicator[which(replicas.sd$indicator=="ev.sol" )] = "Eigen"
replicas.sd$indicator[which(replicas.sd$indicator=="SOL.i.r")] = "Elasticidad i"
replicas.sd$indicator[which(replicas.sd$indicator=="SOL.j.r")] = "Elasticidad j"

base$indicator[which(base$indicator=="ev.sol" )] = "Eigen"
base$indicator[which(base$indicator=="SOL.i.r")] = "Elasticidad i"
base$indicator[which(base$indicator=="SOL.j.r")] = "Elasticidad j"


indicador <- unique(replicas$indicator)#ii
anio <- unique(replicas$year)#jj
pais <- unique(replicas$country)[order(unique(replicas$country))]#kk

paises <- read.csv("data/CC.csv",sep = ",", header = TRUE)

nom.pais <- as.character(paises[match(pais,as.character(paises$ISO3)),"Nombre"])
names(pais) <- nom.pais