EOD <- read.csv(file = "zona EOD chillan.csv", header = TRUE, sep = ",", 
                colClasses = "character")

Lista <- unique(EOD$X.08.Propósito)

Distrib_prob <- function(grupo = "viajes"){
  ##Leer datos
  EOD <- read.csv(file = "zona EOD chillan.csv", header = TRUE, sep = ",")
  Dist <- read.csv(file = "mtx.csv", header = TRUE, sep = ",")
  ##Limpiar Datos
  Dist$Dis2 <- Dist$Distance*100000
  EOD <- EOD[-which(EOD$X.10.Zona.Origen == "Sin Información"),]
  EOD <- EOD[-which(EOD$X.12.Zona.Destino == "Sin Información"),]
  droplevels(EOD$X.12.Zona.Destino)
  droplevels(EOD$X.10.Zona.Origen)
  
  ##totalización por grupo 
  library(dplyr)
  if (grupo == "viajes"){
  EOD <- EOD %>% group_by(X.08.Propósito, X.10.Zona.Origen, X.12.Zona.Destino) %>%
    summarise(Viajes = sum(Viajes))}
  else if (grupo %in% Lista){
    EOD <- EOD[EOD$X.08.Propósito == grupo, ] %>% group_by(X.08.Propósito, 
              X.10.Zona.Origen, X.12.Zona.Destino) %>%
                 summarise(Viajes = sum(Viajes))
  }
  else {
    stop ("introduzca el grupo correcto")
  }
##Crear campo de unión
Dist$ID <- paste0(Dist$InputID, Dist$TargetID)
EOD$ID <- paste0(EOD$X.10.Zona.Origen, EOD$X.12.Zona.Destino)
##Unir tablas
VD <- inner_join(Dist, EOD, by = "ID")
##Obtener Viajes
Viajes <- as.numeric(VD$Viajes)
##Obtener Distribución
library(fitdistrplus)
fit <- fitdist(Viajes, "lnorm")
summary(fit)
plot(fit, demp = TRUE)
}
