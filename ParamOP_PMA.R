#######Directory
setwd("~/ariel")
######Cleaning
par <- read.csv(file = "Indicador4.csv",sep=";",header = T)
levels(par$ICT)[2] <- NA
par$ICT  <- as.numeric(par$ICT)
levels(par$ICF)[2] <- NA
par$ICF <- as.numeric(par$ICF)
par <- par[!(par$Servicio == "B07C" | par$Servicio == "B09C" | par$Servicio == "B90" | par$Servicio == "B91" | par$Servicio == "SINSER" | par$Servicio == "" | par$Servicio == "B92" | par$Servicio == "B94"),]
par$Servicio <- droplevels(par$Servicio)
##############PMA
parPMA <- par[!(par$Servicio == "B51C" | par$Servicio == "B60E" | par$Servicio == "B61C" | par$Servicio == "B71"), ]
#rm(par)
parTNPMA <- parPMA[!(parPMA$Mes == 1 | parPMA$Mes == 2),]
parTNPMA <- parTNPMA[(parTNPMA$Anho == 2014 | parTNPMA$Anho == 2015 ),]
parTNPMA$Servicio <- droplevels(parTNPMA$Servicio)
#########Images
a <- split(x = parTNPMA, f = parTNPMA$Servicio)
for (i in names(a)) {
  jpeg(filename = paste(i, "TN", "PMA", sep="", ".jpg"), width = 2200, 
       height = 2000)
  pairs(~TRX+Prom.ICR+ Capacidad.de.Transporte+KM+PLAZASxKM+Plaza.Teorica+ICT+
          Plaza.Ofrecida+ICF, data = a[[i]], main = paste(i, "PMA"))
  dev.off()
}
