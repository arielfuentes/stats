par <- read.csv(file = "Indicador3.csv",sep=",",header = T)
levels(par$ICT)[2] <- NA
par$ICT  <- as.numeric(par$ICT)
levels(par$ICF)[2] <- NA
par$ICF <- as.numeric(par$ICF)
par <- par[!par$Servicio == "B07C",]
parTN <- par[!(par$Mes == 1 | par$Mes == 2),]
parTN <- parTN[(par$Anho == 2014 | par$Anho == 2015 ),]
pairs(~TRX+Prom.ICR+ Capacidad.de.Transporte+KM+PLAZASxKM+Plaza.Teorica+ICT+Plaza.Ofrecida+ICF, 
      data = parTN, main = "Matriz de dispersión")
pdf("rplotx.pdf", width = 2200, height = 2000)
dev.off()
