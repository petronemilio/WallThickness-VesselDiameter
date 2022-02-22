library(lm.br)
library(plotrix)
#cell -> cell.temp.subseted

cell <- cell.temp

cell <- subset(cell, cell$vwt>0.1)
cell <- subset(cell, cell$vd>0.1)

#####       
# regresión partida para: Todos juntos

  lm.vwt.vd.br <- lm.br(log10(cell$vwt)~log10(cell$vd))
lm.vwt.vd.br

#Cambio a 90.36287um (CI:81.23627 a 101.6553 um)
  10^1.95599
  10^1.90975
  10^2.00713
  
### solo self  1.94522 [ 1.8285, 2.01756 ]
  10^1.94522
  
  ### solo non-self 1.92489  [ 1.79475, 2.04225 ]
10^1.92489
  

#####
##modelos arriba y abajo del umbral
  
###abajo del umbral
cell <- subset(cell, cell$vd < 90)
#range(cell$vwt)
lm.vwt.vd.1 <- lm(log10(cell$vwt)~log10(cell$vd))
plot(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd.1)
confint(lm.vwt.vd.1)
abline(lm.vwt.vd.1)
abline(a=log10(mean(cell$vwt)), b=0)

###arriba del umbral
cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 10^1.95599)
#range(cell$vwt)
#cell <- subset(cell, cell$self..non=="self")
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
plot(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)
abline(lm.vwt.vd)

######
####Fig2a
cell <- cell.temp.subseted
plot(log10(cell$vwt)~log10(cell$vd), cex.lab=1.7,  xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")))
points(log10(cell$vwt[cell$vd<10^1.95599])~log10(cell$vd[cell$vd<10^1.95599]), col="black")
points(log10(cell$vwt[cell$vd>10^1.95599])~log10(cell$vd[cell$vd>10^1.95599]), col="black", pch=19)
ablineclip(lm.vwt.vd, col="black", x1=10^1.95599, x2=3, y1=0.3, y2=1.2, lwd=2)
ablineclip(lm.vwt.vd.1, col="black", x1=0.3, x2=10^1.95599, y1=0.1, y2=2, lty=2, lwd=2)


cell <- cell.temp.subseted
plot((cell$vwt)~(cell$vd), log="xy", cex.lab=2.3, cex.axis=2,  xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")))
points((cell$vwt[cell$vd<10^1.95599])~(cell$vd[cell$vd<10^1.95599]), col="black")
points((cell$vwt[cell$vd>10^1.95599])~(cell$vd[cell$vd>10^1.95599]), col="black", pch=19)
ablineclip(lm.vwt.vd, col="black", x1=10^1.95599, x2=10^3, y1=10^0.3, y2=10^1.2, lwd=2)
ablineclip(lm.vwt.vd.1, col="black", x1=10^0.3, x2=10^1.95599, y1=10^0.1, y2=10^2, lty=2, lwd=2)



### Fig2b

###abajo del umbral
cell <- subset(cell, cell$dwt>0.1)
cell <- subset(cell, cell$vd < 10^1.95599)
#range(cell$vwt)
lm.dwt.vd.1 <- lm(log10(cell$dwt)~log10(cell$vd))
plot(log10(cell$dwt)~log10(cell$vd))
summary(lm.dwt.vd.1)
confint(lm.dwt.vd.1)
abline(lm.dwt.vd.1)
abline(a=log10(mean(cell$dwt)), b=0)

###arriba del umbral
cell <- cell.temp.subseted
cell <- subset(cell, cell$dwt>0.1)
cell <- subset(cell, cell$vd > 10^1.95599)
#range(cell$vwt)
#cell <- subset(cell, cell$self..non=="self")
lm.dwt.vd <- lm(log10(cell$dwt)~log10(cell$vd))
plot(log10(cell$dwt)~log10(cell$vd))
summary(lm.dwt.vd)
confint(lm.dwt.vd)
abline(lm.dwt.vd)

cell <- cell.temp.subseted
plot((cell$dwt)~(cell$vd), log="xy", cex.lab=2.3, cex.axis=2,  xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Double wall thickness ( ", mu, "m )")))
points((cell$dwt[cell$vd<10^1.95599])~(cell$vd[cell$vd<10^1.95599]), col="black")
points((cell$dwt[cell$vd>10^1.95599])~(cell$vd[cell$vd>10^1.95599]), col="black", pch=19)
ablineclip(lm.dwt.vd, col="black", x1=10^1.95599, x2=10^3, y1=10^0.3, y2=10^1.2, lwd=2)
ablineclip(lm.dwt.vd.1, col="black", x1=10^0.3, x2=10^1.95599, y1=10^0.1, y2=10^2, lty=2, lwd=2)




#####
# regresión partida para: Solo self
cell <- subset (cell, cell$self..non == "self")
lm.vwt.vd.br <- lm.br(log10(cell$vwt)~log10(cell$vd))
lm.vwt.vd.br

##no hay cambio... pblueice un cambio en los vasos 
#más chicos (menores a 17.9999) pero sólo por unos cuantos puntos
10^1.25527
cell <- subset(cell, cell$vd < 10^1.25527)
plot(log10(cell$vwt)~log10(cell$vd))
cell <- subset(cell, cell$vd > 10^1.25527)
cell <- cell.temp.subseted
lm.vwt.vd.br <- lm.br(log10(cell$vwt)~log10(cell$vd))
lm.vwt.vd.br

cell <- cell.temp.subseted

#####
#regresión partida para: non-self
cell <- cell.temp.subseted
cell <- subset (cell, cell$self..non == "non")
lm.vwt.vd.br <- lm.br(log10(cell$vwt)~log10(cell$vd))
lm.vwt.vd.br
#1.85308 [ 1.71625, 1.96263 ]
10^1.85308
10^1.71625
10^1.96263
## En las de no-auto-sostén pblueice un cambio a los
## 71.29844um (C.I. 52.02954 a 91.75506)
cell <- subset(cell, cell$vd < 10^1.85308)
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
plot(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)
abline(lm.vwt.vd)

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 10^1.85308)
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
plot(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)
abline(lm.vwt.vd)



#####
#otros


cell <- subset(cell, cell$vd>150)
#Initial plot
plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), col="green")
points(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]),col="blue")

#Quitar a las non self
cell <- subset (cell, cell$self..non == "non")

#
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 106.7997)
#modelo los dos juntos
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
plot(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)
abline(lm.vwt.vd)

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 10^1.85)
#modelo los dos juntos
plot(log10(cell$vwt)~log10(cell$vd))
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)
abline(lm.vwt.vd)


lm.vwt.vd.br <- lm.br(log10(cell$vwt)~log10(cell$vd))



##########################
####Modelos de la tabla arriba del umbral
###ite

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
cell <- subset(cell, cell$vd > 10^1.95599)

lm.vwt.vd.conductive <- lm(log10(cell$vwt)~log10(cell$vd)+cell$conductive)

summary(lm.vwt.vd.conductive)
confint(lm.vwt.vd.conductive)

plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]), pch=19, col="blue")
points(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]), pch=19, col="black")
abline(lm.vwt.vd.conductive)

lm.vwt.vd.conductive.T <- lm(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]))
summary(lm.vwt.vd.conductive.T)                      
abline(lm.vwt.vd.conductive.T, col="blue")                      

lm.vwt.vd.conductive.F <- lm(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]))
summary(lm.vwt.vd.conductive.F)                      
abline(lm.vwt.vd.conductive.F)                      

##########################
###Placa de perforación

lm.vwt.vd.simp.scal <- lm(log10(cell$vwt)~log10(cell$vd)+cell$simp.scal)

summary(lm.vwt.vd.simp.scal)
confint(lm.vwt.vd.simp.scal)

plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]), pch=19, col="blue")
points(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]), pch=19, col="black")


lm.vwt.vd.simple <- lm(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]))
summary(lm.vwt.vd.simple)      
confint(lm.vwt.vd.simple)  
abline(lm.vwt.vd.simple, col="blue")                      

lm.vwt.vd.scalariform <- lm(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]))
summary(lm.vwt.vd.scalariform)                      
abline(lm.vwt.vd.scalariform) 
confint(lm.vwt.vd.scalariform) 


##########################
###Selfies

lm.vwt.vd.self..non <- lm(log10(cell$vwt)~log10(cell$vd)+cell$self..non*log10(cell$vd))

summary(lm.vwt.vd.self..non)

plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), pch=19, col="blue")
points(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]), pch=19, col="black")


lm.vwt.vd.self..non.T <- lm(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]))
summary(lm.vwt.vd.self..non.T)                      
abline(lm.vwt.vd.self..non.T, col="blue")                      

lm.vwt.vd.self..non.F <- lm(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]))
summary(lm.vwt.vd.self..non.F)                      
abline(lm.vwt.vd.self..non.F) 





plot(log10(cell$ITEWTFINAL..wall.thick.final.micrometers)~log10(cell$vd))

