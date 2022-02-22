##########################
####Models above the threshold in table 1 
####and graphs in Fig S1

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
cell <- subset(cell, cell$vd > 10^1.95599)


length(cell$vd)
length(cell$vwt)

####General Model
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)

plot(log10(cell$vwt)~log10(cell$vd))
abline(lm.vwt.vd)


##########################
###Perforation plate type

lm.vwt.vd.simp.scal <- lm(log10(cell$vwt)~log10(cell$vd)+cell$simp.scal)

summary(lm.vwt.vd.simp.scal)
confint(lm.vwt.vd.simp.scal)

plot(log10(cell$vwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Double wall thickness ( ", mu, "m )")))
points(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]), pch=1, col="black", cex=1.7)
points(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]), pch=19, col="black", cex=1.7)
abline(a=-1.08, b=0.72748)
abline(a=-1.08+0.10953, b=0.72748, lty=2)


lm.vwt.vd.simple <- lm(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]))
summary(lm.vwt.vd.simple)      
confint(lm.vwt.vd.simple)  
abline(lm.vwt.vd.simple, col="blue")                      

lm.vwt.vd.scalariform <- lm(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]))
summary(lm.vwt.vd.scalariform)                      
abline(lm.vwt.vd.scalariform) 
confint(lm.vwt.vd.scalariform) 

boxplot(log10(cell$ite.wt)~cell$simp.scal, notch=T)
t.test(log10(cell$ite.wt)~cell$simp.scal)


####ITE Type

lm.vwt.vd.ite.type <- lm(log10(cell$vwt)~log10(cell$vd)*cell$ite.type)

summary(lm.vwt.vd.ite.type)
confint(lm.vwt.vd.ite.type)

plot(log10(cell$vwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Double wall thickness ( ", mu, "m )")))
points(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]), pch=19, col="black", cex=1.7)
points(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]), pch=1, col="black", cex=1.7)
abline(lm.vwt.vd.conductive)

lm.vwt.vd.conductive.T <- lm(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]))
summary(lm.vwt.vd.conductive.T)                      
abline(lm.vwt.vd.conductive.T, col="blue")                      

lm.vwt.vd.conductive.F <- lm(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]))
summary(lm.vwt.vd.conductive.F)                      
abline(lm.vwt.vd.conductive.F)                      




####ITE Conductiveness

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
###Selfies

lm.vwt.vd.self..non <- lm(log10(cell$vwt)~log10(cell$vd)+cell$self..non)

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



##########################
####Models above the threshold in table 1 
####and graphs in FigS1

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$dwt > 0.1)
cell <- subset(cell, cell$vd > 10^1.95599)


length(cell$vd)
length(cell$vwt)

####General Model
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)

plot(log10(cell$vwt)~log10(cell$vd))
abline(lm.vwt.vd)


##########################
###Perforation plate type

lm.vwt.vd.simp.scal <- lm(log10(cell$vwt)~log10(cell$vd)*+cell$simp.scal)

summary(lm.vwt.vd.simp.scal)
confint(lm.vwt.vd.simp.scal)

plot((cell$vwt)~(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")), cex.lab=1.9, cex.axis=2, log="xy")
plot(log10(cell$vwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")), cex.lab=1.9, cex.axis=2)
points(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]), pch=1, col="black", cex=1.7)
points(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]), pch=19, col="black", cex=1.7)
abline(a=-1.08, b=0.72748)
abline(a=-1.08+0.10953, b=0.72748, lty=2)
ablineclip(a=-1.08, b=0.72748, y1=log10(1.6), y2=log10(6), x1=log10(91), x2=log10(181))

lm.vwt.vd.simple <- lm(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]))
summary(lm.vwt.vd.simple)      
confint(lm.vwt.vd.simple)  
abline(lm.vwt.vd.simple, col="blue")                      

lm.vwt.vd.scalariform <- lm(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]))
summary(lm.vwt.vd.scalariform)                      
abline(lm.vwt.vd.scalariform) 
confint(lm.vwt.vd.scalariform) 

boxplot(log10(cell$ite.wt)~cell$simp.scal, notch=T)
t.test(log10(cell$ite.wt)~cell$simp.scal)


####ITE Type



lm.vwt.vd.ite.type <- lm(log10(cell$vwt)~log10(cell$vd)*cell$ite.type)

summary(lm.vwt.vd.ite.type)
confint(lm.vwt.vd.ite.type)

plot(log10(cell$vwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Double wall thickness ( ", mu, "m )")))
points(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]), pch=19, col="black", cex=1.7)
points(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]), pch=1, col="black", cex=1.7)
abline(lm.vwt.vd.conductive)

lm.vwt.vd.conductive.T <- lm(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]))
summary(lm.vwt.vd.conductive.T)                      
abline(lm.vwt.vd.conductive.T, col="blue")                      

lm.vwt.vd.conductive.F <- lm(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]))
summary(lm.vwt.vd.conductive.F)                      
abline(lm.vwt.vd.conductive.F)                      




####ITE Conductiveness

lm.vwt.vd.conductive <- lm(log10(cell$vwt)~log10(cell$vd)*cell$conductive)

summary(lm.vwt.vd.conductive)
confint(lm.vwt.vd.conductive)

plot((cell$vwt)~(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")), cex.lab=1.9, cex.axis=2, log="xy")
plot(log10(cell$vwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")), cex.lab=1.9, cex.axis=2)
points(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]), pch=19, col="black", cex=1.7)
points(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]), pch=1, col="black", cex=1.7)
abline(lm.vwt.vd.conductive)

lm.vwt.vd.conductive.T <- lm(log10(cell$vwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]))
summary(lm.vwt.vd.conductive.T)                      
abline(lm.vwt.vd.conductive.T, col="blue")                      

lm.vwt.vd.conductive.F <- lm(log10(cell$vwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]))
summary(lm.vwt.vd.conductive.F)                      
abline(lm.vwt.vd.conductive.F)                      

##########################
###Selfies

lm.vwt.vd.self..non <- lm(log10(cell$vwt)~log10(cell$vd)+cell$self..non)

summary(lm.vwt.vd.self..non)

plot((cell$vwt)~(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")), cex.lab=1.9, cex.axis=2, log="xy")
plot(log10(cell$vwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")), cex.lab=1.9, cex.axis=2) 
points(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), pch=1, col="black", cex=1.7)
points(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]), pch=19, col="black", cex=1.7)
ablineclip(a=-0.74470-0.09400, b=0.64088, y1=log10(1.3), y2= log10(6.9), x1=log10(91), x2=log10(218), lty=2)
ablineclip(a=-0.74470, b=0.64088, y1=log10(1.3), y2= log10(11.3), x1=log10(96), x2=log10(461))


lm.vwt.vd.self..non.T <- lm(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]))
summary(lm.vwt.vd.self..non.T)                      
abline(lm.vwt.vd.self..non.T, col="blue")                      

lm.vwt.vd.self..non.F <- lm(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]))
summary(lm.vwt.vd.self..non.F)                      
abline(lm.vwt.vd.self..non.F) 





plot(log10(cell$ITEWTFINAL..wall.thick.final.micrometers)~log10(cell$vd))

