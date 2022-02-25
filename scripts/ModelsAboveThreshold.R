##########################
####Models for vessel wall and Vessel wall thickness and vessel diameter
#above the 90 um threshold (reported in table 1) 
####and graphs in Fig S1


#Models for vessel wall thickness (lines 8 to 95)
#####

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
cell <- subset(cell, cell$vd > 90)


length(cell$vd)
length(cell$vwt)

#General Model
#####
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)

plot(log10(cell$vwt)~log10(cell$vd))
abline(lm.vwt.vd)



###VWT~VD among plants with different perforation plate types
##########################

lm.vwt.vd.simp.scal <- lm(log10(cell$vwt)~log10(cell$vd)+cell$simp.scal)

summary(lm.vwt.vd.simp.scal)
confint(lm.vwt.vd.simp.scal)

plot(log10(cell$vwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")))
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



#VWT~VD among plants with different ITE conductive status
#####

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


###VWT~VD among Self- and non-self supporting plants
##########################

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



#Models for Double wall thickness (lines 8 to 95)
#####

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
cell <- subset(cell, cell$vd > 90)


length(cell$vd)
length(cell$vwt)

#General Model (vwt~VD)
#####
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)

plot(log10(cell$vwt)~log10(cell$vd))
abline(lm.vwt.vd)



###vwt~VD among plants with different perforation plate types
##########################

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



#vwt~VD among plants with different ITE conductive status
#####

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


###vwt~vd among self- and non-self supporting plants
##########################

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



#####
##Models for Double wall thickness and vessel diameter


#Models for Double wall thickness (lines 205to 298)
#####

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$dwt > 0.1)
cell <- subset(cell, cell$vd > 90)


length(cell$vd)
length(cell$dwt)

#General Model (DWT~VD)
#####
lm.dwt.vd <- lm(log10(cell$dwt)~log10(cell$vd))
summary(lm.dwt.vd)
confint(lm.dwt.vd)

plot(log10(cell$dwt)~log10(cell$vd))
abline(lm.dwt.vd)



###dwt~VD among plants with different perforation plate types
##########################

lm.dwt.vd.simp.scal <- lm(log10(cell$dwt)~log10(cell$vd)+cell$simp.scal)

summary(lm.dwt.vd.simp.scal)
confint(lm.dwt.vd.simp.scal)

plot(log10(cell$dwt)~log10(cell$vd), cex=1.7, xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Double wall thickness ( ", mu, "m )")))
points(log10(cell$dwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]), pch=1, col="black", cex=1.7)
points(log10(cell$dwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]), pch=19, col="black", cex=1.7)
abline(a=-1.08, b=0.72748)
abline(a=-1.08+0.10953, b=0.72748, lty=2)


lm.dwt.vd.simple <- lm(log10(cell$dwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]))
summary(lm.dwt.vd.simple)      
confint(lm.dwt.vd.simple)  
abline(lm.dwt.vd.simple, col="blue")                      

lm.dwt.vd.scalariform <- lm(log10(cell$dwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]))
summary(lm.dwt.vd.scalariform)                      
abline(lm.dwt.vd.scalariform) 
confint(lm.dwt.vd.scalariform) 

boxplot(log10(cell$ite.wt)~cell$simp.scal, notch=T)
t.test(log10(cell$ite.wt)~cell$simp.scal)



#dwt~VD among plants with different ITE conductive status
#####

lm.dwt.vd.conductive <- lm(log10(cell$dwt)~log10(cell$vd)+cell$conductive)

summary(lm.dwt.vd.conductive)
confint(lm.dwt.vd.conductive)

plot(log10(cell$dwt)~log10(cell$vd))
points(log10(cell$dwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]), pch=19, col="blue")
points(log10(cell$dwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]), pch=19, col="black")
abline(lm.dwt.vd.conductive)

lm.dwt.vd.conductive.T <- lm(log10(cell$dwt[cell$conductive=="Conductive"])~log10(cell$vd[cell$conductive=="Conductive"]))
summary(lm.dwt.vd.conductive.T)                      
abline(lm.dwt.vd.conductive.T, col="blue")                      

lm.dwt.vd.conductive.F <- lm(log10(cell$dwt[cell$conductive=="Non-conductive"])~log10(cell$vd[cell$conductive=="Non-conductive"]))
summary(lm.dwt.vd.conductive.F)                      
abline(lm.dwt.vd.conductive.F)                      


###dwt~VD among Self- and non-self supporting plants
##########################

lm.dwt.vd.self..non <- lm(log10(cell$dwt)~log10(cell$vd)+cell$self..non)

summary(lm.dwt.vd.self..non)

plot(log10(cell$dwt)~log10(cell$vd))
points(log10(cell$dwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), pch=19, col="blue")
points(log10(cell$dwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]), pch=19, col="black")


lm.dwt.vd.self..non.T <- lm(log10(cell$dwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]))
summary(lm.dwt.vd.self..non.T)                      
abline(lm.dwt.vd.self..non.T, col="blue")                      

lm.dwt.vd.self..non.F <- lm(log10(cell$dwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]))
summary(lm.dwt.vd.self..non.F)                      
abline(lm.dwt.vd.self..non.F) 

