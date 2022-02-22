#########################################################
#####Grosor de pared de vaso (dwt) + pared ITE###########
#########################################################
#Regresar a base original
cell <- cell.temp.subseted

#Initial plot
plot(log10(cell$dwt)~log10(cell$vd))
points(log10(cell$dwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), col="green")
points(log10(cell$dwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]),col="blue")

#
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$dwt > 0.1)

#modelo los dos juntos
lm.dwt.vd <- lm(log10(cell$dwt)~log10(cell$vd))
summary(lm.dwt.vd)
confint(lm.dwt.vd)

png("Figures/VD_DWT.png")
plot(log10(cell$dwt)~log10(cell$vd), pch=19, col="black",cex=0.8,
     xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Double wall thickness ", mu, "m")))
abline(lm.dwt.vd, col="red",lwd=2)
dev.off()

pdf("Figures/VD_DWT.pdf")
plot(log10(cell$dwt)~log10(cell$vd), pch=19, col="black",cex=0.8,
     xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Double wall thickness ", mu, "m")))
abline(lm.dwt.vd, col="red",lwd=2)
dev.off()


###Entre tipos de placa de perforación Y AGREGANDO EL UMBRAL#############
#####################################
boxplot(cell$dwt~cell$FINAL.simple.scalariform, notch=F, ylab="Vessel wall thickness (um)")
#Subset solo simple y escalariformes
cell <- subset(cell, cell$simp.scal=="simple"|cell$simp.scal=="scalariform")
cell <- subset(cell, cell$vd < 92.42)
#
boxplot(cell$dwt~cell$simp.scal, notch=F, ylab="Double wall thickness (um)")

#
plot(log10(cell$dwt)~log10(cell$vd))
points(log10(cell$dwt[cell$FINAL.simple.scalariform=="simple"])~log10(cell$vd[cell$FINAL.simple.scalariform=="simple"]), col="green")
points(log10(cell$dwt[cell$FINAL.simple.scalariform=="scalariform"])~log10(cell$vd[cell$FINAL.simple.scalariform=="scalariform"]),col="blue")
#modelo los dos juntos
lm.dwt.vd.bellow <- lm(log10(cell$dwt)~log10(cell$vd))
summary(lm.dwt.vd.bellow)
confint(lm.dwt.vd.bellow)
#modelo solo simple
lm.dwt.vd.simple.bellow <- lm(log10(cell$dwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]))
summary(lm.dwt.vd.simple.bellow)
confint(lm.dwt.vd.simple.bellow)
#modelo solo scalariforme
lm.dwt.vd.scalariform.bellow <- lm(log10(cell$dwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal =="scalariform"]))
summary(lm.dwt.vd.scalariform.bellow)
confint(lm.dwt.vd.scalariform.bellow)
#modelo para probar tipo de placa más diámetro de vaso
lm.dwt.vd.perfp.bellow <- lm(log10(cell$dwt) ~ log10(cell$vd) + cell$simp.scal)
summary(lm.dwt.vd.perfp.bellow)
confint(lm.dwt.vd.perfp.bellow)

#modelo para probar diferencias en pendientes (interacción)
lm.dwt.vd.int.bellow <- lm(log10(cell$dwt) ~ log10(cell$vd) + cell$simp.scal + cell$simp.scal * log10(cell$vd))
summary(lm.dwt.vd.int.bellow)
confint(lm.dwt.vd.int.bellow)
###interacción no significativa, la eliminamos
lm.dwt.perfp <- lm(log10(cell$dwt)~cell$simp.scal)
summary(lm.dwt.vd.ancova)

#prueba de t y boxplot
boxplot(cell$dwt ~ cell$simp.scal, notch=T, ylab="Double wall thickness (um)")
t.test(cell$dwt~cell$simp.scal)

#####Ojo!
lm.dwt.perfo <-lm(log10(cell$dwt)~cell$simp.scal)
summary(lm.dwt.perfo)

#Single wall thickness~Vessel diameter just self-supporting
TukeyHSD(dwt.perfo.anova)

png("../Figures/DWT_perfplate.png")
boxplot(cell$dwt ~ cell$simp.scal[cell$simp.scal!="absent"], 
        notch = T, xlab = "Perforation plate type",
        ylab = expression(paste("Double wall thickness ", mu, "m")))
dev.off()

###Entre tipos de elemento no perforado#############
####################################################
#Regresar a base original
cell <- cell.temp.subseted

itetypes<-levels(as.factor(cell$ite.type))
itetypes

plot(log10(cell$ite.wt)~log10(cell$vd))
points(log10(cell$vwt[cell$ITE.type=="libriform"])~log10(cell$vd[cell$ITE.type=="libriform"]), col="green")
points(log10(cell$vwt[cell$ITE.type=="fiber tracheids"])~log10(cell$vd[cell$ITE.type=="fiber tracheids"]), col="blue")
points(log10(cell$vwt[cell$ITE.type=="tracheids"])~log10(cell$vd[cell$ITE.type=="tracheids"]), col="red")
points(log10(cell$vwt[cell$ITE.type=="absent"])~log10(cell$vd[cell$ITE.type=="absent"]), col="yellow")

##########
cell <- subset(cell, cell$vd>1)
cell <- subset(cell, cell$dwt>1)

#Eliminar ITE
cell <- subset(cell, cell$ite.type!="")
cell <- subset(cell, cell$ite.type!="absent")
cell <- subset(cell, cell$vd < 92.42)

######
#modelo los dos juntos
lm.dwt.vd.itetype.bellow <- lm(log10(cell$dwt)~log10(cell$vd) + cell$conductive)
summary(lm.dwt.vd.itetype.bellow)
confint(lm.dwt.vd.itetype.bellow)
plot(lm.vwt.vd.itetype.bellow)
#interacción
lm.dwt.vd.itetype.int.bellow <- lm(log10(cell$dwt)~log10(cell$vd) * cell$conductive)
summary(lm.dwt.vd.itetype.int.bellow)
confint(lm.dwt.vd.itetype.int.bellow)
#PLot to identify different ite types
plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$ITE.type=="libriform"])~log10(cell$vd[cell$ITE.type=="libriform"]), col="green")
points(log10(cell$vwt[cell$ITE.type=="fiber tracheids"])~log10(cell$vd[cell$ITE.type=="fiber tracheids"]), col="blue")
points(log10(cell$vwt[cell$ITE.type=="tracheids"])~log10(cell$vd[cell$ITE.type=="tracheids"]), col="red")


#ANOVA Double wall thickness~Vessel diameter just self-supporting
lm.dwt.itetype <- lm(log10(cell$dwt)~cell$ite.type)
dwt.perfo.anova <-aov(log10(cell$dwt)~cell$ite.type)
summary(dwt.perfo.anova)
#
TukeyHSD(dwt.perfo.anova)

boxplot(log10(cell$dwt)~cell$ite.type, notch=F)


#######
###Hacer modelo lineal con las dos variables categóricas
#
cell <- cell.temp.subseted
#Eliminar celdas sin datos
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$dwt > 0.1)
#
cell <- subset(cell, cell$simp.scal=="simple"|cell$simp.scal=="scalariform")
#
cell <- subset(cell, cell$vd < 92.42)
#
lm.dwt.perfo.type.bellow <- lm(log10(cell$dwt) ~ cell$simp.scal)
summary(lm.dwt.perfo.type.bellow)
confint(lm.dwt.perfo.type.bellow)
#Eliminar ITE's sin datos
cell <- subset(cell, cell$ite.type!="")
cell <- subset(cell, cell$ite.type!="absent")

#Discard level without information
lm.dwt.ite.type.bellow <- lm(log10(cell$dwt) ~ cell$conductive)
summary(lm.dwt.ite.type.bellow)
confint(lm.dwt.ite.type.bellow)

lm.dwt.perfo.itetype <- lm(log10(cell$dwt) ~ cell$simp.scal * cell$conductive)
summary(lm.dwt.perfo.itetype)
anova(lm.dwt.perfo.itetype)
#
boxplot(log10(cell$dwt) ~ cell$simp.scal * cell$conductive)
#
emm2 <- emmeans(lm.dwt.perfo.itetype, specs = pairwise ~ simp.scal:conductive)
emm2$emmeans
emm2$contrasts
#####
lm.dwt.vd.perfp.itetype.bellow <-lm(log10(cell$dwt)~ log10(cell$vd) + cell$conductive+ cell$simp.scal)
summary(lm.dwt.vd.perfp.itetype.bellow)
confint(lm.dwt.vd.perfp.itetype.bellow)

lm.dwt.perfp.itetype <-lm(log10(cell$dwt)~ cell$ite.type+ cell$simp.scal)
#####
lm.dwt.vd.perfp.itetype.int.bellow <-lm(log10(cell$dwt)~ log10(cell$vd) + cell$conductive * cell$simp.scal)
summary(lm.dwt.vd.perfp.itetype.int.bellow)
confint(lm.dwt.vd.perfp.itetype.int.bellow)
#####compare rsq
summary(lm.dwt.vd.perfotype.itetype)#.1057
summary(lm.dwt.perfotype.itetype)#0.07065
0.07065/0.1057