library(ggplot2)
library(dplyr)
library(emmeans) #library to get means of linear models with more than one categorical variable
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#############################################
#####Vessel wall thickness (vwt)###########
#############################################
#Plotting vwt and vd
plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]), col="green")
points(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]),col="blue")

#modelo sencillo para todos los diámtros de vaso
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd)
confint(lm.vwt.vd)

#Plot only self species
png("../figures/VD_VWT.png")
pdf("../figures/VD_VWT.pdf")
plot(log10(cell$vwt)~log10(cell$vd), pch=19, col="black",cex=0.8,
     xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Vessel wall thickness ", mu, "m")))
abline(lm.vwt.vd, col="red",lwd=2)
dev.off()
##### Use species behind the inflection point####
cell <- subset(cell, cell$vd < 90)
###Entre tipos de placa de perforación#############
#####################################
lm.vwt.vd.bellow <- lm(log10(cell$vwt)~log10(cell$vd))
summary(lm.vwt.vd.bellow)
confint(lm.vwt.vd.bellow)

plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]), col="green")
points(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]),col="blue")

#modelo solo simple
lm.vwt.vd.simple.bellow <- lm(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]))
summary(lm.vwt.vd.simple.bellow)
confint(lm.vwt.vd.simple.bellow)

#modelo solo scalariforme
lm.vwt.vd.scalariform.bellow <- lm(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]))
summary(lm.vwt.vd.scalariform.bellow)
confint(lm.vwt.vd.scalariform.bellow)

#modelo múltiple para ver vd y tipo de placa de perforacióm
lm.vdt.perfpl.bellow <- lm(log10(cell$vwt) ~ log10(cell$vd) + cell$simp.scal)
summary(lm.vdt.perfpl.bellow)
confint(lm.vdt.perfpl.bellow)
anova(lm.vdt.perfpl.bellow)
#modelo para probar diferencias en pendientes (interacción)
lm.vwt.vd.int.bellow <- lm(log10(cell$vwt) ~ log10(cell$vd) + cell$simp.scal + cell$simp.scal * log10(cell$vd))
summary(lm.vwt.vd.int.bellow)
confint(lm.vwt.vd.int.bellow)
anova(lm.vwt.vd.int.bellow)
###
png("../figures/VD_VWT_perftype.png")
pdf("../figures/VD_VWT_perftype.pdf")
plot(log10(cell$vwt)~log10(cell$vd),cex= 0.8,
     xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Vessel wall thickness ", mu, "m")))
points(log10(cell$vwt[cell$simp.scal=="simple"])~log10(cell$vd[cell$simp.scal=="simple"]),
       pch=19, cex= 0.8, col="green")
points(log10(cell$vwt[cell$simp.scal=="scalariform"])~log10(cell$vd[cell$simp.scal=="scalariform"]),
       pch=19, cex= 0.8,col="blue")
abline(lm.vwt.vd.simple.bellow, col="green")
abline(lm.vwt.vd.scalariform.bellow ,col="blue")
dev.off()
#
####T-test and boxplot
t.test(cell$vwt~cell$simp.scal)
t.test(log10(cell$vwt) ~ cell$simp.scal)
png("../figures/VWT_perfplate.png")
boxplot(cell$vwt ~ cell$simp.scal[cell$simp.scal!="absent"], 
        notch = T, xlab = "Perforation plate type",
        ylab = expression(paste("Vessel wall thickness ", mu, "m")))
dev.off()
png("../figures/VWT_log_perfplate.png")
boxplot(log10(cell$vwt) ~ cell$simp.scal[cell$simp.scal!="absent"], 
        notch = T, xlab = "Perforation plate type",
        ylab = expression(paste("Log-Vessel wall thickness ", mu, "m")))
dev.off()

#####Comparing between ite type#############
####################################################
#regresar a base original
cell <- cell.temp.subseted
#####ITEwt
itetypes<-levels(as.factor(cell$ite.type))
itetypes
##########
#Remove plants without ites
cell <- subset(cell, cell$ite.type!="absent")
#Use just plants bellow the threshold
cell <- subset(cell, cell$vd < 90)
#######Models using vessel diam. and ite type
lm.vwt.vd.itetype.bellow <- lm(log10(cell$vwt)~log10(cell$vd) + cell$conductive)
summary(lm.vwt.vd.itetype.bellow)
plot(lm.vwt.vd.itetype.bellow)
confint(lm.vwt.vd.itetype.bellow)
anova(lm.vwt.vd.itetype.bellow)
#interacción
lm.vwt.vd.itetype.int.bellow <- lm(log10(cell$vwt) ~ log10(cell$vd) * cell$conductive)
summary(lm.vwt.vd.itetype.int.bellow)
confint(lm.vwt.vd.itetype.int.bellow)
anova(lm.vwt.vd.itetype.int.bellow)
#PLot to identify different ite types
plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$ITE.type=="libriform"])~log10(cell$vd[cell$ITE.type=="libriform"]), col="green")
points(log10(cell$vwt[cell$ITE.type=="fiber tracheids"])~log10(cell$vd[cell$ITE.type=="fiber tracheids"]), col="blue")
points(log10(cell$vwt[cell$ITE.type=="tracheids"])~log10(cell$vd[cell$ITE.type=="tracheids"]), col="red")
##### Poner todas las variables
lm.vwt.vd.perfp.itetype.bellow <- lm(log10(cell$vwt) ~ log10(cell$vd) + cell$conductive +
                                       cell$simp.scal)
summary(lm.vwt.vd.perfp.itetype.bellow)
anova(lm.vwt.vd.perfp.itetype.bellow)
confint(lm.vwt.vd.perfp.itetype.bellow)
#inter
lm.vwt.vd.perfp.itetype.int <- lm(log10(cell$vwt) ~ log10(cell$vd) + cell$ite.type *
                                    cell$simp.scal)
summary(lm.vwt.vd.perfp.itetype.int)
anova(lm.vwt.vd.perfp.itetype.int)
##
boxplot(log10(cell$vwt) ~ cell$ite.type)
boxplot(log10(cell$vwt) ~ cell$conductive)
lm.vwt.simpscal.itetype<- lm(log10(cell$vwt) ~ cell$ite.type + cell$simp.scal)
##### Sacar r cuad parciales. #####
summary(lm.vwt.vd)#0.06359
summary(lm.vwt.simpscal.itetype)#0.0462
summary(lm.vwt.vd.perfp.itetype)#0.1308
r_vd <- 0.0462/0.1308
#
#Grosor del ITE y diámetro de los vasos.
#
kruskal.test(log10(cell$vwt) ~ cell$ite.type)

pairwise.wilcox.test(x = log10(cell$vwt), g = cell$ite.type, p.adjust.method = "holm" )
library(FSA)
## Dunn test library(FSA) PT =  
dunnTest(log10(cell$vwt) ~ cell$ite.type, data=cell, method="bh")
####
#Put as factor ite type just considering conductive vs not conductive
cell.ite.type <- as.factor(cell$ite.type)
cell.ite.type
levels(cell.ite.type) <- c("Non-Conductive", "Non-Conductive", "Conductive")
levels(cell.ite.type)

boxplot(log10(cell$vwt) ~ cell$ite.type, 
        notch = T, xlab = "ITE type",
        ylab = expression(paste("Log-Vessel wall thickness ", mu, "m")))

####Double boxplot * boxplot by two factors #####
#Regresar a la base completa
cell <- cell.temp.subseted
#
lm.vwt.perfotype.bellow <- lm(log10(cell$vwt) ~ cell$simp.scal)
summary(lm.vwt.perfotype.bellow)
confint(lm.vwt.perfotype.bellow)
#Removing rows without ITE information
cell <- subset(cell, cell$ite.type!="")
cell <- subset(cell, cell$ite.type!="absent")
#Filter vd menor a 92
cell <- subset(cell, cell$vd < 90)

#Discard level without information
lm.vwt.itetype.bellow <- lm(log10(cell$vwt) ~ cell$conductive)
summary(lm.vwt.itetype.bellow)
confint(lm.vwt.itetype.bellow)
#Change level of tracheids 
lm.vwt.perfo.itetype.bellow <- lm(log10(cell$vwt) ~ cell$simp.scal * cell$conductive)
#getting est. mean values
emm1 <-emmeans(lm.vwt.perfo.itetype.bellow,specs = pairwise ~ simp.scal:conductive)
emm1$emmeans
emm1$contrasts
multcomp::cld(emm1, alpha = 0.10, Letters = LETTERS)

emmeans(lm.vwt.perfo.itetype.bellow, ~ simp.scal:conductive)

boxplot(log10(cell$vwt) ~ cell$simp.scal * cell$conductive)
library(wesanderson)
pal<-wes_palette("GrandBudapest1")

###### Especies en las esquinas ######
library(dplyr)
cell <- cell.temp.subseted
plot(log10(cell$vwt)~log10(cell$vd))
library(ggplot2)
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
cell <- subset(cell, cell$dwt > 0.1)
cell <- subset(cell, cell$vd < 92.42)
cell <- subset(cell, cell$self..non == "self")
ggplot(cell, aes(x=(log10(vd)), 
                 y=(log10(vwt)), label=spe))+
  geom_point(size=1)+stat_smooth(formula= y~x, method = "lm")+
  geom_text(position = "identity", angle=25, size=2.5, alpha=0.8)

ggplot(cell, aes(x=(log10(vd)), 
                 y=(log10(dwt)), label=spe))+
  geom_point(size=1)+stat_smooth(formula= y~x, method = "lm")+
  geom_text(position = "identity", angle=25, size=2.5, alpha=0.8)

cell_sorted <- arrange(cell, vd)
cell <- cell_sorted
plot(cell$tbsq)

####pared sencilla
lm.vwt.vd <- lm(log10(cell$vwt) ~ log10(cell$vd))
summary(lm.vwt.vd)

residuales <- lm.vwt.vd$residuals
cell$residuals <- residuales

plot(log10(cell$vwt)~log10(cell$vd))
points(log10(cell$vwt[cell$residuals > 0.25])~log10(cell$vd[cell$residuals > 0.25]), col="green")
points(log10(cell$vwt[cell$residuals < -0.25])~log10(cell$vd[cell$residuals < -0.25]), col="red")

summary(cell$vd)
upperintvd <- cell[cell$vd > 57,]
lowerintvd <- cell[cell$vd < 28,] 

upperright <- upperintvd[upperintvd$residuals > 0.15,] 
upperright <- arrange(upperright, residuals)
upperright <- tail(upperright, 15)
upperright  <- arrange(upperright, WDratio)

lowerright <- upperintvd[upperintvd$residuals < -0.15,] 
lowerright <- arrange(lowerright, residuals)
lowerright <- head(lowerright, 15)
lowerright <- arrange(lowerright, WDratio)

upperleft <- lowerintvd[lowerintvd$residuals > 0.15,]
upperleft <- arrange(upperleft, residuals)
upperleft <- tail(upperleft, 15)
upperleft <- arrange(upperleft, WDratio)

lowerleft <- lowerintvd[lowerintvd$residuals < -0.15,] 
lowerleft <- arrange(lowerleft, residuals)
lowerleft <- head(lowerleft, 15)
lowerleft <- arrange(lowerleft, WDratio)


upperright[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
lowerright[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
upperleft[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
lowerleft[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]

##################
####pared doble
lm.dwt.vd <- lm(log10(cell$dwt) ~ log10(cell$vd))
summary(lm.dwt.vd)

residuales_dwt <- lm.dwt.vd$residuals
cell$residuals_dwt <- residuales_dwt

plot(log10(cell$dwt)~log10(cell$vd))
points(log10(cell$dwt[cell$residuals_dwt > 0.25])~log10(cell$vd[cell$residuals_dwt > 0.25]), col="green")
points(log10(cell$dwt[cell$residuals_dwt < -0.25])~log10(cell$vd[cell$residuals_dwt < -0.25]), col="red")

summary(cell$vd)
upperintvd <- cell[cell$vd > 57,]
lowerintvd <- cell[cell$vd < 28,] 

upperright_dwt <- upperintvd[upperintvd$residuals_dwt > 0.15,] 
upperright_dwt <- arrange(upperright_dwt, residuals_dwt)
upperright_dwt <- tail(upperright_dwt, 15)
upperright_dwt  <- arrange(upperright_dwt, WDratio)

lowerright_dwt <- upperintvd[upperintvd$residuals_dwt < -0.15,] 
lowerright_dwt <- arrange(lowerright_dwt, residuals_dwt)
lowerright_dwt <- head(lowerright_dwt, 15)
lowerright_dwt <- arrange(lowerright_dwt, WDratio)

upperleft_dwt <- lowerintvd[lowerintvd$residuals_dwt > 0.15,]
upperleft_dwt <- arrange(upperleft_dwt, residuals_dwt)
upperleft_dwt <- tail(upperleft_dwt, 15)
upperleft_dwt <- arrange(upperleft_dwt, WDratio)

lowerleft_dwt <- lowerintvd[lowerintvd$residuals_dwt < -0.15,] 
lowerleft_dwt <- arrange(lowerleft_dwt, residuals_dwt)
lowerleft_dwt <- head(lowerleft_dwt, 15)
lowerleft_dwt <- arrange(lowerleft_dwt, WDratio)


upperright_dwt[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
lowerright_dwt[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
upperleft_dwt[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
lowerleft_dwt[,c("fam","spe","vd","vwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]


###
setdiff(upperright$spe,upperright_dwt$spe)
setdiff(lowerright$spe,lowerright_dwt$spe)
setdiff(upperleft$spe,upperleft_dwt$spe)
setdiff(lowerleft$spe,lowerleft_dwt$spe)
