###### Especies en las esquinas VWT ######
library(dplyr)
library(ggplot2)

cell <- cell.temp.subseted
cell$WDratio <- round((cell$vwt/cell$vd), 2) ###razón grosor pared entre diametro 
cell$DWDratio <- round((cell$dwt/cell$vd), 2) ###razón grosor pared doble entre diametro
plot(log10(cell$vwt)~log10(cell$vd))
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
cell <- subset(cell, cell$vd < 90)
cell <- subset(cell, cell$self..non == "self")
plot((cell$vwt)~(cell$vd), log="xy")

#cuartiles
summary((cell$vwt))
summary((cell$vd))


upperintvd <- subset(cell, cell$vd > 58) #Tercer cuartil
lowerintvd <- subset(cell, cell$vd < 28) #Primer cuartil

upperleft <- subset(lowerintvd, lowerintvd$vwt > 3) #lowerintvd[lowerintvd$vwt > 3,]
lowerleft <-  subset(lowerintvd, lowerintvd$vwt < 2) #lowerintvd[lowerintvd$vwt < 2,]
upperright <- subset(upperintvd, upperintvd$vwt > 3) #upperintvd[lowerintvd$vwt > 3,]
lowerright <- subset(upperintvd, upperintvd$vwt < 2) #upperintvd[lowerintvd$vwt < 2,]

upperleft <- arrange(upperleft, vwt)
upperleft <- tail(upperleft, 15)
upperleft  <- arrange(upperleft, 1/WDratio)

upperright <- arrange(upperright, vwt)
upperright <- tail(upperright, 15)
upperright  <- arrange(upperright, 1/WDratio)


lowerright <- arrange(lowerright, vwt)
lowerright <- head(lowerright, 15)
lowerright  <- arrange(lowerright, WDratio)


lowerleft <- arrange(lowerleft, vwt)
lowerleft <- head(lowerleft, 15)
lowerleft  <- arrange(lowerleft, WDratio)


upperleft$spe
upperright$spe
lowerright$spe
lowerleft$spe

points(upperleft$vwt~upperleft$vd, col="green", pch=19)
points(lowerleft$vwt~lowerleft$vd, col="red", pch=19)
points(upperright$vwt~upperright$vd, col="green", pch=19)
points(lowerright$vwt~lowerright$vd, col="red", pch=19)


cell$vd[cell$spe=="Nicotiana glauca"]
cell$WDratio[cell$spe=="Nicotiana glauca"]

cell$vd[cell$gen=="Nicotiana"]
cell$vwt[cell$gen=="Nicotiana"]


###### Especies en las esquinas DWT ######


cell <- cell.temp.subseted
cell$WDratio <- round((cell$vwt/cell$vd), 2) ###razón grosor pared entre diametro 
cell$DWDratio <- round((cell$dwt/cell$vd), 2) ###razón grosor pared doble entre diametro
plot(log10(cell$dwt)~log10(cell$vd))
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$dwt > 0.1)
cell <- subset(cell, cell$vd < 90)
cell <- subset(cell, cell$self..non == "self")
plot((cell$dwt)~(cell$vd), log="xy")

#cuartiles
summary((cell$dwt))
summary((cell$vd))


upperintvd <- subset(cell, cell$vd > 58) #Tercer cuartil
lowerintvd <- subset(cell, cell$vd < 28) #Primer cuartil

upperleft_dwt <- subset(lowerintvd, lowerintvd$dwt > 6.625) #lowerintvd[lowerintvd$dwt > 3,]
lowerleft_dwt <-  subset(lowerintvd, lowerintvd$dwt < 4.6) #lowerintvd[lowerintvd$dwt < 2,]
upperright_dwt <- subset(upperintvd, upperintvd$dwt > 6.625) #upperintvd[lowerintvd$dwt > 3,]
lowerright_dwt <- subset(upperintvd, upperintvd$dwt < 4.6) #upperintvd[lowerintvd$dwt < 2,]

upperleft_dwt <- arrange(upperleft_dwt, dwt)
upperleft_dwt <- tail(upperleft_dwt, 15)
upperleft_dwt  <- arrange(upperleft_dwt, 1/DWDratio)

upperright_dwt <- arrange(upperright_dwt, dwt)
upperright_dwt <- tail(upperright_dwt, 15)
upperright_dwt  <- arrange(upperright_dwt, 1/DWDratio)


lowerright_dwt <- arrange(lowerright_dwt, dwt)
lowerright_dwt <- head(lowerright_dwt, 15)
lowerright_dwt  <- arrange(lowerright_dwt, DWDratio)


lowerleft_dwt <- arrange(lowerleft_dwt, dwt)
lowerleft_dwt <- head(lowerleft_dwt, 15)
lowerleft_dwt  <- arrange(lowerleft_dwt, DWDratio)


upperleft_dwt$spe
upperright_dwt$spe
lowerright_dwt$spe
lowerleft_dwt$spe

points(upperleft_dwt$dwt~upperleft_dwt$vd, col="green", pch=19)
points(lowerleft_dwt$dwt~lowerleft_dwt$vd, col="red", pch=19)
points(upperright_dwt$dwt~upperright_dwt$vd, col="green", pch=19)
points(lowerright_dwt$dwt~lowerright_dwt$vd, col="red", pch=19)



###Tablas S1 y S2


#Tabla VWT
UpperRightCorner <- upperright[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
LowerRightCorner <- lowerright[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
UpperLeftCorner <-upperleft[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
LowerLeftCorner <-lowerleft[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]

write.csv(UpperRightCorner, "UpperRightCorner.csv")
write.csv(LowerRightCorner, "LowerRightCorner.csv")
write.csv(UpperLeftCorner, "UpperLeftCorner.csv")
write.csv(LowerLeftCorner, "LowerLeftCorner.csv")

#Tabla DWT

UpperRightCornerDW <- upperright_dwt[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
LowerRightCornerDW <- lowerright_dwt[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
UpperLeftCornerDW <- upperleft_dwt[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]
LowerLeftCornerDW <-lowerleft_dwt[,c("fam","spe","vd","vwt", "dwt", "ite.type", "simp.scal", "WDratio", "DWDratio")]


write.csv(UpperRightCornerDW, "UpperRightCornerDW.csv")
write.csv(LowerRightCornerDW, "LowerRightCornerDW.csv")
write.csv(UpperLeftCornerDW, "UpperleftCornerDW.csv")
write.csv(LowerLeftCornerDW, "LowerLeftCornerDW.csv")

###
setdiff(upperright$spe,upperright_dwt$spe)
setdiff(lowerright$spe,lowerright_dwt$spe)
setdiff(upperleft$spe,upperleft_dwt$spe)
setdiff(lowerleft$spe,lowerleft_dwt$spe)

###
intersect(upperright$spe,upperright_dwt$spe)
intersect(lowerright$spe,lowerright_dwt$spe)
intersect(upperleft$spe,upperleft_dwt$spe)
intersect(lowerleft$spe,lowerleft_dwt$spe)


