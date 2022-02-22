######Script for loading and renaming columns from the Vessel-Diameter script
#####
#Print r version 
R.version
#Load useful libraries
library(ggplot2)
library(dplyr)
library(emmeans) #library to get means of linear models with more than one categorical variable

#Load the database
#cell <- read.csv("../conduitDiameter/Data/VesselDiameterVesselWall.csv",row.names = 1)
cell <- read.csv("data/VesselDiameterVesselWall.csv",row.names = 1)
#Change column names
names(cell)[1:8] <- c("ord", "fam", "gen","sp","auth", "col","ncol","o")
names(cell)[names(cell) == "VDFinal.Vessel.diameter.interpreted"] <- "vd"
names(cell)[names(cell) == "VWTFINAL.Vessel.wall.thickness.draw.micrometers"] <- "vwt"
names(cell)[names(cell)=="FINAL.scalariform.not" ] <- "simp.scal"
names(cell)[names(cell)=="ITEWTFINAL..wall.thick.final.micrometers" ] <- "ite.wt"
names(cell)[names(cell)=="ITE.Final" ] <- "ite.type"
names(cell)[names(cell)=="self.non"] <- "self"
names(cell)[names(cell)=="DWT..Double.wall.thickness..um."  ] <- "dwt"
####numeric
cell$vd <- as.numeric(cell$vd)
cell$vwt <- as.numeric(cell$vwt)
cell$ite.wt <- as.numeric(cell$ite.wt)
cell$dwt <- as.numeric(cell$dwt)
cell$spe <- paste(cell$gen, cell$sp, sep=" ")
##
cell$conductive <- cell$ite.type == "tracheids" #asign logical values to conductive. Tracheids =TRUE
cell$conductive <- as.numeric(cell$conductive) #CONVERT TO NUMERIC 0= libriform and fiber tracheids
cell$conductive[cell$conductive==0] <- "Non-conductive"  #0nonconductive
cell$conductive[cell$conductive==1] <- "Conductive" #1 conductive
cell$WDratio <- round((cell$vwt/cell$vd), 2) ###razón grosor pared entre diametro 
cell$DWDratio <- round((cell$dwt/cell$vd), 2) ###razón grosor pared doble entre diametro

###Temporal copy from base
cell.temp.subseted <- cell
