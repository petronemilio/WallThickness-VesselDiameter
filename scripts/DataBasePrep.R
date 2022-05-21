######Script for loading and renaming columns from the Vessel-Diameter script
#####
#Print r version 
R.version
#Load useful libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(emmeans) #library to get means of linear models with more than one categorical variable
#To download data from TRY database you need to register. Then copy the url of the database
download.file('https://www.try-db.org/tmpdnld/Try2022521181315815_VesselDiameterVesselWallThickness.xlsx'
              ,'data/VesselDiameterVesselWallThickness.xlsx')
#You can open the xlsx file and save it as csv or use read.xls comand
cell<-read_xlsx("data/VesselDiameterVesselWallThickness.xlsx")
summary(cell)
#Change column names
names(cell)[1:8] <- c("ord", "fam", "gen","sp","auth", "col","ncol","o")
names(cell)[names(cell) == "Vessel.diameter.micrometers.curated"] <- "vd"
names(cell)[names(cell) == "Vessel.wall.thickness.micrometers.curated"] <- "vwt"
names(cell)[names(cell)=="perforation.plate.type.curated"] <- "simp.scal"
names(cell)[names(cell)=="Imperforate.tracheary.element.wall.thickness.micrometers.curated"] <- "ite.wt"
names(cell)[names(cell)=="Imperforate.tracheary.element.type.curated"] <- "ite.type"
names(cell)[names(cell)=="Vessels.group.curated"] <- "vg"
names(cell)[names(cell)=="Imperforate.tracheary.element.diametermicrometers.curated"] <- "ite.d"
#names(cell)[names(cell)=="self.supporting"
names(cell)[names(cell)=="DWT..Double.wall.thickness..um."] <- "dwt"
####numeric
cell$vd <- as.numeric(cell$vd)
cell$vwt <- as.numeric(cell$vwt)
cell$ite.wt <- as.numeric(cell$ite.wt)
cell$dwt <- as.numeric(cell$dwt)
cell$vg <- as.numeric(cell$vg)
cell$dwt.conditional <- ifelse(cell$vg > 1.5, cell$vwt*2, cell$vwt+cell$ite.wt)
cell$spe <- paste(cell$gen, cell$sp, sep=" ")
##
cell$conductive <- cell$ite.type == "tracheids" #asign logical values to conductive. Tracheids =TRUE
cell$conductive <- as.numeric(cell$conductive) #CONVERT TO NUMERIC 0= libriform and fiber tracheids
cell$conductive[cell$conductive==0] <- "Non-conductive"  #0nonconductive
cell$conductive[cell$conductive==1] <- "Conductive" #1 conductive
cell$WDratio <- round((cell$vwt/cell$vd), 2) ###ratio between vessel wall thickness and vessel diameter 
cell$DWDratio <- round((cell$dwt/cell$vd), 2) ###ratio double wall thickness and vessel diameter

###Temporal copy from base
cell.temp.subseted <- cell
