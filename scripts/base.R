#####
#Print r version 
R.version
#Load useful libraries
library(ggplot2)
library(dplyr)
library(emmeans) #library to get means of linear models with more than one categorical variable

#Load the database
cell <- read.csv("Data/XylemMorphospace050519 - datos morfoespacio.csv")


##nombres
names(cell)
names(cell)[names(cell)=="ï..order" ] <- "ord"
names(cell)[names(cell)=="family" ] <- "fam"
names(cell)[names(cell)=="genus" ] <- "gen"
names(cell)[names(cell)=="species" ] <- "sp"
names(cell)[names(cell)=="authority" ] <- "auth"
names(cell)[names(cell)=="collector" ] <- "col"
names(cell)[names(cell)=="collection." ] <- "ncol"
names(cell)[names(cell)=="Organ" ] <- "o"
names(cell)[names(cell)=="Stem.Diameter.cm" ] <- "sd"
names(cell)[names(cell)=="Height.m" ] <- "h"
names(cell)[names(cell)=="VDFinal.Vessel.diameter.interpreted" ] <- "vd"
names(cell)[names(cell)=="VG.curated" ] <- "vg"
#VMM.FINAL contains descritpions for possible outliers.
#We should probably use VMM.Vessel.density.mm.2.RAW
names(cell)[names(cell
                  