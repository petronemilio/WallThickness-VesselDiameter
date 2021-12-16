#######Loading packages required for ecologic analysis################
library(letsR)
library(dismo)
library(BIEN)
library(raster)
library(ggplot2)
library(plotbiomes)
library(kgc)
library(gridExtra)
#######Extract ocurrences for all species of Carlquist database
ocurrences_dataframe <-data.frame()
cell_species <- unique(cell$spe)
for (species in cell_species){
  temporal <- BIEN_occurrence_species(species, political.boundaries = T)
  ocurrences_dataframe <- rbind(ocurrences_dataframe, temporal)
}
####Associate climate variables
length(unique(ocurrences_dataframe$scrubbed_species_binomial))
bioclim.data <- getData(name = "worldclim",var = "bio",
                        res = 2.5)
class(bioclim.data)
#
r <- bioclim.data[[c(1,2,4,5,6,7,8,9,10,11,12,13,14,15)]]
names(r) <- c("Temp","MeanDiurnalRange","Temp-Seasonality","MaxTempWarmestMonth",
              "MinTempColdestMonth","Temp-AnnualRange","MeanTempWettestQrt","MeanTempDryestQrt",
              "MeanTempWarmestQrt","MeanTempColdestQrt","Ann-Prec","PrecWetMonth",
              "PrecDriestMonth","Prec-Seasonality")
ocurrences_dataframe <- ocurrences_dataframe[!is.na(ocurrences_dataframe$latitude),]
ocurrences_dataframe <- ocurrences_dataframe[!is.na(ocurrences_dataframe$longitude),]
#
coords <- data.frame(x = ocurrences_dataframe$longitude, y = ocurrences_dataframe$latitude)
points <- SpatialPoints(coords, proj4string = r@crs)
#
values <- extract(r,points)
#
ocurrences_dataframe <- cbind.data.frame(ocurrences_dataframe,values)
write.csv(ocurrences_dataframe, "Data/CarlquistBIENocurrences.csv")
ocurrences_dataframe <- read.csv("Data/CarlquistBIENocurrences.csv", row.names = 1)
#
length(unique(ocurrences_dataframe$scrubbed_species_binomial))
head(sort(table(ocurrences_dataframe$scrubbed_species_binomial),decreasing = TRUE))
sort(table(ocurrences_dataframe$scrubbed_species_binomial))   
#### 
ocurrences_dataframe_mediandist <- aggregate(ocurrences_dataframe[,6:7],
                                             list(species=ocurrences_dataframe$scrubbed_species_binomial,
                                                  province=ocurrences_dataframe$state_province,
                                                  county=ocurrences_dataframe$county),median) 
#Add variables to median datafrmae
ocurrences_dataframe_mediandist <- ocurrences_dataframe_mediandist[!is.na(ocurrences_dataframe_mediandist$latitude),]
ocurrences_dataframe_mediandist <- ocurrences_dataframe_mediandist[!is.na(ocurrences_dataframe_mediandist$longitude),]
#
coords <- data.frame(x = ocurrences_dataframe_mediandist$longitude, y = ocurrences_dataframe_mediandist$latitude)
points <- SpatialPoints(coords, proj4string = r@crs)
#
values <- extract(r,points)
ocurrences_dataframe_mediandist <- cbind.data.frame(ocurrences_dataframe_mediandist,values)
sort(table(ocurrences_dataframe_mediandist$species),decreasing = TRUE)
write.csv(ocurrences_dataframe_mediandist, "Data/CarlquistBIENmedianocurrences.csv")
ocurrences_dataframe_mediandist <- read.csv("Data/CarlquistBIENmedianocurrences.csv", row.names = 1)
#####
ocurrences_dataframe_mediandist_sp <- aggregate(ocurrences_dataframe[,6:7],
                                                list(species=ocurrences_dataframe$scrubbed_species_binomial),median) 
#
coords <- data.frame(x = ocurrences_dataframe_mediandist_sp$longitude, y = ocurrences_dataframe_mediandist_sp$latitude)
points <- SpatialPoints(coords, proj4string = r@crs)
#
values <- extract(r,points)
ocurrences_dataframe_mediandist_sp <- cbind.data.frame(ocurrences_dataframe_mediandist_sp,values)
ocurrences_dataframe_mediandist_sp <- read.csv("Data/CarlquistBIENmedianspocurrences.csv", row.names =1)
rm(r)
rm(points)
rm(values)
rm(bioclim.data)
rm(coords)
######################################################
##### Based on kgc package return climatic zones #####
ocurrences_dataframe$latituderound <- RoundCoordinates(ocurrences_dataframe$latitude,res='course',latlong='lat')
ocurrences_dataframe$longituderound <- RoundCoordinates(ocurrences_dataframe$longitude,res='course',latlong='lon')

ocurrences_dataframe$Cls <- climatezones[match(paste(ocurrences_dataframe$latituderound,ocurrences_dataframe$longituderound),
                                               paste(climatezones$Lat,climatezones$Lon)),'Cls']
write.csv(ocurrences_dataframe_mediandist_sp, "Data/CarlquistBIENmedianspocurrences.csv")
table(ocurrences_dataframe$scrubbed_species_binomial,ocurrences_dataframe$Cls)
####
ocurrences_dataframe_mediandist$latituderound <- RoundCoordinates(ocurrences_dataframe_mediandist$latitude,res='course',latlong='lat')
ocurrences_dataframe_mediandist$longituderound <- RoundCoordinates(ocurrences_dataframe_mediandist$longitude,res='course',latlong='lon')

ocurrences_dataframe_mediandist$Cls <- climatezones[match(paste(ocurrences_dataframe_mediandist$latituderound,
                                                                ocurrences_dataframe_mediandist$longituderound),
                                                          paste(climatezones$Lat,climatezones$Lon)),'Cls']
table(ocurrences_dataframe_mediandist$species,ocurrences_dataframe_mediandist$Cls)
####
ocurrences_dataframe_mediandist_sp$latituderound <- RoundCoordinates(ocurrences_dataframe_mediandist_sp$latitude,res='course',latlong='lat')
ocurrences_dataframe_mediandist_sp$longituderound <- RoundCoordinates(ocurrences_dataframe_mediandist_sp$longitude,res='course',latlong='lon')

ocurrences_dataframe_mediandist_sp$Cls <- climatezones[match(paste(ocurrences_dataframe_mediandist_sp$latituderound,
                                                                   ocurrences_dataframe_mediandist_sp$longituderound),
                                                             paste(climatezones$Lat,climatezones$Lon)),'Cls']
plot(ocurrences_dataframe$Cls)
plot(ocurrences_dataframe_mediandist$Cls)
plot(ocurrences_dataframe_mediandist_sp$Cls)
#Determine total of species for which we have georeference information
sum(is.na(ocurrences_dataframe$latitude))
sum(is.na(ocurrences_dataframe$longitude))
length(unique(ocurrences_dataframe$scrubbed_species_binomial))
length(unique(cell$spe))
# Adding plot 
# Determine observations observed from the database by species 
#Leave just one observation per species and climate 
climate_freq <- ocurrences_dataframe[!duplicated(ocurrences_dataframe[,c("scrubbed_species_binomial","Cls")]),]
climate_freq <- as.data.frame(table(climate_freq$Cls))
####Determine number of species with cls info
clsnumber<-ocurrences_dataframe[!is.na(ocurrences_dataframe$Cls),]
length(unique(clsnumber$scrubbed_species_binomial))
rm(clsnumber)
#save image
climate1 <- ggplot(data = climate_freq, aes(x =Var1,y= Freq ))+
  geom_bar(stat="identity")
ggsave("Figures/climate1.pdf",climate1, device = "pdf")
climate_freq <- as.data.frame(table(ocurrences_dataframe$Cls))
climate2 <- ggplot(data = climate_freq, aes(x =Var1,y= Freq ))+
  geom_bar(stat="identity")
ggsave("Figures/climate2.pdf",climate2, device = "pdf")
rm(climate_freq, climate1, climate2)
##### Add identifier to ocurrence dataframe from dataframes of species from the corners
upperleft <- read.csv("meta/UpperLeftCorner.csv", row.names = 1)
upperleftdw <- read.csv("meta/UpperleftCornerDW.csv", row.names = 1)
nvthickw <- rbind(upperleft, upperleftdw)
#
lowerleft <- read.csv("meta/LowerLeftCorner.csv", row.names = 1)
lowerleftdw <- read.csv("meta/LowerLeftCornerDW.csv", row.names = 1)
nvnarrw <- rbind(lowerleft, lowerleftdw)
#
upperright <- read.csv("meta/UpperRightCorner.csv", row.names = 1)
upperrightdw <- read.csv("meta/UpperRightCornerDW.csv", row.names = 1)
wvthickw <- rbind(upperright, upperrightdw)
#
lowerright <- read.csv("meta/LowerRightCorner.csv", row.names = 1)
lowerrightdw <- read.csv("meta/LowerRightCornerDW.csv", row.names = 1)
wvnarrw <- rbind(lowerright, lowerrightdw)
#### Creating unique data frame of species from the corner
corners <- cbind(rep("Narrow vessels thin walls",length(nvnarrw$spe)),nvnarrw)
colnames(corners)[1] <- "Corner"
temp <- cbind(rep("Narrow vessels thick walls",length(nvthickw$spe)),nvthickw)
colnames(temp)[1] <- "Corner"
corners <- rbind(corners, temp)
temp <- cbind(rep("Wide vessels thin walls",length(wvnarrw$spe)),wvnarrw)
colnames(temp)[1] <- "Corner"
corners <- rbind(corners, temp)
temp <- cbind(rep("Wide vessels thick walls",length(wvthickw$spe)),wvthickw)
colnames(temp)[1] <- "Corner"
corners <- rbind(corners, temp)
rm(lowerleft,lowerleftdw, upperleft, upperleftdw,lowerright, 
   lowerrightdw,temp,wvnarrw,wvthickw,nvnarrw,nvthickw)
###Populate the data frame
ocurrences_dataframe$corners <- NA
ocurrences_dataframe_mediandist$corners <- NA
#Add ids
matcher <-match(ocurrences_dataframe$scrubbed_species_binomial, corners$spe)
matcher <- corners$Corner[matcher]
ocurrences_dataframe$corners<- matcher
ocurrences_dataframe$corners[is.na(ocurrences_dataframe$corners)] <- "Not from corner"
table(ocurrences_dataframe$corners)
#####Median list create the same for the mediandist data frame 
matcher <-match(ocurrences_dataframe_mediandist$species, corners$spe)
matcher <- corners$Corner[matcher]
ocurrences_dataframe_mediandist$corners<- matcher
ocurrences_dataframe_mediandist$corners[is.na(ocurrences_dataframe_mediandist$corners)] <- "Not from corner"
table(ocurrences_dataframe_mediandist$corners)
####Add new corners info to celldata frame####
matcher <- match(cell$spe, corners$spe)
matcher <- corners$Corner[matcher]
cell$corner <- matcher
cell$corner[is.na(cell$corner)] <- "Not from corner"
table(cell$corner)
rm(matcher)
#### Continue with info about geo info#####
corners <- subset(ocurrences_dataframe, ocurrences_dataframe$corners != "Not from corner")
cornerstable <- as.data.frame(table(corners$Cls, corners$corners))
str(corners)
#
corners_1 <- cornerstable %>% ggplot(aes(x=Var1, fill=Var1, y=Freq))+
  geom_col()+  
  facet_grid(~Var2) +coord_flip()
ggsave("Figures/corner_climate_all.pdf", corners_1, device = "pdf")

cornerstable <- corners[!duplicated(corners[,c("scrubbed_species_binomial","Cls")]),]
cornerstable <- as.data.frame(table(cornerstable$Cls, cornerstable$corners))
climate2 <- ggplot(data = cornerstable, aes(x =Var1,fill=Var1,y= Freq ))+
  geom_col() + facet_grid(~Var2) +coord_flip()
ggsave("Figures/corner_climate_oneocurrence.pdf", climate2, device = "pdf")
rm(climate2, cornerstable, corners_1)
########Repeat procedure for the median list
corners_m <- subset(ocurrences_dataframe_mediandist, ocurrences_dataframe_mediandist$corners != "Not from corner")
cornerstable_m <- as.data.frame(table(corners_m$Cls, corners_m$corners))
#
cornerstable_m %>% ggplot(aes(x=Var1, fill=Var1, y=Freq))+
  geom_col()+  
  facet_grid(~Var2) +coord_flip()
#Nicotiana info
nicotiana<- corners[grepl("Nicotiana ", corners$scrubbed_species_binomial),]
nicotiana <- as.data.frame(table(nicotiana$Cls, nicotiana$corners))
nicotiana_glauca <- nicotiana %>% ggplot(aes(x=Var1, fill=Var1, y=Freq))+
  geom_col()+  
  facet_grid(~Var2) +coord_flip()+ theme(legend.position = "none")
solanum<- corners[grepl("Solanum rugosum", corners$scrubbed_species_binomial),]
solanum <-corners[grepl("Solanum ", corners$scrubbed_species_binomial),]
solanum <- as.data.frame(table(solanum$Cls, solanum$corners))
solanum_rugosum <- solanum %>% ggplot(aes(x=Var1, fill=Var1, y=Freq))+
  geom_col()+  
  facet_grid(~Var2) + coord_flip()
#
pdf("Figures/NicotianaSolanum.pdf")
grid.arrange(nicotiana_glauca, solanum_rugosum,nrow = 2)
dev.off()
########
#ocurrences_dataframe_mediandist_sp$Temp <- (ocurrences_dataframe_mediandist_sp$Temp)/10
ocurrences_dataframe <- ocurrences_dataframe %>%
  mutate_at(vars(Temp:Prec.Seasonality), funs(./10))
ocurrences_dataframe_mediandist <- ocurrences_dataframe_mediandist %>%
  mutate_at(vars(Temp:Prec.Seasonality), funs(./10))
ocurrences_dataframe_mediandist_sp <- ocurrences_dataframe_mediandist_sp %>%
  mutate_at(vars(Temp:Prec.Seasonality), funs(./10))
#############################################
###### Plotting biomes Withaker biomes ######
wit <- whittaker_base_plot()
witreduced <- wit + theme(legend.position = "none")
all <- wit + geom_point(data = ocurrences_dataframe, 
                        aes(Temp, Ann.Prec),alpha=0.3)
withoutcorners <- subset(ocurrences_dataframe,ocurrences_dataframe$corners != "Not from corner")
corn <- wit + geom_point(data = withoutcorners, 
                         aes(Temp, Ann.Prec, colour = factor(corners)),alpha=0.5)
g <- arrangeGrob(witreduced, all,corn, nrow=3) 
####w
ggsave(file="Figures/WhittakerAll.pdf", g,width = 6, height = 8, 
       device="pdf",useDingbats = TRUE)
ggsave(file="Figures/Whittaker1.pdf", witreduced,width = 8, height =6 , 
       device="pdf",useDingbats = TRUE)
ggsave(file="Figures/Whittaker2.pdf", all,width = 8, height = 6, 
       device="pdf",useDingbats = TRUE)
ggsave(file="Figures/Whittaker3.pdf", corn,width = 8, height = 6, 
       device="pdf",useDingbats = TRUE)
ggsave(file="Figures/WhittakerAll.png", g,width = 8, height = 10, 
       device="png")
#pdf("Figures/WhittakerAll.pdf")
#grid.arrange(witreduced, all, corn,nrow = 3)
#dev.off()
#
allmedian <- wit + geom_point(data = ocurrences_dataframe_mediandist, 
                              aes(Temp, Ann.Prec),alpha=0.3)
withoutcorners_median <- subset(ocurrences_dataframe_mediandist,ocurrences_dataframe_mediandist$corners != "Not from corner")
corn_median <- wit + geom_point(data = withoutcorners_median, 
                                aes(Temp, Ann.Prec, colour = factor(corners)),alpha=0.8)
g <- arrangeGrob(witreduced,allmedian,corn_median, nrow=3) 
####w
ggsave(file="Figures/WhittakerAllMedian.pdf", allmedian,width = 10, height = 6, 
       device="pdf",useDingbats = TRUE)
ggsave(file="Figures/WhittakerMediancorners.pdf", corn_median,width = 10, height = 6, 
       device="pdf",useDingbats = TRUE)

#
png("Figures/WhittakerPlot.png")
pdf("Figures/WhittakerPlot.pdf")
plot(ocurrences_dataframe_mediandist$Ann.Prec~ocurrences_dataframe_mediandist$Temp,
     xlab= expression(paste("Mean Annual Temperture")), 
     ylab= expression(paste("Mean Annual Precipitiation (mm)")))
points(ocurrences_dataframe_mediandist$Ann.Prec[ocurrences_dataframe_mediandist$corners=="Wide vessels thin walls"]
       ~ ocurrences_dataframe_mediandist$Temp[ocurrences_dataframe_mediandist$corners=="Wide vessels thin walls"], col="green")
points(ocurrences_dataframe_mediandist$Ann.Prec[ocurrences_dataframe_mediandist$corners=="Narrow vessels thin walls"]
       ~ ocurrences_dataframe_mediandist$Temp[ocurrences_dataframe_mediandist$corners=="Narrow vessels thin walls"], col="red")
points(ocurrences_dataframe_mediandist$Ann.Prec[ocurrences_dataframe_mediandist$corners=="Narrow vessels thick walls"]
       ~ ocurrences_dataframe_mediandist$Temp[ocurrences_dataframe_mediandist$corners=="Narrow vessels thick walls"], col="blue", pch=18)
points(ocurrences_dataframe_mediandist$Ann.Prec[ocurrences_dataframe_mediandist$corners=="Wide vessels thick walls"]
       ~ ocurrences_dataframe_mediandist$Temp[ocurrences_dataframe_mediandist$corners=="Wide vessels thick walls"], col="orange", pch=18)
dev.off()

levels(as.factor(ocurrences_dataframe$corners))
######Information about functional traits obtained from bien ####
######### Using bien traits ##################
#Creating the databse
traits_dataframe <-data.frame()
cell_species <- unique(cell$spe)
for (species in cell_species){
  temporal <- BIEN_trait_species(species)
  traits_dataframe <- rbind(traits_dataframe, temporal)
}
#Writing the database
write.csv(traits_dataframe, "Data/CarlquistBIENtraits.csv")
traits_dataframe <- read.csv("Data/CarlquistBIENtraits.csv", row.names = 1)
######Check number of species#######
head(sort(table(traits_dataframe$trait_name),decreasing = TRUE))
###
woodiness <- subset(traits_dataframe, traits_dataframe$trait_name == "whole plant woodiness")  
woodiness <- woodiness[!duplicated(woodiness[1:2]),]
####Use match to get wood density
matcher <- match(cell$spe, woodiness$scrubbed_species_binomial)
matcher <- woodiness$trait_value[matcher]
cell$woodiness <- matcher
rm(woodiness)
#####Make plot to see variation in woodiness
png("Figures/VD_VWT_woodiness.png")
plot(log10(cell$vwt)~ log10(cell$vd),xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Vessel wall thickness ", mu, "m")))
points(log10(cell$vwt[cell$woodiness=="woody"])~ 
         log10(cell$vd[cell$woodiness=="woody"]), col="black", pch= 19)
points(log10(cell$vwt[cell$woodiness=="herbaceous"])~ 
         log10(cell$vd[cell$woodiness=="herbaceous"]), col="green", pch= 19)
points(log10(cell$vwt[cell$woodiness=="variable"])~ 
         log10(cell$vd[cell$woodiness=="variable"]), col="red", pch= 19)
dev.off()
#######
growthform <- subset(traits_dataframe, traits_dataframe$trait_name == "whole plant growth form")
table(growthform$trait_value)
rm(growthform)

#####
height <- subset(traits_dataframe, traits_dataframe$trait_name == "whole plant height")
height <- aggregate(as.numeric(height$trait_value), list(spe=height$scrubbed_species_binomial), mean)
matcher <- match(cell$spe, height$spe)
matcher <- height$x[matcher]
cell$heigth <- matcher
summary(cell$heigth)
##### Constructing try traits database#####
#filenames <- gsub("Data/try/17246segmenta\\.txt$","",list.files("Data/try/", pattern = "17246segment"))
#filenames <- paste("Data/try/",filenames, sep="")
#gsub("Data/try/17246segmenta\\.txt$","",list.files(pattern = "Data/try/17246segmenta"))
#matching.df <- data.frame()
#for(i in filenames){
# temporal <- read.csv(i,sep ='\t', header = FALSE, quote = "",stringsAsFactors = FALSE,fileEncoding="latin1")
#temporal<-temporal[!is.na(match(temporal$V5, cell$spe)),]
#matching.df <- rbind(matching.df, temporal)
#rm(temporal)
#}
#write.csv(matching.df, "Data/try_carlquist.csv")
#Loading
try_carlquist<-read.csv("Data/try_carlquist.csv", row.names = 1)
head(sort(table(try_carlquist$V13), decreasing = TRUE),10)
try_carlquist <- subset(try_carlquist, try_carlquist$V13 != "Latitude" & try_carlquist$V13!= "Longitude")
try_carlquist <- subset(try_carlquist, try_carlquist$V13 != "Species phylogenic group")
try_carlquist <- subset(try_carlquist, try_carlquist$V13 != "Mean annual temperature (MAT)" &
                          try_carlquist$V13 != "Mean sum of annual precipitation (PPT / MAP / TAP)")
try_carlquist <- subset(try_carlquist, try_carlquist$V13 != "Reference / source" &
                          try_carlquist$V13 != "Family original")
try_carlquist <- subset(try_carlquist, try_carlquist$V13 != "ID in contributed dataset" & 
                          try_carlquist$V13 != "Identifier within contributed dataset (ID)") 
try_carlquist <- subset(try_carlquist, try_carlquist$V13 != "Dataset (1)" &
                          try_carlquist$V13 != "Species name original")
try_carlquist <- subset(try_carlquist, try_carlquist$V13 != "Family" & try_carlquist$V13 != "Order" &
                          try_carlquist$V13 != "Subclass")
head(sort(table(try_carlquist$V13), decreasing = TRUE),10)
head(sort(table(try_carlquist$V26), decreasing = TRUE),10)
######Start to recover leaf phenology
#####Also load traits from try
fenology <-subset(traits_dataframe, traits_dataframe$trait_name == "whole plant vegetative phenology")
table(fenology$trait_value)
matcher <- match(cell$spe, fenology$scrubbed_species_binomial)
matcher <- fenology$trait_value[matcher]
cell$phenology <- matcher
rm(fenology)
####
try_phenology <- subset(try_carlquist, try_carlquist$V11 == "Leaf phenology type")
length(unique(try_phenology$V5))
sort(table(try_phenology$V14), decreasing = TRUE)
sort(table(try_phenology$V15), decreasing = TRUE)
#Remove species that already have phenology info
cell<-cell[!is.na(cell$phenology),]
x <- unique(cell$spe)
try_phenology <- try_phenology[ ! try_phenology$V5 %in% x, ]
#Remove duplicated values based on species, datbase and codification
try_phenology <- try_phenology[!duplicated(try_phenology[,c("V5","V14","V15")]),]
sort(table(try_phenology$V5), decreasing = TRUE)
#aDD IDENTIFIER TO COLUMNS 
spe <- as.factor(try_phenology$V5)
CountNumber <- function(x) ave(seq_along(spe), x, FUN=seq_along) 
try_phenology$spe_number_member <- CountNumber(try_phenology$V5)
sort(table(try_phenology$V15),decreasing=TRUE)
evergreen <- c("evergreen","E","EV","Y","Evergreen broad-leaved","Evergreen",
               "always persistent green","no", "Evergreen scale-like")
deciduous <- c("deciduous","D", "Deciduous broad-leaved","N","Deciduous", "W","drought-deciduous",
               "always overwintering green","semi-deciduous","yes")
matcher <- as.logical(match(try_phenology$V15, evergreen))
try_phenology$V15[matcher] <- "evergreen"
matcher <- as.logical(match(try_phenology$V15, deciduous))
try_phenology$V15[matcher] <- "deciduous"
#Removing duplicates
try_phenology <- subset(try_phenology, try_phenology$spe_number_member <=1)
sort(table(try_phenology$V15),decreasing=TRUE)
sort(table(try_phenology$V5),decreasing=TRUE)
matcher <- match(cell$spe, try_phenology$V5)
matcher <- try_phenology$V15[matcher]
cell$phenology_try <- matcher
#unite both columns in a final column and remove the other ones 
cell$phenology_curated <- with(cell, ifelse(is.na(phenology), phenology_try, phenology))

plot(log10(cell$vwt)~ log10(cell$vd),xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Vessel wall thickness ", mu, "m")))
points(log10(cell$vwt[cell$phenology_curated =="evergreen"])~ 
         log10(cell$vd[cell$phenology_curated =="evergreen"]), col="green", pch= 19)
points(log10(cell$vwt[cell$phenology_curated =="deciduous"])~ 
         log10(cell$vd[cell$phenology_curated =="deciduous"]), col="red", pch= 19)
table(cell$phenology_curated)
####determine % of species above and bellow the 90 
cell.above <- cell %>% filter(vd > 90)
table(cell.above$woodiness)
table(cell.above$phenology_curated)
cell.bellow <- cell %>% filter(vd < 90)
table(cell.bellow$woodiness)
table(cell.bellow$phenology_curated)

####Make plot of leaf traits
pdf("Figures/vdvwt_traits.pdf")
par(mfrow=c(2,1))
plot(log10(cell$vwt)~ log10(cell$vd),xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Vessel wall thickness ", mu, "m")))
points(log10(cell$vwt[cell$woodiness=="woody"])~ 
         log10(cell$vd[cell$woodiness=="woody"]), col="black", pch= 19)
points(log10(cell$vwt[cell$woodiness=="herbaceous"])~ 
         log10(cell$vd[cell$woodiness=="herbaceous"]), col="green", pch= 19)
points(log10(cell$vwt[cell$woodiness=="variable"])~ 
         log10(cell$vd[cell$woodiness=="variable"]), col="red", pch= 19)
plot(log10(cell$vwt)~ log10(cell$vd),xlab= expression(paste("log"[10], " Vessel Diameter ", mu,"m")), 
     ylab= expression(paste("log"[10], " Vessel wall thickness ", mu, "m")))
points(log10(cell$vwt[cell$phenology_curated =="evergreen"])~ 
         log10(cell$vd[cell$phenology_curated =="evergreen"]), col="green", pch= 19)
points(log10(cell$vwt[cell$phenology_curated =="deciduous"])~ 
         log10(cell$vd[cell$phenology_curated =="deciduous"]), col="red", pch= 19)
dev.off()
#####Unite traits from bien and try
#length(unique(matching.df$V5))
#length(unique(traits_dataframe$scrubbed_species_binomial))
#head(sort(table(matching.df$V11), decreasing = TRUE))
#vt <- subset(matching.df, matching.df$V13=="Vegetation type / Biome") 
#####Match for our speceis of the corners
#matcher<- match(corners$scrubbed_species_binomial, vt$V5)
#corners$vgtype <- vt$V15[matcher]
#table(corners$vgtype, corners$corners)
#######Add the cell wood anatomy variables
#matcher <- match(ocurrences_dataframe$scrubbed_species_binomial, cell$spe)
#matcher <- cbind(log10(cell$vd[matcher]),log10(cell$vwt[matcher]),
#cell$woodiness[matcher],log10(cell$ite.wt[matcher]),cell$conductive[matcher])
#colnames(matcher)<- c("LogVD","LogVWT","Woodiness", "ITE-wt","ITE-type")
#ocurrences_dataframe <- cbind(ocurrences_dataframe,matcher)
#ocurrences_dataframe$LogVD <-as.numeric(ocurrences_dataframe$LogVD)
#ocurrences_dataframe$LogVWT <-as.numeric(ocurrences_dataframe$LogVWT)
#
#matcher <- match(ocurrences_dataframe_mediandist$spe, cell$spe)
#matcher <- cbind(log10(cell$vd[matcher]),log10(cell$vwt[matcher]),
#                 cell$woodiness[matcher],log10(cell$ite.wt[matcher]),cell$conductive[matcher])
#colnames(matcher)<- c("LogVD","LogVWT","Woodiness", "ITE-wt","ITE-type")
#ocurrences_dataframe_mediandist <- cbind(ocurrences_dataframe_mediandist,matcher)
#ocurrences_dataframe_mediandist$LogVD <- as.numeric(ocurrences_dataframe_mediandist$LogVD)
#ocurrences_dataframe_mediandist$LogVWT <- as.numeric(ocurrences_dataframe_mediandist$LogVWT)
#
#matcher <- match(ocurrences_dataframe_mediandist_sp$spe, cell$spe)
#matcher <- cbind(log10(cell$vd[matcher]),log10(cell$vwt[matcher]),
#                cell$woodiness[matcher],log10(cell$ite.wt[matcher]),cell$conductive[matcher])
#colnames(matcher)<- c("LogVD","LogVWT","Woodiness", "ITE-wt","ITE-type")
#ocurrences_dataframe_mediandist_sp <- cbind(ocurrences_dataframe_mediandist_sp,matcher)

#######PCA #########
#ocurrences_df_complete <- ocurrences_dataframe_mediandist[complete.cases(ocurrences_dataframe_mediandist[,c(6,9:10,16:19,24:25)]),]
#pca.corners <- prcomp(ocurrences_df_complete[,c(6,9:10,16:19,24:25)], scale=TRUE,center=TRUE,retx=TRUE)

#summary(pca.corners)
#pca.corners$rotation #eigenvectores
#biplot(pca.corners, scale=0)
#library(factoextra)
#####
#fviz_pca_ind(pca.corners,label = "none",
#            habillage = ocurrences_df_complete$corners, 
#           palette = c("#b84c7d","#56ae6c","#7f63b8","#ac9c3d","#ba543d"),
#          addEllipses = TRUE)

#png("Figures/pca_corners.png")
#fviz_pca_ind(pca.corners,label = "none",
#            habillage = ocurrences_df_complete$conductive, 
#           palette = c("#b94c75","#75aa56"),
#          addEllipses = TRUE)
#dev.off()

####Wood density mapping ####
library(BIOMASS)
wd<-data("wdData")
str(wdData)
summary(wdData$wd)
####Use match to get wood density
wdcross <- match(cell$gen, wdData$genus)
wdcross <- wdData$wd[wdcross]
cell$WooDensity <-wdcross
summary(cell$WooDensity)
#trying to add data from the other database
wooddensity<-subset(traits_dataframe,traits_dataframe$trait_name == "stem wood density")
wooddensity$trait_value <- as.numeric(wooddensity$trait_value)
wooddensity <-aggregate(wooddensity$trait_value, by = list(species = wooddensity$scrubbed_species_binomial), mean)
####Wodd density from bien
wdcross <- match(cell$spe, wooddensity$species)
wdcross <- wooddensity$x[wdcross]
cell$WooDensityBien <- wdcross
summary(cell$WooDensityBien)
####Unite both variables and then remove them####
cell$WD <- with(cell, ifelse(is.na(cell$WooDensity), cell$WooDensityBien, cell$WooDensity))
summary(cell$WD)
cell$WooDensity <- NULL
cell$WooDensityBien <- NULL
####
plot(cell$WD ~ log10(cell$vd))
plot(cell$WD ~ log10(cell$vwt))
plot(cell$WD ~ log10(cell$dwt))
#See some generalities of our database
summary(cell$WD)
head(cell[order(cell$WD),])
head(cell[order(cell$WD, decreasing = TRUE),])
#####Use wood density for the species of the corner
#matcher <- match(ocurrences_dataframe$scrubbed_species_binomial, cell$spe)
#matcher <- cell$WD[as.numeric(matcher)]
#ocurrences_dataframe$WD <- matcher
pdf("Figures/wooddensitygroup.pdf")
boxplot(cell$WD~ cell$corner)
dev.off()
cell.prueba <- subset(cell, cell$WD > 0)
length(unique(cell.prueba$spe))
table(cell.prueba$corner)
cell.corner <- subset(cell.prueba, cell.prueba$corner != "Not from corner")
table(cell.corner$spe)
table(cell.corner$corner)
table(cell$phenology_curated,cell$corner)
wd_media_corner <- aggregate(cell$WD,
                             by=list(as.factor(cell$corner)), mean, na.rm=TRUE)
summary(wd_media_corner)
summary(cell$WD ~ cell$corner)
lm.wd.dwt <- lm(cell$WD ~ log10(cell$dwt))
summary(lm.wd.dwt)
lm.wd.tbsq <- lm(cell$WD ~ log10(cell$tbsq))
summary(lm.wd.tbsq)
pdf("Figures/woodDensity.pdf")
par(mfrow=c(2,2))    # set the plotting area into a 1*2 array
plot(cell$WD ~ log10(cell$vd))
plot(cell$WD ~ log10(cell$dwt))
plot(cell$WD ~ log10(cell$tbsqt))
plot(cell$WD ~ log10(cell$DWDratio)) #dwratio es dwt/vd
dev.off()

cell <- subset(cell, cell$ite.type !="" & cell$ite.type !="absent")
cell <- subset(cell, cell$simp.scal!="absent")
boxplot(cell$WD ~ cell$ite.type)
boxplot(cell$WD ~ cell$simp.scal)
boxplot(cell$WD ~ cell$simp.scal*cell$ite.type)

lm.vw.wd <-lm(cell$vwt ~ cell$WD+log10(cell$vd)+cell$simp.scal)
summary(lm.vw.wd)

subset(try_carlquist, try_carlquist$V14 == "WD")
table(try_carlquist$V14)

######Running a model bellow the scaling point using the residual and wodd density
lm.vwt.vd <- lm(log10(cell$vwt) ~ log10(cell$vd))
summary(lm.vwt.vd)
residuals_all <- lm.vwt.vd$residuals
plot(cell$WD ~ residuals_all)
lm.residuales<-lm(cell$WD ~ residuals_all)
summary(lm.residuales)  
###
plot(log10(cell$vwt)~log10(cell$vd))
######

####
summary(cell.temp.subseted$vg)
119/1093
#ifelse(cell$ite.type == "libriform", cell$vwt*2, cell$vwt+cell$ite.wt)
#ifelse(cell$ite.type == "tracheids",  cell$vwt*cell$ite.wt, cell$vwt*2)
cell$dwt.conditional <- ifelse(cell$vg > 4, cell$vwt*2, cell$vwt+cell$ite.wt)
cell$dwt.conditional2 <- ifelse(cell$vg > 3, cell$vwt*2, cell$vwt+cell$ite.wt)
cell$dwt.conditional3 <-  ifelse(cell$vg > 2, cell$vwt*2, cell$vwt+cell$ite.wt)
cell$dwt.conditional4 <- ifelse(cell$vg > 1.5, cell$vwt*2, cell$vwt+cell$ite.wt)

cell$dwt.conditional5 <- ifelse(cell$ite.type == "libriform", cell$vwt*2, cell$vwt+cell$ite.wt)

plot(log10(cell$vg) ~ log10(cell$vd))
cell$dwt.conditional6 <- ifelse(cell$ite.type == "tracheids",  cell$vwt+cell$ite.wt, cell$vwt*2)
par(mfrow=c(1,2))
boxplot(log10(cell$dwt.conditional) ~ cell$simp.scal* cell$conductive)
boxplot(log10(cell$dwt.conditional2) ~ cell$simp.scal* cell$conductive)
boxplot(log10(cell$dwt.conditional3) ~ cell$simp.scal* cell$conductive)
boxplot(log10(cell$dwt.conditional4) ~ cell$simp.scal* cell$conductive)

boxplot(log10(cell$ite.wt) ~ cell$simp.scal* cell$conductive)
boxplot(log10(cell$ite.wt) ~ cell$simp.scal* cell$ite.type)
prueba <-subset(cell, cell$ite.type!= "fiber tracheids")
boxplot(log10(prueba$ite.wt) ~ prueba$simp.scal* prueba$ite.type)
boxplot(log10(prueba$dwt.conditional)~ prueba$conductive)
boxplot(log10(prueba$dwt.conditional)~ prueba$simp.scal)
boxplot(log10(prueba$dwt.conditional2)~ prueba$simp.scal)
boxplot(log10(prueba$dwt.conditional3)~ prueba$simp.scal)
boxplot(log10(prueba$dwt.conditional4)~ prueba$simp.scal)

boxplot(log10(cell$dwt.conditional2)~ cell$simp.scal)
boxplot(log10(cell$dwt.conditional3)~ cell$simp.scal)
boxplot(log10(cell$dwt.conditional4)~ cell$simp.scal)
boxplot(log10(cell$dwt.conditional5)~ cell$simp.scal)
boxplot(log10(cell$dwt.conditional5)~ cell$conductive)

#
t.test(log10(prueba$dwt.conditional) ~ prueba$simp.scal)
t.test(log10(prueba$dwt.conditional) ~ prueba$conductive)

t.test(log10(cell$dwt.conditional) ~ cell$simp.scal)
t.test(log10(cell$dwt.conditional) ~ cell$conductive)

t.test(log10(prueba$dwt.conditional4) ~ prueba$simp.scal)
t.test(cell$dwt.conditional4~ cell$conductive)

boxplot(log10(prueba$dwt.conditional4)~ prueba$simp.scal)
boxplot(prueba$dwt~ prueba$simp.scal)

boxplot(log10(prueba$dwt.conditional4)~ prueba$simp.scal)
boxplot(log10(prueba$dwt.conditional4)~ prueba$conductive)

boxplot(log10(cell$dwt)~ cell$conductive)

boxplot(log10(cell$ite.wt)~ cell$conductive)
boxplot(log10(cell$ite.wt)~ cell$simp.scal)

boxplot(log10(cell$dwt.conditional) ~ cell$conductive)
boxplot(log10(cell$dwt.conditional2) ~ cell$conductive)
boxplot(log10(cell$dwt.conditional3) ~ cell$conductive)

boxplot(log10(cell$dwt.conditional) ~ cell$simp.scal)
boxplot(log10(cell$dwt.conditional2) ~ cell$simp.scal)
boxplot(log10(cell$dwt.conditional3) ~ cell$simp.scal)

t.test(log10(cell$dwt.conditional)~cell$simp.scal)
t.test(log10(cell$dwt.conditional2)~cell$simp.scal)
t.test(log10(cell$dwt.conditional3)~cell$simp.scal)

t.test(log10(cell$dwt.conditional)~cell$conductive)
t.test(log10(cell$dwt.conditional2)~cell$conductive)
t.test(log10(cell$dwt.conditional3)~cell$conductive)

boxplot(log10(cell$dwt  ) ~ cell$simp.scal* cell$conductive)

par(mfrow=c(1,1))
boxplot(log10(cell$dwt.conditional) ~ cell$simp.scal* cell$conductive)
######
lm.dwt.cond.perfo.itetype.bellow <- lm(log10(cell$dwt.conditional) ~ cell$simp.scal * cell$conductive)
summary(lm.dwt.cond.perfo.itetype.bellow)
anova(lm.dwt.cond.perfo.itetype.bellow)
confint(lm.dwt.cond.perfo.itetype.bellow)
#getting est. mean values
#getting est. mean values
emm <-emmeans(lm.dwt.cond.perfo.itetype.bellow,specs = pairwise ~ simp.scal:conductive)
emm$emmeans
emm$contrasts
multcomp::cld(emm, alpha = 0.10, Letters = LETTERS)
#s
cell<-subset(cell, cell$ite.type !="absent")
m1 <- lm(log10(cell$vwt) ~ cell$simp.scal * cell$ite.type)
m2 <- lm(log10(cell$ite.wt) ~ cell$simp.scal * cell$ite.type)
m3 <- lm(log10(cell$dwt) ~ cell$simp.scal * cell$ite.type)
emm <-emmeans(lm.dwt.cond.perfo.itetype.bellow,specs = pairwise ~ simp.scal:conductive)
emm$emmeans
emm$contrasts
multcomp::cld(emm, alpha = 0.10, Letters = LETTERS)
#
emm_1 <-emmeans(m1,specs = pairwise ~ simp.scal:ite.type)
emm_1$emmeans
emm_1$contrasts
multcomp::cld(emm_1, alpha = 0.10, Letters = LETTERS)
#
emm_2 <-emmeans(m2,specs = pairwise ~ simp.scal:ite.type)
emm_2$emmeans
emm_2$contrasts
multcomp::cld(emm_2, alpha = 0.10, Letters = LETTERS)
#
emm_3 <-emmeans(m3,specs = pairwise ~ simp.scal:ite.type)
emm_3$emmeans
emm_3$contrasts
multcomp::cld(emm_3, alpha = 0.10, Letters = LETTERS)
#
scal <- subset(cell, cell$simp.scal == "scalariform")
simple <- subset(cell, cell$simp.scal == "simple")

plot(log10(scal$vwt)~log10(scal$vel))
plot(log10(scal$vwt)~log10(scal$vd))

lm.vwt.vel <- lm(log10(scal$vwt)~log10(scal$vel))
lm.vwt.vel.simple <- lm(log10(simple$vwt)~log10(simple$vel))

summary(lm.vwt.vel)
summary(lm.vwt.vel.simple)


ggplot(cell, aes(x=(dwt.conditional), y=(dwt), label=spe))+   
  geom_point(size=1)+stat_smooth(formula= y~x, method = "lm")+
  geom_text(position = "identity", angle=25, size=2.5, alpha=0.8)
###################################3
####Select species from corners 
summary(cell$dwt.conditional4)

cell<- subset(cell, cell$self..non == "self")
cell$dwt.final <- with(cell, ifelse(is.na(cell$dwt.conditional4),
                                    cell$dwt, cell$dwt.conditional4))
summary(cell$dwt.final)
left_down <-subset(cell, cell$vd < 28 & cell$dwt.conditional4 < 4.4)
left_down$tbcond <- left_down$dwt.final/left_down$vd
left_down <- arrange(left_down, left_down$tbcond)

left_up <- subset(cell, cell$vd < 28 & cell$dwt.conditional4 > 6.6)
left_up$tbcond <- left_up$dwt.final/left_up$vd
left_up <- arrange(left_up, left_up$tbcond)

right_down <- subset(cell, cell$vd > 58 & cell$dwt.conditional4 < 4.4) 
right_down$tbcond <- right_down$dwt.final/right_down$vd
right_down <- arrange(right_down, right_down$tbcond)

right_up <- subset(cell, cell$vd > 58 & cell$dwt.conditional4 > 6.6) 
right_up$tbcond <- right_up$dwt.final/right_up$vd
right_up <- arrange(right_up, right_up$tbcond)
