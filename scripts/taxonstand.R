#install.packages("Taxonstand") #Require package for checking species
library(Taxonstand)
data("bryophytes")
str(bryophytes)
r1 <- TPL(bryophytes$Full.name[1:20], corr = TRUE, version = "1.0")
str(r1)
#Database version from 07/03/21
#checking species
ourspecies <- paste(cell$gen, cell$sp, sep=" ")
ourspecies <- gsub("_", " ", ourspecies)
r1 <- TPL(ourspecies, corr = TRUE, version = "1.0")
warnings()
r1.subset <-subset(r1, r1$Taxonomic.status=="Synonym")
plot(table(r1$Taxonomic.status))
plot(table(r1$New.Taxonomic.status))
warnings()
write.csv(r1.subset, "meta/synonims_taxonstand.csv")
write.csv(r1,"meta/taxonstand.csv" )