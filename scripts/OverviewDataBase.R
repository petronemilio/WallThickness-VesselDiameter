#Load useful libraries
library(ggplot2)
library(dplyr)
#######Checkout number of orders, families, genus and species####
#Check number of orders
tableOrder <- as.data.frame(sort(table(cell$ord), decreasing = TRUE))
# Number of levels of the variable order
nlevels(as.factor(cell$ord))
# 49 orders
#PLot to see distribution of species in orders
plot(tableOrder)
#Number of families
nlevels(as.factor(cell$fam))
# 156 families
tablefam <- as.data.frame(sort(table(cell$fam), decreasing = TRUE))
#
plot(tablefam)
#Number of genera
nlevels(as.factor(cell$gen))
####In this section we figure out how many different samples we have for each species.####
#Spe column has the identifier of gen and sp.
# Count number of species seen in data frame
spp_df <- as.data.frame(sort(table(cell$spe), decreasing = TRUE))
nlevels(as.factor(cell$spe))
# Make a table of the number of counts of species in the table
spp_table <- as.data.frame(sort(table(spp_df$Freq), decreasing = TRUE))
plot(spp_table$Freq)
#Measuring relative frequencies and cumulative
spp_table$freq_counts <- prop.table(spp_table$Freq)
spp_table$cum_freq <- cumsum(spp_table$freq_counts)
#We can see that 76% of the species happens only 1 time in the data frame.
# The remaining 24% appears more than one time but we don't know if they correspond 
# to different samples

#Joining collection number to genus and spp to see number of samples.
sample <- paste0(cell$spe, " ", cell$ncol)
sample_df <- as.data.frame(sort(table(sample), decreasing = TRUE))
plot(sample_df)
# Count freq of samples from sample_df
sample_table <- as.data.frame(sort(table(sample_df$Freq), decreasing = TRUE))
plot(sample_table$Freq)
#Count freq
sample_table$freq_counts <- prop.table(sample_table$Freq)
# 96% of the samples happens only one time. The remaining 4% happens more than 1 time.
# But know we aren't considering that one species could have different samples.

#To know how many different samples based on species we have to do a table considering spp and 
# collection. Because table will count for every possible combination 
# of sample and species the table will be very long and we have to remove 0 counts
table_sample_spp <- as.data.frame(table(cell$spe, cell$ncol))
table_sample_spp <- subset(table_sample_spp, table_sample_spp$Freq != 0)

#table_sample_spp counts based on spp and sample. If we make a table from Freq1 we get 
# number of different samples for each spp.
table_spp <- as.data.frame(table(table_sample_spp$Var1))       
#We can tell from table_spp that Scaevola taccada has the biggest number of samples (9) followed 
# by Gnetum gnemon (7) and Moringa drouhardii (6)	
#Get a table with the frequency of samples by species
last_table <- as.data.frame(sort(table(table_spp$Freq), decreasing = TRUE))
#Get the frequencies
last_table$freq_counts <- prop.table(last_table$Freq)
print(last_table)
#79% of the spp had only one sample. 21% of the spp has more than one sample.

###############################################################################
######## A brief summary and plots of variables of the data base
summary(cell)
#Check vessel diameter
boxplot(cell$vd)
dotchart(cell$vd)
hist(cell$vd)
#
summary(cell$vd) #6 A 461
#Hacer log para ver distribución de los datos
boxplot(log10(cell$vd))
dotchart(log10(cell$vd))
hist(log10(cell$vd))
#Vessel wall thickness
summary(cell$vwt) # mean = 2.9 ##2.7
sd(cell$vwt, na.rm = TRUE) #sd = 1.36
boxplot(cell$vwt)
hist(cell$vwt)
dotchart(cell$vwt)
#Graficar en log
boxplot(log10(cell$vwt))
hist(log10(cell$vwt))
dotchart(log10(cell$vwt))

#ITE wall thickness
summary(cell$ite.wt) #Mean = 3.7
sd(cell$ite.wt, na.rm = TRUE) #sd = 1.537
boxplot(cell$ite.wt)
hist(cell$ite.wt)
dotchart(cell$ite.wt)

boxplot(log10(cell$ite.wt))
hist(log10(cell$ite.wt))
dotchart(log10(cell$ite.wt))
#Itetype
cell <- cell.temp.subseted
cell <- subset(cell, cell$ite.type != "")
cell <- subset(cell, cell$ite.type != "absent")

cell_prueba <- cell
table(cell_prueba$ite.type)

spe <- as.factor(cell_prueba$spe)
CountNumber <- function(x) ave(seq_along(spe), x, FUN=seq_along) 
cell_prueba$spe_number_member <- CountNumber(cell_prueba$spe)

cell_subset <- subset(cell_prueba, cell_prueba$spe_number_member <=1)
table(cell_prueba$ite.type)
table(cell_subset$ite.type)
376+503+325
284+423+134
284/(284+423+134)
423/(284+423+134)
134/(284+423+134)

cell <- cell.temp.subseted 

cell <- subset(cell, cell$vd > 88.1 )
table(cell$self..non)