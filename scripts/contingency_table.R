#####
#Preparing contingency table between ite type and perforation plate type
#
library(DescTools)
library(vcd) # useful package for graphics
#####
cell <- cell.temp.subseted
#Checking and removing variables and NAs
cell$ite.type
#
table(cell$ite.type)
table(cell$simp.scal)

#After checking the corresponding species without info. from ite.type ("" and absent)
# and perforation plate type we concluded that is correct to remove them from the analysis 
#subset absent ites and perforation plate types
cell <- subset(cell, cell$simp.scal != "absent")
cell <- subset(cell, cell$ite.type != "absent")
cell <- subset(cell, cell$ite.type != "")

#####
#Constructing the contingency table
ite.type <- as.factor(cell$ite.type)
perforation.plate <- as.factor(cell$simp.scal)
contingency_table <- addmargins(table(perforation.plate, ite.type))
contingency_table
#Building the table with xtabs (another method)
cell.table <- xtabs(~perforation.plate + ite.type, data=cell) # forms contingency table
#Tables calculated
#####
#Statistic test
ite.perfo.chi <- chisq.test(contingency_table)
#Calc with the table from xtabs
cell.xtabs.chi <- chisq.test(cell.table)
#Compared expected values
ite.perfo.chi$expected
cell.xtabs.chi$expected
#Gtest
ite.perfo.gi <- GTest(contingency_table)
#####
#Checkout the standard residuals
ite.perfo.chi$stdres
mosaic(cell.table, gp=shading_Friendly, residuals=ite.perfo.chi$stdres,
       residuals_type="Std\nresiduals", labeling=labeling_residuals)
#Put as factor ite type just considering conductive vs not conductive
cell.ite.type <- as.factor(cell$ite.type)
cell.ite.type
levels(cell.ite.type) <- c("Non-Conductive", "Non-Conductive", "Conductive")
#Build database with that clasification
conductivevsnon.table <- xtabs(~perforation.plate + cell.ite.type) # forms contingency table

conductive.non.chi <- chisq.test(conductivevsnon.table)
conductive.non.chi$stdres
conductive.non.chi$expected
conductive.non.chi$stdres
#
png("Figures/contingency_2t2table.png")
mosaic(conductivevsnon.table, gp=shading_Friendly, residuals=conductive.non.chi$stdres,
       residuals_type="Std\nresiduals", labeling=labeling_residuals)
dev.off()

cell <- cell.temp.subseted