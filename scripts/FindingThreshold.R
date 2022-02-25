####Script for finding threshold value for the scaling relationship between
####vessel wall thickness and vessel diameter and Figure 2

library(lm.br)
library(plotrix)

cell <- cell.temp.subseted
cell <- subset(cell, cell$vwt>0.1)
cell <- subset(cell, cell$vd>0.1)


#Preliminary visual inspection suggests a change in slope in the relationship
plot((cell$vwt)~(cell$vd), cex.lab=1.7, log="xy",  xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")))

# Broken linear regression to find the change in slope

lm.vwt.vd.br <- lm.br(log10(cell$vwt)~log10(cell$vd))
lm.vwt.vd.br

######
####Figure 2a
cell <- cell.temp.subseted
#####
##modelos arriba y abajo del umbral

###Model below threshold
cell <- subset(cell, cell$vd < 90)
lm.vwt.vd.1 <- lm(log10(cell$vwt)~log10(cell$vd))


###Model above threshold
cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 90)
lm.vwt.vd <- lm(log10(cell$vwt)~log10(cell$vd))


plot(log10(cell$vwt)~log10(cell$vd), cex.lab=1.7,  xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")))
points(log10(cell$vwt[cell$vd<10^1.95599])~log10(cell$vd[cell$vd<10^1.95599]), col="black")
points(log10(cell$vwt[cell$vd>10^1.95599])~log10(cell$vd[cell$vd>10^1.95599]), col="black", pch=19)
ablineclip(lm.vwt.vd, col="black", x1=10^1.95599, x2=3, y1=0.3, y2=1.2, lwd=2)
ablineclip(lm.vwt.vd.1, col="black", x1=0.3, x2=10^1.95599, y1=0.1, y2=2, lty=2, lwd=2)


cell <- cell.temp.subseted
plot((cell$vwt)~(cell$vd), log="xy", cex.lab=2.3, cex.axis=2,  xlab=expression(paste("Vessel Diameter ( ", mu, "m )")),  ylab=expression(paste("Vessel wall thickness ( ", mu, "m )")))
points((cell$vwt[cell$vd<10^1.95599])~(cell$vd[cell$vd<10^1.95599]), col="black")
points((cell$vwt[cell$vd>10^1.95599])~(cell$vd[cell$vd>10^1.95599]), col="black", pch=19)
ablineclip(lm.vwt.vd, col="black", x1=10^1.95599, x2=10^3, y1=10^0.3, y2=10^1.2, lwd=2)
ablineclip(lm.vwt.vd.1, col="black", x1=10^0.3, x2=10^1.95599, y1=10^0.1, y2=10^2, lty=2, lwd=2)


#Figure 2b

###Model below threshold
cell <- subset(cell, cell$vd < 90)
lm.dwt.vd.1 <- lm(log10(cell$dwt)~log10(cell$vd))


###Model above threshold
cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 90)
lm.dwt.vd <- lm(log10(cell$dwt)~log10(cell$vd))


plot(log10(cell$dwt)~log10(cell$vd), cex.lab=1.7,  xlab=expression(paste("Double Diameter ( ", mu, "m )")),  ylab=expression(paste("Double wall thickness ( ", mu, "m )")))
points(log10(cell$dwt[cell$vd<10^1.95599])~log10(cell$vd[cell$vd<10^1.95599]), col="black")
points(log10(cell$dwt[cell$vd>10^1.95599])~log10(cell$vd[cell$vd>10^1.95599]), col="black", pch=19)
ablineclip(lm.dwt.vd, col="black", x1=10^1.95599, x2=3, y1=0.3, y2=1.2, lwd=2)
ablineclip(lm.dwt.vd.1, col="black", x1=0.3, x2=10^1.95599, y1=0.1, y2=2, lty=2, lwd=2)


cell <- cell.temp.subseted
plot((cell$dwt)~(cell$vd), log="xy", cex.lab=2.3, cex.axis=2,  xlab=expression(paste("Double Diameter ( ", mu, "m )")),  ylab=expression(paste("Double wall thickness ( ", mu, "m )")))
points((cell$dwt[cell$vd<10^1.95599])~(cell$vd[cell$vd<10^1.95599]), col="black")
points((cell$dwt[cell$vd>10^1.95599])~(cell$vd[cell$vd>10^1.95599]), col="black", pch=19)
ablineclip(lm.dwt.vd, col="black", x1=10^1.95599, x2=10^3, y1=10^0.3, y2=10^1.2, lwd=2)
ablineclip(lm.dwt.vd.1, col="black", x1=10^0.3, x2=10^1.95599, y1=10^0.1, y2=10^2, lty=2, lwd=2)


#########################
##################

#load libraries
library(broom)
library(sjPlot)
library(sjmisc)
library(ggplot2)
#####Script to find the inflection point of a vwt~vd
#####Remove species that do not have vd info
cell<- cell.temp.subseted
dotchart(cell$vd)
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
hist(cell$vd)
hist(log10(cell$vd))
###Make a data frame sorted by values 
cell_sorted <- arrange(cell, vd)
cell <- cell_sorted
#
dotchart(cell$vd)
#Make plots
plot(log10(cell$vwt) ~ log10(cell$vd))
plot(cell$vwt ~ log10(cell$vd))
plot(cell$vwt ~ cell$vd)
par(mfrow=c(2,2))
hist(sort(cell$vd)[1:640])
hist(sort(cell$vd)[640:1280])
hist(sort(cell$vd)[1280:1920])
hist(sort(cell$vd)[1920:2563])
dev.off()
par(mfrow=c(2,1))
plot(log10(cell$vwt) ~ log10(cell$vd))
hist(log10(cell$vd))
dev.off()
#Make model with self and non-self supporting species
lm.vwt.vd.selfnonself <-lm(log10(cell$vwt)~ log10(cell$vd))
summary(lm.vwt.vd.selfnonself)
#with interaction
# make categorical
self.non <- to_factor(cell$self..non)
vwtlog <- log10(cell$vwt)
vdlog <- log10(cell$vd)
lm.vwt.vd.selfnonself.int <-lm(vwtlog~ vdlog * self.non)

summary(lm.vwt.vd.selfnonself.int)
plot_model(lm.vwt.vd.selfnonself.int, type = "int")

##############
####Try to sort values by vd in increasing order to plot
cell <- subset(cell, cell$vd < 50)
plot(log10(cell$vwt) ~ log10(cell$vd))
lm.vwt.vd.menos50 <- lm(log10(cell$vwt) ~ log10(cell$vd))
summary(lm.vwt.vd.menos50)

cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 50)
lm.vwt.vd.mas50 <- lm(log10(cell$vwt) ~ log10(cell$vd))
summary(lm.vwt.vd.mas50)
plot(log10(cell$vwt) ~ log10(cell$vd))
abline(lm.vwt.vd.mas50, col="red", lwd=2)

lm.vwt.vd.mas50.int <- lm(log10(cell$vwt) ~ log10(cell$vd)* cell$self..non)
summary(lm.vwt.vd.mas50.int)
points(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), col="green")
points(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]),col="blue")

cell <- cell.temp.subseted

cell <- subset(cell, cell$vd > 60)
lm.vwt.vd.mas60 <- lm(log10(cell$vwt) ~ log10(cell$vd))
summary(lm.vwt.vd.mas60)
plot(log10(cell$vwt) ~ log10(cell$vd))
abline(lm.vwt.vd.mas60, col="red", lwd=2)

lm.vwt.vd.mas60.int <- lm(log10(cell$vwt) ~ log10(cell$vd)* cell$self..non)
summary(lm.vwt.vd.mas60.int)
points(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), col="green")
points(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]),col="blue")


cell <- cell.temp.subseted
cell <- subset(cell, cell$vd > 70)

lm.vwt.vd.mas70 <- lm(log10(cell$vwt) ~ log10(cell$vd))
summary(lm.vwt.vd.mas70)
plot(log10(cell$vwt) ~ log10(cell$vd))
abline(lm.vwt.vd.mas70, col="red", lwd=2)

lm.vwt.vd.mas70.int <- lm(log10(cell$vwt) ~ log10(cell$vd)* cell$self..non)
summary(lm.vwt.vd.mas70.int)
points(log10(cell$vwt[cell$self..non=="self"])~log10(cell$vd[cell$self..non=="self"]), col="green")
points(log10(cell$vwt[cell$self..non=="non"])~log10(cell$vd[cell$self..non=="non"]),col="blue")
par(mfrow=c(2,2))
plot(lm.vwt.vd.mas70.int)

##################
##Find value of vessel diameter in which slope changes 
#create empty list to add models
models.vd <- list()
slopes <- c()
rsqr <- c()
pvalue <- c()
cell <- cell_sorted
set.seed(12)
rsqr_ave <- c()
pvalue_ave <- c()
slope_ave <- c()
x = 1
for (i in seq(1,1220,305)) {
  cell <- cell[c(i:(i+304)),]
  for (z in seq(1,50,1)) {
    sub_cell <- cell[c(sample(i:i+305, 100, replace=F)),]
    z_i <-lm(log10(sub_cell$vwt)~log10(sub_cell$vd))
    slopes <- append(slopes, z_i$coefficients[2]) 
    rsqr <- append(rsqr,summary(z_i)$r.squared)
    pvalue <- append(pvalue, glance(z_i)[[5]])
    cell <- cell_sorted
    models.vd <- c(models.vd, list(z_i))
  }
  pvalue_ave <- append(pvalue_ave, mean(pvalue[x:x+49]))
  rsqr_ave <- append(rsqr_ave, mean(rsqr[x:x+49]))
  slope_ave <- append(slope_ave, mean(slopes[x:x+49]))
  
  x = x + 50
}
summary(pvalue[1:50])
summary(pvalue[51:100])
summary(pvalue[101:150])
summary(pvalue[1:150])

summary(pvalue[151:200])
#
summary(rsqr[1:50])
summary(rsqr[51:100])
summary(rsqr[101:150])
summary(rsqr[151:200])


png("Figures/models_partition.png")
par(mfrow=c(1,3))
plot(rsqr, cex=1.7, cex.axis=2)
plot(pvalue, cex=1.7, cex.axis=2)
plot(slopes, cex=1.7, cex.axis=2)
dev.off()
#
plot(rsqr_ave)
plot(pvalue_ave)
plot(slope_ave)

####Repeat with different subsets####
models.vd.2 <- list()
slopes.2 <- c()
rsqr.2 <- c()
pvalue.2 <- c()
cell <- cell_sorted
set.seed(12)
rsqr_ave.2 <- c()
pvalue_ave.2 <- c()
slope_ave.2 <- c()
cell[1,]$vd
for (i in seq(1,1923,640)) {
  cell <- cell[c(i:(i+640)),]
  for (z in seq(1,100,1)) {
    sub_cell <- cell[c(sample(i:i+640, 100, replace=F)),]
    z_i <-lm(log10(sub_cell$vwt)~log10(sub_cell$vd))
    slopes.2 <- append(slopes.2, z_i$coefficients[2]) 
    rsqr.2 <- append(rsqr.2,summary(z_i)$r.squared)
    pvalue.2 <- append(pvalue.2, glance(z_i)[[5]])
    cell <- cell_sorted
    models.vd.2 <- c(models.vd.2, list(z_i))
  }
  pvalue_ave.2 <- append(pvalue_ave.2, mean(pvalue.2))
  rsqr_ave.2 <- append(rsqr_ave.2, mean(rsqr.2))
  slope_ave.2 <- append(slope_ave.2, mean(slopes.2))
}

par(mfrow=c(1,3))
plot(rsqr.2)
plot(pvalue.2)
plot(slopes.2)
dev.off()

plot(rsqr_ave.2)
plot(pvalue_ave)
plot(slope_ave)

#####Repeat the results with the double wall thickness
#Repeat with different subsets
cell <- subset(cell, cell$dwt > 0.1)

models.vd.3 <- list()
slopes.3 <- c()
rsqr.3 <- c()
pvalue.3 <- c()
cell <- cell_sorted
cell <- subset(cell, cell$dwt > 0.1)
set.seed(12)
rsqr_ave.3 <- c()
pvalue_ave.3 <- c()
slope_ave.3 <- c()
for (i in seq(1,1923,640)) {
  cell <- cell[c(i:(i+640)),]
  for (z in seq(1,100,1)) {
    sub_cell <- cell[c(sample(i:i+640, 100, replace=F)),]
    z_i <-lm(log10(sub_cell$dwt)~log10(sub_cell$vd))
    slopes.3 <- append(slopes.3, z_i$coefficients[2]) 
    rsqr.3 <- append(rsqr.3,summary(z_i)$r.squared)
    pvalue.3 <- append(pvalue.3, glance(z_i)[[5]])
    cell <- cell_sorted
    models.vd.3 <- c(models.vd.3, list(z_i))
  }
  pvalue_ave.3 <- append(pvalue_ave.3, mean(pvalue.3))
  rsqr_ave.3 <- append(rsqr_ave.3, mean(rsqr.3))
  slope_ave.3 <- append(slope_ave.3, mean(slopes.3))
}
plot(rsqr.3)
plot(pvalue.3)
plot(slopes.3)
