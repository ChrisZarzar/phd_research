
##Author: Christopher Zarzar
##Created: 7-April-2017

##NOTES: This script creates box plots of data


library("AppliedPredictiveModeling")
library("caret")

set.seed(7)


## Read in the data
augbv.data <- read.csv("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/datasets/aug2015BV25_3class.csv", header = TRUE)
augrad.data <- read.csv("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/datasets/aug2015Rad25_3class.csv", header = TRUE)
augref.data <- read.csv("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/datasets/aug2015Ref25_3class.csv", header = TRUE)


## Reclassify BV columns
augbv.data$NewClass <- NA
augbv.data$NewClass[augbv.data$Class == "Land"] <- "TV"
augbv.data$NewClass[augbv.data$Class == "Water"] <- "OW"
augbv.data$NewClass[augbv.data$Class == "Aquatics"] <- "AV"

AquaBV.samp <- augbv.data[which(augbv.data$Class == "Aquatics") , ]
LandBV.samp <- augbv.data[which(augbv.data$Class == "Land") , ]
WatBV.samp <- augbv.data[which(augbv.data$Class == "Water") , ]
augbv.samp <- rbind(WatBV.samp,AquaBV.samp, LandBV.samp)

## Reclassify Rad columns
augrad.data$NewClass <- NA
augrad.data$NewClass[augrad.data$Class == "Land"] <- "TV"
augrad.data$NewClass[augrad.data$Class == "Water"] <- "OW"
augrad.data$NewClass[augrad.data$Class == "Aquatics"] <- "AV"

AquaRad.samp <- augrad.data[which(augrad.data$Class == "Aquatics") , ]
LandRad.samp <- augrad.data[which(augrad.data$Class == "Land") , ]
WatRad.samp <- augrad.data[which(augrad.data$Class == "Water") , ]
augrad.samp <- rbind(WatRad.samp,AquaRad.samp, LandRad.samp)

## Reclassify Ref columns
augref.data$NewClass <- NA
augref.data$NewClass[augref.data$Class == "Land"] <- "TV"
augref.data$NewClass[augref.data$Class == "Water"] <- "OW"
augref.data$NewClass[augref.data$Class == "Aquatics"] <- "AV"


AquaRef.samp <- augref.data[which(augref.data$Class == "Aquatics") , ]
LandRef.samp <- augref.data[which(augref.data$Class == "Land") , ]
WatRef.samp <- augref.data[which(augref.data$Class == "Water") , ]
augref.samp <- rbind(WatRef.samp, AquaRef.samp, LandRef.samp)

#Rename the band and NewClass column

names(augbv.data)[5:7]<- c("A Green A", "B Red B", "C NIR C")
names(augrad.data)[5:7]<- c("A Green A", "B Red B", "C NIR C")
names(augref.data)[5:7]<- c("A Green A", "B Red B", "C NIR c")


## Create Spectra plots of median band value for features (WORK IN PROGRESS)
MedsampBVs <- cbind(median(WatBV.samp[,5]), median(AquaBV.samp[,5]), median(LandBV.samp[,5]),median(WatBV.samp[,6]), median(AquaBV.samp[,6]), median(LandBV.samp[,6]),median(WatBV.samp[,7]), median(AquaBV.samp[,7]), median(LandBV.samp[,7]))


## Create Box Plots

sampBVs <- cbind(WatBV.samp[5], AquaBV.samp[5], LandBV.samp[5],WatBV.samp[6], AquaBV.samp[6], LandBV.samp[6],WatBV.samp[7], AquaBV.samp[7], LandBV.samp[7])
sampRads <- cbind(WatRad.samp[5], AquaRad.samp[5], LandRad.samp[5],WatRad.samp[6], AquaRad.samp[6], LandRad.samp[6],WatRad.samp[7], AquaRad.samp[7], LandRad.samp[7])
sampRefs <- cbind(WatRef.samp[5], AquaRef.samp[5], LandRef.samp[5],WatRef.samp[6], AquaRef.samp[6], LandRef.samp[6],WatRef.samp[7], AquaRef.samp[7], LandRef.samp[7])

png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Class_BV_BoxPlot.png", width=700, height=400)
boxplot(sampBVs, use.col=TRUE, at =c(1,2,3, 5,6,7, 9,10,11), names=c("OW","AV","TV", "OW", "AV", "TV","OW", "AV", "TV"), col=c('green4','green4','green4','red','red','red','mistyrose3','mistyrose3','mistyrose3'))
op <- par(mar = c(5,5,4,2) + 0.2) ## default is c(5,4,4,2) + 0.1
mtext("Class", side=1, line=3, cex=1.5)
mtext("Brightness Values", side=2.75, line=3.25, cex=1.5)
mtext("Green", side=3, adj=.12, line=.75, cex=1.5)
mtext("Red", side=3, adj=.5, line=.75, cex=1.5)
mtext("NIR", side=3, adj=.86, line=.75, cex=1.5)
par(op)
dev.off()

png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Class_Rad_BoxPlot.png", width=700, height=400)
boxplot(sampRads, use.col=TRUE, at =c(1,2,3, 5,6,7, 9,10,11), names=c("OW","AV","TV", "OW", "AV", "TV","OW", "AV", "TV"), col=c('green4','green4','green4','red','red','red','mistyrose3','mistyrose3','mistyrose3'))
op <- par(mar = c(5,5,4,2) + 0.2) ## default is c(5,4,4,2) + 0.1
mtext("Class", side=1, line=2.75, cex=1.5)
mtext(expression(paste(plain("Radiance (W sr") ^ plain("-1"), plain(" m") ^ plain("-2"), plain(")"))), side=2, line=3.25, cex=1.5)
mtext("Green", side=3, adj=.12, line=.75, cex=1.5)
mtext("Red", side=3, adj=.5, line=.75, cex=1.5)
mtext("NIR", side=3, adj=.86, line=.75, cex=1.5)
par(op)
dev.off()

png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Class_Ref_BoxPlot.png", width=700, height=400)
boxplot(sampRefs, use.col=TRUE, at =c(1,2,3, 5,6,7, 9,10,11), names=c("OW","AV","TV", "OW", "AV", "TV","OW", "AV", "TV"), col=c('green4','green4','green4','red','red','red','mistyrose3','mistyrose3','mistyrose3'))
op <- par(mar = c(5,5,4,2) + 0.2) ## default is c(5,4,4,2) + 0.1
mtext("Class", side=1, line=2.75, cex=1.5)
mtext("Reflectance", side=2, line=3.25, cex=1.5)
mtext("Green", side=3, adj=.12, line=.75, cex=1.5)
mtext("Red", side=3, adj=.5, line=.75, cex=1.5)
mtext("NIR", side=3, adj=.86, line=.75, cex=1.5)
par(op)
dev.off()





## Multipanel Scatter Plot
transparentTheme(trans = .4)
featurePlot(x = augbv.data[, 5:7],
            y = augbv.data$Class,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))
dev.off()


## Multipanel Density Plots
png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Class_BV_Density.png", width=700, height=400)
transparentTheme(trans = .9)
featurePlot(x = augbv.data[5:7], 
            y = factor(augbv.data$NewClass),
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(3, 1), 
            auto.key = list(columns = 3),
            labels = c("Brightness Values", "Density"))
dev.off()

png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Class_Rad_Density.png", width=700, height=400)
transparentTheme(trans = .9)
featurePlot(x = augrad.data[5:7], 
            y = factor(augrad.data$NewClass),
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(3, 1), 
            auto.key = list(columns = 3),
            labels = c("Radiance", "Density"))
dev.off()

png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Class_Ref_Density.png", width=700, height=400)
transparentTheme(trans = .9)
featurePlot(x = augref.data[5:7], 
            y = factor(augref.data$NewClass),
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(3, 1), 
            auto.key = list(columns = 3),
            labels = c("Reflectance", "Density"))
dev.off()

closeAllConnections()

## END  ##