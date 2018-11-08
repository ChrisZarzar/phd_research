
##Author: Christopher Zarzar
##Created: 7-April-2017

##NOTES: This script creates box plots of data

set.seed(7)


## Read in the data
augbv.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015BV25_3class.csv", header = TRUE)
augrad.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Rad25_3class.csv", header = TRUE)
augref.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Ref25_3class.csv", header = TRUE)

## Reclassify BV columns
augbv.data$NewClass <- NA
augbv.data$NewClass[augbv.data$Class == "Land"] <- 2
augbv.data$NewClass[augbv.data$Class == "Water"] <- 1
augbv.data$NewClass[augbv.data$Class == "Aquatics"] <- 3

AquaBV.samp <- augbv.data[sample(augbv.data$Class == "Aquatics") , ]
LandBV.samp <- augbv.data[sample(augbv.data$Class == "Land") , ]
WatBV.samp <- augbv.data[sample(augbv.data$Class == "Water") , ]
augbv.samp <- rbind(WatBV.samp,AquaBV.samp, LandBV.samp)

## Reclassify Rad columns
augrad.data$NewClass <- NA
augrad.data$NewClass[augrad.data$Class == "Land"] <- 2
augrad.data$NewClass[augrad.data$Class == "Water"] <- 1
augrad.data$NewClass[augrad.data$Class == "Aquatics"] <- 3

AquaRad.samp <- augrad.data[sample(augrad.data$Class == "Aquatics") , ]
LandRad.samp <- augrad.data[sample(augrad.data$Class == "Land") , ]
WatRad.samp <- augrad.data[sample(augrad.data$Class == "Water") , ]
augrad.samp <- rbind(WatRad.samp,AquaRad.samp, LandRad.samp)

## Reclassify Ref columns
augref.data$NewClass <- NA
augref.data$NewClass[augref.data$Class == "Land"] <- 2
augref.data$NewClass[augref.data$Class == "Water"] <- 1
augref.data$NewClass[augref.data$Class == "Aquatics"] <- 3

AquaRef.samp <- augref.data[sample(augref.data$Class == "Aquatics") , ]
LandRef.samp <- augref.data[sample(augref.data$Class == "Land") , ]
WatRef.samp <- augref.data[sample(augref.data$Class == "Water") , ]
augref.samp <- rbind(WatRef.samp, AquaRef.samp, LandRef.samp)

sampBVs <- cbind(WatBV.samp[5], AquaBV.samp[5], LandBV.samp[5],WatBV.samp[6], AquaBV.samp[6], LandBV.samp[6],WatBV.samp[7], AquaBV.samp[7], LandBV.samp[7])
sampRads <- cbind(WatRad.samp[5], AquaRad.samp[5], LandRad.samp[5],WatRad.samp[6], AquaRad.samp[6], LandRad.samp[6],WatRad.samp[7], AquaRad.samp[7], LandRad.samp[7])
sampRefs <- cbind(WatBV.samp[5], AquaRef.samp[5], LandRef.samp[5],WatRef.samp[6], AquaRef.samp[6], LandRef.samp[6],WatRef.samp[7], AquaRef.samp[7], LandRef.samp[7])

boxplot(sampBVs, use.col=TRUE, at =c(1,2,3, 5,6,7, 9,10,11), names=c("","","", "", "Red", "","", "NIR", ""), col=c('green4','green4','green4','red','red','red','mistyrose3','mistyrose3','mistyrose3'))
mtext("Feature Spectral Response", side=3, line=1.5, cex=2)
mtext("Brightness Value", side=2, line=2.5, cex=1.5)

dev.off()

