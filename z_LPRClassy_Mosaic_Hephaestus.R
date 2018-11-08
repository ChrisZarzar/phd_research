##Author: Christopher Zarzar
##Created: 3-Mar-2017
##NOTES: This script classifies LPR samples from August 2015 imagery using the top to 
##classification techniques I determined from previous work. It then outputs and assess the 
##user and producer errors for comparison. 
##EDIT: 4-Mar-2017 Chris Zarzar: Added write.csv to write out the accuracyMatrix. 
##EDIT: 9-Mar-2017 Chris Zarzar: Removed MLC from classification. Would not work with technique used to iterate. 
##EDIT: 9-Mar-2017 Chris Zarzar: Shortened all paths by setting global variables

## load library requirements
library("rasclass")
library("rgdal")
library("raster")



## Set up global variables
mainPath <- "/home/chriszarzar/R/RWorkspace/"
inRasBV <- "/home/chriszarzar/R/resampledCIR_25_Mosaics/aug2015BV25.tif"
inRasRad <- "/home/chriszarzar/R/resampledCIR_25_Mosaics/aug2015Rad25.tif"
inRasRef <- "/home/chriszarzar/R/resampledCIR_25_Mosaics/aug2015Ref25.tif"

## Read in the ground reference data
augbv.data <- read.csv(paste(mainPath,'/datasets/aug2015BV25_3class.csv', sep = ''), header = TRUE)
augrad.data <- read.csv(paste(mainPath,'/datasets/aug2015Rad25_3class.csv', sep = ''), header = TRUE)
augref.data <- read.csv(paste(mainPath,'/datasets/aug2015Ref25_3class.csv', sep = ''), header = TRUE)

## Make sure required directories exist 
dir.create(paste(mainPath,'lpr/lpr_classification_output/bv_3class/',sep=''), showWarnings = FALSE)
dir.create(paste(mainPath,'lpr/lpr_classification_output/outimgs/',sep=''), showWarnings = FALSE)
dir.create(paste(mainPath,'lpr/lpr_classification_output/outRasters/',sep=''), showWarnings = FALSE)

## Set paths
outClassPath <- paste(mainPath,'lpr/lpr_classification_output/bv_3class/',sep='')
outImgPath <- paste(mainPath,'lpr/lpr_classification_output/outimgs/',sep='') 
outRasPath <- paste(mainPath,'lpr/lpr_classification_output/outRasters/',sep='') 


## Redirect output
sink.cmd <- paste('sink("',outClassPath,'classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

## Reclassify columns
augbv.data$NewClass <- NA
augbv.data$NewClass[augbv.data$Class == "Land"] <- 2
augbv.data$NewClass[augbv.data$Class == "Water"] <- 1
augbv.data$NewClass[augbv.data$Class == "Aquatics"] <- 3

#Set up the number of samples and classification techniques to loop through
classMethod <- c('logit')#,'neuralNetwork','randomForest','supportVector') #couldn't predict with maximumLikelihood 
nSamp <- 260

for (k in 1:length(classMethod)){
  set.seed(7)
  ## Randomly select nSamp samples from each class
  print(paste("Randomly sampling tables for ", nSamp, " samples"))
  Aqua.samp <- augbv.data[sample(which(augbv.data$Class == "Aquatics") , nSamp) , ]
  Land.samp <- augbv.data[sample(which(augbv.data$Class == "Land") , nSamp) , ]
  Wat.samp <- augbv.data[sample(which(augbv.data$Class == "Water") , nSamp) , ]
  augbv.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
  
  ## Create raster stack object for full mosaic my reading in an ESRI TIFF Raster
  ras.band1 <- raster(inRasBV, band=1)
  ras.band2 <- raster(inRasBV, band=2)
  ras.band3 <- raster(inRasBV, band=3)
  ras.stack <- stack(ras.band1, ras.band2, ras.band3)
  outPut.cmd <- paste('png(file="',outImgPath,'BV3_RGNIR.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotRGB(ras.stack, main="False Color Image (Brightness Values)", r=3, g=2, b=1)
  dev.off()
  
  ## Rename to columns that will be used to build the formula so they match with the raster
  names(augbv.samp)[5]<- names(ras.stack[[1]])
  names(augbv.samp)[6]<- names(ras.stack[[2]])
  names(augbv.samp)[7]<- names(ras.stack[[3]])
  
  ## Assign the columns to the respective values
  samples <- augbv.samp[8]
  band1Val <- augbv.samp[5] 
  band2Val <- augbv.samp[6]
  band3Val <- augbv.samp[7]
  
  ## Assign only those randomly selected data I need to a new data frame
  newdata <- data.frame(samples, band1Val, band2Val, band3Val)
  
  ## create a raster based on newdata
  print("Creating new raster dataset from samples")
  object <- new('rasclass')
  object <- setRasclassData(newdata, ncols = 3*nSamp, nrows = 1,
                            xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                            samplename = 'NewClass')
  print("Summary of new raster")
  print(summary(object))
  

   ## Output all returned values to text file
  print(paste("Classifying for class method " , classMethod[k], " with ", nSamp, " samples"))
  
  ## Classify using multiple algorithms
  outlist <- list()
  outlist[[classMethod[k]]] <- classifyRasclass(object, splitfraction = .7, method =classMethod[k])
  kappa <- outlist[[classMethod[k]]]@kappa
  overallAccuracy <- outlist[[classMethod[k]]]@overallAccuracy
  accuracyMatrix <- outlist[[classMethod[k]]]@accuracyMatrix
  write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste(outClassPath,classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste(outClassPath,classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write.csv(accuracyMatrix, file =paste(outClassPath,classMethod[k],nSamp,'matrix.csv', sep=''))
  if (classMethod[k] == "maximumLikelihood"){
    y.hat <- predict(ras.stack, outlist$maximumLikelihood@maximumLikelihood)
  }
  if (classMethod[k] == "logit"){
    y.hat <- predict(ras.stack, outlist$logit@logit)
  }
  if (classMethod[k] == "neuralNetwork"){
    y.hat <- predict(ras.stack, outlist$neuralNetwork@neuralNetwork)
  }
  if (classMethod[k] == "randomForest"){
    y.hat <- predict(ras.stack, outlist$randomForest@randomForest)
  }
  if (classMethod[k] == "supportVector"){
    y.hat <- predict(ras.stack, outlist$supportVector@supportVector)
  }
  outPut.cmd <- paste('png(file="',outImgPath,classMethod[k],'_BV3_class.png")',sep='')
  eval(parse(text=outPut.cmd))
  breakpoints <- c(0,1,2,3)
  colors <- c("royalblue","forestgreen","seagreen1","cyan3")
  plot(y.hat, breaks=breakpoints, col=colors, main=paste(classMethod[k], " Classification", sep=""), axes=FALSE)
  dev.off()
  raster::writeRaster(y.hat, filename=paste(outRasPath,classMethod[k],'_BV3_class.tif',sep=''),format="GTiff", overwrite=TRUE)
}
closeAllConnections()





#### ***START RADIANCE CLASSIFICATIONS*** ####

## Make sure required directories exist 
dir.create(paste(mainPath,'lpr/lpr_classification_output/rad_3class/',sep=''), showWarnings = FALSE)
dir.create(paste(mainPath,'lpr/lpr_classification_output/outimgs/',sep=''), showWarnings = FALSE)
dir.create(paste(mainPath,'lpr/lpr_classification_output/outRasters/',sep=''), showWarnings = FALSE)

## Set paths
outClassPath <- paste(mainPath,'lpr/lpr_classification_output/rad_3class/',sep='')
outImgPath <- paste(mainPath,'lpr/lpr_classification_output/outimgs/',sep='') 
outRasPath <- paste(mainPath,'lpr/lpr_classification_output/outRasters/',sep='') 


## Redirect output
sink.cmd <- paste('sink("',outClassPath,'classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

## Reclassify columns
augrad.data$NewClass <- NA
augrad.data$NewClass[augrad.data$Class == "Land"] <- 2
augrad.data$NewClass[augrad.data$Class == "Water"] <- 1
augrad.data$NewClass[augrad.data$Class == "Aquatics"] <- 3

#Set up the number of samples and classification techniques to loop through
classMethod <- c('logit')#,'neuralNetwork','randomForest','supportVector') #couldn't predict with maximumLikelihood 
nSamp <- 260

for (k in 1:length(classMethod)){
  set.seed(7)
  ## Randomly select nSamp samples from each class
  print(paste("Randomly sampling tables for ", nSamp, " samples"))
  Aqua.samp <- augrad.data[sample(which(augrad.data$Class == "Aquatics") , nSamp) , ]
  Land.samp <- augrad.data[sample(which(augrad.data$Class == "Land") , nSamp) , ]
  Wat.samp <- augrad.data[sample(which(augrad.data$Class == "Water") , nSamp) , ]
  augrad.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
  
  ## Create raster stack object for full mosaic my reading in an ESRI TIFF Raster
  ras.band1 <- raster(inRasRad, band=1)
  ras.band2 <- raster(inRasRad, band=2)
  ras.band3 <- raster(inRasRad, band=3)
  ras.stack <- stack(ras.band1, ras.band2, ras.band3)
  outPut.cmd <- paste('png(file="',outImgPath,'Rad3_RGNIR.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotRGB(ras.stack, main="False Color Image (Radiance)", r=3, g=2, b=1)
  dev.off()
  
  ## Rename to columns that will be used to build the formula so they match with the raster
  names(augrad.samp)[5]<- names(ras.stack[[1]])
  names(augrad.samp)[6]<- names(ras.stack[[2]])
  names(augrad.samp)[7]<- names(ras.stack[[3]])
  
  ## Assign the columns to the respective values
  samples <- augrad.samp[8]
  band1Val <- augrad.samp[5] 
  band2Val <- augrad.samp[6]
  band3Val <- augrad.samp[7]
  
  ## Assign only those randomly selected data I need to a new data frame
  newdata <- data.frame(samples, band1Val, band2Val, band3Val)
  
  ## create a raster based on newdata
  print("Creating new raster dataset from samples")
  object <- new('rasclass')
  object <- setRasclassData(newdata, ncols = 3*nSamp, nrows = 1,
                            xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                            samplename = 'NewClass')
  print("Summary of new raster")
  print(summary(object))
  
  
  ## Output all returned values to text file
  print(paste("Classifying for class method " , classMethod[k], " with ", nSamp, " samples"))
  
  ## Classify using multiple algorithms
  outlist <- list()
  outlist[[classMethod[k]]] <- classifyRasclass(object, splitfraction = .7, method =classMethod[k])
  kappa <- outlist[[classMethod[k]]]@kappa
  overallAccuracy <- outlist[[classMethod[k]]]@overallAccuracy
  accuracyMatrix <- outlist[[classMethod[k]]]@accuracyMatrix
  write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste(outClassPath,classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste(outClassPath,classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write.csv(accuracyMatrix, file =paste(outClassPath,classMethod[k],nSamp,'matrix.csv', sep=''))
  if (classMethod[k] == "maximumLikelihood"){
    y.hat <- predict(ras.stack, outlist$maximumLikelihood@maximumLikelihood)
  }
  if (classMethod[k] == "logit"){
    y.hat <- predict(ras.stack, outlist$logit@logit)
  }
  if (classMethod[k] == "neuralNetwork"){
    y.hat <- predict(ras.stack, outlist$neuralNetwork@neuralNetwork)
  }
  if (classMethod[k] == "randomForest"){
    y.hat <- predict(ras.stack, outlist$randomForest@randomForest)
  }
  if (classMethod[k] == "supportVector"){
    y.hat <- predict(ras.stack, outlist$supportVector@supportVector)
  }
  outPut.cmd <- paste('png(file="',outImgPath,classMethod[k],'_Rad3_class.png")',sep='')
  eval(parse(text=outPut.cmd))
  breakpoints <- c(0,1,2,3)
  colors <- c("royalblue","forestgreen","seagreen1","cyan3")
  plot(y.hat, breaks=breakpoints, col=colors, main=paste(classMethod[k], " Classification", sep=""), axes=FALSE)
  dev.off()
  raster::writeRaster(y.hat, filename=paste(outRasPath,classMethod[k],'_Rad3_class.tif',sep=''),format="GTiff", overwrite=TRUE)
}
closeAllConnections()





#### ***START REFLECTANCE CLASSIFICATIONS*** ####

## Make sure required directories exist 
dir.create(paste(mainPath,'lpr/lpr_classification_output/ref_3class/',sep=''), showWarnings = FALSE)
dir.create(paste(mainPath,'lpr/lpr_classification_output/outimgs/',sep=''), showWarnings = FALSE)
dir.create(paste(mainPath,'lpr/lpr_classification_output/outRasters/',sep=''), showWarnings = FALSE)

## Set paths
outClassPath <- paste(mainPath,'lpr/lpr_classification_output/ref_3class/',sep='')
outImgPath <- paste(mainPath,'lpr/lpr_classification_output/outimgs/',sep='') 
outRasPath <- paste(mainPath,'lpr/lpr_classification_output/outRasters/',sep='') 


## Redirect output
sink.cmd <- paste('sink("',outClassPath,'classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

## Reclassify columns
augref.data$NewClass <- NA
augref.data$NewClass[augref.data$Class == "Land"] <- 2
augref.data$NewClass[augref.data$Class == "Water"] <- 1
augref.data$NewClass[augref.data$Class == "Aquatics"] <- 3

#Set up the number of samples and classification techniques to loop through
classMethod <- c('logit')#,'neuralNetwork','randomForest','supportVector') #couldn't predict with maximumLikelihood 
nSamp <- 260

for (k in 1:length(classMethod)){
  set.seed(7)
  ## Randomly select nSamp samples from each class
  print(paste("Randomly sampling tables for ", nSamp, " samples"))
  Aqua.samp <- augref.data[sample(which(augref.data$Class == "Aquatics") , nSamp) , ]
  Land.samp <- augref.data[sample(which(augref.data$Class == "Land") , nSamp) , ]
  Wat.samp <- augref.data[sample(which(augref.data$Class == "Water") , nSamp) , ]
  augref.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
  
  ## Create raster stack object for full mosaic my reading in an ESRI TIFF Raster
  ras.band1 <- raster(inRasRef, band=1)
  ras.band2 <- raster(inRasRef, band=2)
  ras.band3 <- raster(inRasRef, band=3)
  ras.stack <- stack(ras.band1, ras.band2, ras.band3)
  outPut.cmd <- paste('png(file="',outImgPath,'Ref3_RGNIR.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotRGB(ras.stack, main="False Color Image (Reflectance)", r=3, g=2, b=1)
  dev.off()
  
  ## Rename to columns that will be used to build the formula so they match with the raster
  names(augref.samp)[5]<- names(ras.stack[[1]])
  names(augref.samp)[6]<- names(ras.stack[[2]])
  names(augref.samp)[7]<- names(ras.stack[[3]])
  
  ## Assign the columns to the respective values
  samples <- augref.samp[8]
  band1Val <- augref.samp[5] 
  band2Val <- augref.samp[6]
  band3Val <- augref.samp[7]
  
  ## Assign only those randomly selected data I need to a new data frame
  newdata <- data.frame(samples, band1Val, band2Val, band3Val)
  
  ## create a raster based on newdata
  print("Creating new raster dataset from samples")
  object <- new('rasclass')
  object <- setRasclassData(newdata, ncols = 3*nSamp, nrows = 1,
                            xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                            samplename = 'NewClass')
  print("Summary of new raster")
  print(summary(object))
  
  
  ## Output all returned values to text file
  print(paste("Classifying for class method " , classMethod[k], " with ", nSamp, " samples"))
  
  ## Classify using multiple algorithms
  outlist <- list()
  outlist[[classMethod[k]]] <- classifyRasclass(object, splitfraction = .7, method =classMethod[k])
  kappa <- outlist[[classMethod[k]]]@kappa
  overallAccuracy <- outlist[[classMethod[k]]]@overallAccuracy
  accuracyMatrix <- outlist[[classMethod[k]]]@accuracyMatrix
  write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste(outClassPath,classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste(outClassPath,classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write.csv(accuracyMatrix, file =paste(outClassPath,classMethod[k],nSamp,'matrix.csv', sep=''))
  if (classMethod[k] == "maximumLikelihood"){
    y.hat <- predict(ras.stack, outlist$maximumLikelihood@maximumLikelihood)
  }
  if (classMethod[k] == "logit"){
    y.hat <- predict(ras.stack, outlist$logit@logit)
  }
  if (classMethod[k] == "neuralNetwork"){
    y.hat <- predict(ras.stack, outlist$neuralNetwork@neuralNetwork)
  }
  if (classMethod[k] == "randomForest"){
    y.hat <- predict(ras.stack, outlist$randomForest@randomForest)
  }
  if (classMethod[k] == "supportVector"){
    y.hat <- predict(ras.stack, outlist$supportVector@supportVector)
  }
  outPut.cmd <- paste('png(file="',outImgPath,classMethod[k],'_Ref3_class.png")',sep='')
  eval(parse(text=outPut.cmd))
  breakpoints <- c(0,1,2,3)
  colors <- c("royalblue","forestgreen","seagreen1","cyan3")
  plot(y.hat, breaks=breakpoints, col=colors, main=paste(classMethod[k], " Classification", sep=""), axes=FALSE)
  dev.off()
  raster::writeRaster(y.hat, filename=paste(outRasPath,classMethod[k],'_Ref3_class.tif',sep=''),format="GTiff", overwrite=TRUE)
}
closeAllConnections()

## -------------------------------------------------
print ("COMPLETE")




##  END  ##