##Author: Christopher Zarzar
##Created: 3-Mar-2017
##NOTES: This script classifies LPR samples from August 2015 imagery using the top to 
##classification techniques I determined from previous work. It then outputs and assess the 
##user and producer errors for comparison. 
##EDIT: 4-Mar-2017 Chris Zarzar: Added write.csv to write out the accuracyMatrix. 
##EDIT: 9-Mar-2017 Chris Zarzar: Removed MLC from classification. Would not work with technique used to iterate. 
##EDIT: 9-Mar-2017 Chris Zarzar: Shortened all paths by setting global variables
##EDIT: 9-Mar-2017 Chris Zarzar: Added run time elements to the loops
##EDIT: 12-Mar-2017 Chris Zarzar: Added a primary vector to loop through all dates. Adjusted variable accordingly


## load library requirements
library("rasclass")
library("rgdal")
library("raster")
library("tools")

## Set up primary vector to loop through all of the dates available
loop.files <- c('aug2015') #'dec2014','dec2015','mar2015','may2015 completed
for (d in 1:length(loop.files)){
  date = loop.files[d]
  var.name = file_path_sans_ext(date)
  date.name = substr (date,1,7)
  print (date)
  
  ## Set up global variables
  mainPath <- "E:/RWorkspace/"
  inRasBV <- paste("E:/CIR_UAS_Imagery/resampledCIR_25_Mosaics/",date.name,"BV25.tif", sep="")
  inRasRad <- paste("E:/CIR_UAS_Imagery/resampledCIR_25_Mosaics/",date.name,"Rad25.tif", sep="")
  inRasRef <- paste("E:/CIR_UAS_Imagery/resampledCIR_25_Mosaics/",date.name,"Ref25.tif", sep="")
  
  ## Read in the ground reference data
  inbv.data <- read.csv(paste(mainPath,'datasets/lprLandWaterAquatics/',date.name,'BV25.csv', sep = ''), header = TRUE)
  inrad.data <- read.csv(paste(mainPath,'datasets/lprLandWaterAquatics/',date.name,'Rad25.csv', sep = ''), header = TRUE)
  inref.data <- read.csv(paste(mainPath,'datasets/lprLandWaterAquatics/',date.name,'Ref25.csv', sep = ''), header = TRUE)
  
  ## Make sure required directories exist 
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name, sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/bv_3class/',sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outRasters/',sep=''), showWarnings = FALSE)
  
  ## Set paths
  outClassPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/bv_3class/',sep='')
  outImgPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep='') 
  outRasPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outRasters/',sep='') 
  
  
  ## Redirect output
  sink.cmd <- paste('sink("',outClassPath,'classificationOutput.txt")',sep='')
  eval(parse(text=sink.cmd))
  
  ## Reclassify columns
  inbv.data$NewClass <- NA
  inbv.data$NewClass[inbv.data$Class == "Land"] <- 2
  inbv.data$NewClass[inbv.data$Class == "Water"] <- 1
  inbv.data$NewClass[inbv.data$Class == "Aquatics"] <- 3
  
  #Set up the number of samples and classification techniques to loop through
  classMethod <- c('randomForest','neuralNetwork','supportVector') #'logit'  couldn't get predict to work with maximumLikelihood
  nSamp <- 260
  
  for (k in 1:length(classMethod)){
    start.time <- Sys.time()
    set.seed(7)
    ## Randomly select nSamp samples from each class
    print(paste("Randomly sampling tables for ", nSamp, " samples"))
    Aqua.samp <- inbv.data[sample(which(inbv.data$Class == "Aquatics") , nSamp) , ]
    Land.samp <- inbv.data[sample(which(inbv.data$Class == "Land") , nSamp) , ]
    Wat.samp <- inbv.data[sample(which(inbv.data$Class == "Water") , nSamp) , ]
    inbv.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
    
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
    names(inbv.samp)[5]<- names(ras.stack[[1]])
    names(inbv.samp)[6]<- names(ras.stack[[2]])
    names(inbv.samp)[7]<- names(ras.stack[[3]])
    
    ## Assign the columns to the respective values
    samples <- inbv.samp[8]
    band1Val <- inbv.samp[5] 
    band2Val <- inbv.samp[6]
    band3Val <- inbv.samp[7]
    
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
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste(classMethod[k],",Runtime, Start,",start.time,",End,",end.time,",Total,",time.taken))
  }
  closeAllConnections()
  
  
  
  
  
  #### ***START RADIANCE CLASSIFICATIONS*** ####
  
  
  ## Make sure required directories exist 
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name, sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/rad_3class/',sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outRasters/',sep=''), showWarnings = FALSE)
  
  ## Set paths
  outClassPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/rad_3class/',sep='')
  outImgPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep='') 
  outRasPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outRasters/',sep='') 
  
  
  ## Redirect output
  sink.cmd <- paste('sink("',outClassPath,'classificationOutput.txt")',sep='')
  eval(parse(text=sink.cmd))
  
  ## Reclassify columns
  inrad.data$NewClass <- NA
  inrad.data$NewClass[inrad.data$Class == "Land"] <- 2
  inrad.data$NewClass[inrad.data$Class == "Water"] <- 1
  inrad.data$NewClass[inrad.data$Class == "Aquatics"] <- 3
  
  #Set up the number of samples and classification techniques to loop through
  classMethod <- c('randomForest','neuralNetwork','supportVector') #'logit'  couldn't get predict to work with maximumLikelihood
  nSamp <- 260
  
  for (k in 1:length(classMethod)){
    start.time <- Sys.time()
    set.seed(7)
    ## Randomly select nSamp samples from each class
    print(paste("Randomly sampling tables for ", nSamp, " samples"))
    Aqua.samp <- inrad.data[sample(which(inrad.data$Class == "Aquatics") , nSamp) , ]
    Land.samp <- inrad.data[sample(which(inrad.data$Class == "Land") , nSamp) , ]
    Wat.samp <- inrad.data[sample(which(inrad.data$Class == "Water") , nSamp) , ]
    inrad.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
    
    ## Create raster stack object for full mosaic my reading in an ESRI TIFF Raster
    ras.band1 <- raster(inRasRad, band=1)
    ras.band2 <- raster(inRasRad, band=2)
    ras.band3 <- raster(inRasRad, band=3)
    ras.stack <- stack(ras.band1, ras.band2, ras.band3)
    
    ## Rename to columns that will be used to build the formula so they match with the raster
    names(inrad.samp)[5]<- names(ras.stack[[1]])
    names(inrad.samp)[6]<- names(ras.stack[[2]])
    names(inrad.samp)[7]<- names(ras.stack[[3]])
    
    ## Assign the columns to the respective values
    samples <- inrad.samp[8]
    band1Val <- inrad.samp[5] 
    band2Val <- inrad.samp[6]
    band3Val <- inrad.samp[7]
    
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
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste(classMethod[k],",Runtime, Start,",start.time,",End,",end.time,",Total,",time.taken))
  }
  closeAllConnections()
  
  
  
  
  
  #### ***START REFLECTANCE CLASSIFICATIONS*** ####
  
  ## Make sure required directories exist 
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name, sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/ref_3class/',sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outRasters/',sep=''), showWarnings = FALSE)
  
  ## Set paths
  outClassPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/ref_3class/',sep='')
  outImgPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep='') 
  outRasPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outRasters/',sep='') 
  
  
  ## Redirect output
  sink.cmd <- paste('sink("',outClassPath,'classificationOutput.txt")',sep='')
  eval(parse(text=sink.cmd))
  
  ## Reclassify columns
  inref.data$NewClass <- NA
  inref.data$NewClass[inref.data$Class == "Land"] <- 2
  inref.data$NewClass[inref.data$Class == "Water"] <- 1
  inref.data$NewClass[inref.data$Class == "Aquatics"] <- 3
  
  #Set up the number of samples and classification techniques to loop through
  classMethod <- c('randomForest','neuralNetwork','supportVector') #'logit'  couldn't get predict to work with maximumLikelihood
  nSamp <- 260
  
  for (k in 1:length(classMethod)){
    start.time <- Sys.time()
    set.seed(7)
    ## Randomly select nSamp samples from each class
    print(paste("Randomly sampling tables for ", nSamp, " samples"))
    Aqua.samp <- inref.data[sample(which(inref.data$Class == "Aquatics") , nSamp) , ]
    Land.samp <- inref.data[sample(which(inref.data$Class == "Land") , nSamp) , ]
    Wat.samp <- inref.data[sample(which(inref.data$Class == "Water") , nSamp) , ]
    inref.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
    
    ## Create raster stack object for full mosaic my reading in an ESRI TIFF Raster
    ras.band1 <- raster(inRasRef, band=1)
    ras.band2 <- raster(inRasRef, band=2)
    ras.band3 <- raster(inRasRef, band=3)
    ras.stack <- stack(ras.band1, ras.band2, ras.band3)
    
    ## Rename to columns that will be used to build the formula so they match with the raster
    names(inref.samp)[5]<- names(ras.stack[[1]])
    names(inref.samp)[6]<- names(ras.stack[[2]])
    names(inref.samp)[7]<- names(ras.stack[[3]])
    
    ## Assign the columns to the respective values
    samples <- inref.samp[8]
    band1Val <- inref.samp[5] 
    band2Val <- inref.samp[6]
    band3Val <- inref.samp[7]
    
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
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste(classMethod[k],",Runtime, Start,",start.time,",End,",end.time,",Total,",time.taken))
  }
  closeAllConnections()
}

## -------------------------------------------------
print ("COMPLETE")




