##Author: Christopher Zarzar
##Created: 3-Mar-2017
##NOTES: This script classifies LPR samples from August 2015 imagery using the top to 
##classification techniques I determined from previous work. It then outputs and assess the 
##user and producer errors for comparison. 
##Edit: 4-Mar-2017 Chris Zarzar: Added write.csv to write out the accuracyMatrix. 

## load library requirements
library("rasclass")

## Read in the ground reference data
# bv.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015BV25_3class.csv", header = TRUE)
# ad.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Rad25_3class.csv", header = TRUE)
# augref.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Ref25_3class.csv", header = TRUE)
augbv.data <- read.csv("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/datasets/aug2015BV25_3class.csv", header = TRUE)
augrad.data <- read.csv("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/datasets/aug2015Rad25_3class.csv", header = TRUE)
augref.data <- read.csv("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/datasets/aug2015Ref25_3class.csv", header = TRUE)


## Reclassify columns
augbv.data$NewClass <- NA
augbv.data$NewClass[augbv.data$Class == "Land"] <- 2
augbv.data$NewClass[augbv.data$Class == "Water"] <- 1
augbv.data$NewClass[augbv.data$Class == "Aquatics"] <- 3

## Redirect output
sink.cmd <- paste('sink("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/bv_3class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

#Set up the number of samples and classification techniques to loop through
classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
nSamp <- 260

for (k in 1:length(classMethod)){
  set.seed(7)
  ## Randomly select nSamp samples from each class
  print(paste("Randomly sampling tables for ", nSamp, " samples"))
  Aqua.samp <- augbv.data[sample(which(augbv.data$Class == "Aquatics") , nSamp) , ]
  Land.samp <- augbv.data[sample(which(augbv.data$Class == "Land") , nSamp) , ]
  Wat.samp <- augbv.data[sample(which(augbv.data$Class == "Water") , nSamp) , ]
  augbv.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
  
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
  write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/bv_3class/',classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/bv_3class/',classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write.csv(accuracyMatrix, file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/bv_3class/',classMethod[k],nSamp,'matrix.csv', sep=''))
}
closeAllConnections()





#### ***START RADIANCE CLASSIFICATIONS*** ####


## Reclassify columns
augrad.data$NewClass <- NA
augrad.data$NewClass[augrad.data$Class == "Land"] <- 2
augrad.data$NewClass[augrad.data$Class == "Water"] <- 1
augrad.data$NewClass[augrad.data$Class == "Aquatics"] <- 3

## Redirect output
sink.cmd <- paste('sink("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/rad_3class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

#Set up the number of samples and classification techniques to loop through
classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
nSamp <- 260

for (k in 1:length(classMethod)){
  set.seed(7)
  ## Randomly select nSamp samples from each class
  print(paste("Randomly sampling tables for ", nSamp, " samples"))
  Aqua.samp <- augrad.data[sample(which(augrad.data$Class == "Aquatics") , nSamp) , ]
  Land.samp <- augrad.data[sample(which(augrad.data$Class == "Land") , nSamp) , ]
  Wat.samp <- augrad.data[sample(which(augrad.data$Class == "Water") , nSamp) , ]
  augrad.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
  
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
  write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/rad_3class/',classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/rad_3class/',classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write.csv(accuracyMatrix, file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/rad_3class/',classMethod[k],nSamp,'matrix.csv', sep=''))
}
closeAllConnections()


#### ***START REFLECTANCE CLASSIFICATIONS*** ####
## Reclassify columns
augref.data$NewClass <- NA
augref.data$NewClass[augref.data$Class == "Land"] <- 2
augref.data$NewClass[augref.data$Class == "Water"] <- 1
augref.data$NewClass[augref.data$Class == "Aquatics"] <- 3

## Redirect output
sink.cmd <- paste('sink("C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/ref_3class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

#Set up the number of samples and classification techniques to loop through
classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
nSamp <- 260

for (k in 1:length(classMethod)){
  set.seed(7)
  ## Randomly select nSamp samples from each class
  print(paste("Randomly sampling tables for ", nSamp, " samples"))
  Aqua.samp <- augref.data[sample(which(augref.data$Class == "Aquatics") , nSamp) , ]
  Land.samp <- augref.data[sample(which(augref.data$Class == "Land") , nSamp) , ]
  Wat.samp <- augref.data[sample(which(augref.data$Class == "Water") , nSamp) , ]
  augref.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
  
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
  write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/ref_3class/',classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/ref_3class/',classMethod[k],nSamp,'.txt', sep=''), append = TRUE, sep = ",")
  write.csv(accuracyMatrix, file =paste('C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/lpr_classification_output/ref_3class/',classMethod[k],nSamp,'matrix.csv', sep=''))
}
closeAllConnections()

## -------------------------------------------------
print ("COMPLETE")




##  END  ##