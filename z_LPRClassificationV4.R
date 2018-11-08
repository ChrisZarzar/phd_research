##Author: Christopher Zarzar
##Created: 15-Nov-2016
##Edit: CZ 23-Nov-2016: Adapted example script for the LPR classifications
##Edit: CZ 25-Nov-2016: Added higher level functionality and loops
##Edit: CZ 5-Dec-2016: Added radiance and reflectance classificaiton to script. 
##Edit: CZ 8-Dec-2016: Added aquatics class to the classification.Had to also update ncol = 75 and nrows = 80 for the extra 2000 samples
##Edit: CZ 2-Feb-2017: My adjustment of the script was wronge. Mercer suggested the below:
## 1) Outer loop for the different methods you're testing
## 2) Set seed (to 7 or whatever number you choose)
## 3) 1st inner loop over the different training thresholds/sample sizes (depending on the script)
## 4) 2nd inner loop doing 1000 resamples
##NOTES: This script classifies data from the LPR August 2015 imagery. 

## load library requirements
library("rasclass")
#library("car")
source('/home/chriszarzar/Desktop/RWorkspace/rcodes/plot.ci.R')

#### ***START BRIGHTNESS CLASSIFICATIONS*** ####



## Read in the data
augbv.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015BV25_3class.csv", header = TRUE)
augrad.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Rad25_3class.csv", header = TRUE)
augref.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Ref25_3class.csv", header = TRUE)

## Reclassify columns
augbv.data$NewClass <- NA
augbv.data$NewClass[augbv.data$Class == "Land"] <- 2
augbv.data$NewClass[augbv.data$Class == "Water"] <- 1
augbv.data$NewClass[augbv.data$Class == "Aquatics"] <- 3

## Assign the columns to the respective values
samples <- augbv.data[8]
band1Val <- augbv.data[5]
band2Val <- augbv.data[6]
band3Val <- augbv.data[7]
## Assign only those data I need to a new data frame
newdata <- data.frame(samples, band1Val, band2Val, band3Val)

## create a raster based on newdata
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 75, nrows = 80,
                          xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                          samplename = 'NewClass')
summary(object)

#Set up the number of samples and classification techniques to loop through

classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')

## Output all returned values to text file
sink.cmd <- paste('sink("/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/bv_class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))
for (i in 1:length(classMethod)){
  set.seed(7)
  for (j in seq(0.05,1,.05)){
    for (k in 1:1000){
  ## Classify using multiple algorithms
  outlist <- list()
  outlist[[classMethod[i]]] <- classifyRasclass(object, splitfraction = j, method =classMethod[i])
  #summary(outlist[['maximumLikelihood']])
  kappa <- outlist[[classMethod[i]]]@kappa
  overallAccuracy <- outlist[[classMethod[i]]]@overallAccuracy
  accuracyMatrix <- outlist[[classMethod[i]]]@accuracyMatrix
  if (k==1){
    write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/bv_class/',classMethod[i],j,'.txt', sep=''), append = TRUE, sep = ",")
  }
  write(paste(classMethod[i],j,kappa,overallAccuracy, sep =','), file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/bv_class/',classMethod[i],j,'.txt', sep=''), append = TRUE, sep = ",")
  }
}
}
closeAllConnections()


#### ***START RADIANCE CLASSIFICATIONS*** ####


augrad.data$NewClass <- NA
augrad.data$NewClass[augrad.data$Class == "Land"] <- 2 
augrad.data$NewClass[augrad.data$Class == "Water"] <- 1 
augrad.data$NewClass[augbv.data$Class == "Aquatics"] <- 3 

## Assign the columns to the respective values
samples <- augrad.data[8]
band1Val <- augrad.data[5]
band2Val <- augrad.data[6]
band3Val <- augrad.data[7]
## Assign only those data I need to a new data frame
newdata <- data.frame(samples, band1Val, band2Val, band3Val)

## create a raster based on newdata
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 75, nrows = 80,
                          xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999, 
                          samplename = 'NewClass')
summary(object)

#Set up the number of samples and classification techniques to loop through

classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
sink.cmd <- paste('sink("/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/rad_class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))
for (i in 1:length(classMethod)){
  set.seed(7)
  for (j in seq(0.05,1,.05)){
    for (k in 1:1000){
      ## Classify using multiple algorithms
      outlist <- list()
      outlist[[classMethod[i]]] <- classifyRasclass(object, splitfraction = j, method =classMethod[i])
      #summary(outlist[['maximumLikelihood']])
      kappa <- outlist[[classMethod[i]]]@kappa
      overallAccuracy <- outlist[[classMethod[i]]]@overallAccuracy
      accuracyMatrix <- outlist[[classMethod[i]]]@accuracyMatrix
      if (k==1){
        write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/rad_class/',classMethod[i],j,'.txt', sep=''), append = TRUE, sep = ",")
      }
      write(paste(classMethod[i],j,kappa,overallAccuracy, sep =','), file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/rad_class/',classMethod[i],j,'.txt', sep=''), append = TRUE, sep = ",")
    }
  }
}
closeAllConnections()
#### ***START REFLECTANCE CLASSIFICATIONS*** ####


augref.data$NewClass <- NA
augref.data$NewClass[augref.data$Class == "Land"] <- 2 
augref.data$NewClass[augref.data$Class == "Water"] <- 1 
augref.data$NewClass[augbv.data$Class == "Aquatics"] <- 3 

## Assign the columns to the respective values
samples <- augref.data[8]
band1Val <- augref.data[5]
band2Val <- augref.data[6]
band3Val <- augref.data[7]
## Assign only those data I need to a new data frame
newdata <- data.frame(samples, band1Val, band2Val, band3Val)

## create a raster based on newdata
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 75, nrows = 80,
                          xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999, 
                          samplename = 'NewClass')
summary(object)

#Set up the number of samples and classification techniques to loop through

classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
## Output all returned values to text file
sink.cmd <- paste('sink("/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/ref_class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))
for (i in 1:length(classMethod)){
  set.seed(7)
  for (j in seq(0.05,1,.05)){
    for (k in 1:1000){
      ## Classify using multiple algorithms
      outlist <- list()
      outlist[[classMethod[i]]] <- classifyRasclass(object, splitfraction = j, method =classMethod[i])
      #summary(outlist[['maximumLikelihood']])
      kappa <- outlist[[classMethod[i]]]@kappa
      overallAccuracy <- outlist[[classMethod[i]]]@overallAccuracy
      accuracyMatrix <- outlist[[classMethod[i]]]@accuracyMatrix
      if (k==1){
        write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/ref_class/',classMethod[i],j,'.txt', sep=''), append = TRUE, sep = ",")
      }
      write(paste(classMethod[i],j,kappa,overallAccuracy, sep =','), file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_percent_classifications/ref_class/',classMethod[i],j,'.txt', sep=''), append = TRUE, sep = ",")
    }
  }
}
closeAllConnections()

## Use below to visualize the data
## -------------------------------------------------
## store the data so it can be displayed
# samples.ras <- (new('rasclassRaster'))
# samples.ras@grid <- augbv.data$NewClass
# samples.ras@nrows <- 80
# samples.ras@ncols <- 50
# samples.ras@xllcorner <- 0
# samples.ras@yllcorner <- 0
# samples.ras@cellsize <- 1
# samples.ras@NAvalue <- -9999
# writeRaster(samples.ras, path = "E:/Research/RWorkspace/datasets/lprRasters/augBVsamples.asc")
# 
# ## display the resolts of the classifer
# ## create a multipanel plot
# opar <- par(mfrow = c(2,3))
# image(samples.ras)
# title('Sample Data')
# for(i in 1:length(outlist)){
#   image(outlist[[i]]@predictedGrid)
#   title(names(outlist)[[i]])
#   if (names(outlist)[[i]] == 'maximumLikelihood'){
#     writeRaster(outl++ist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_MLC.asc")
#   }
#   
#   if (names(outlist)[[i]] =='logit'){
#     writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_LOG.asc")
#   }
#   
#   if (names(outlist)[[i]] =='neuralNetwork'){
#     writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_NN.asc")
#   }
#   
#   if (names(outlist)[[i]] =='randomForest'){
#     writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_RAN.asc")
#   }
#   
#   if (names(outlist)[[i]] =='supportVector'){
#     writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_SVM.asc")
#   }
#   
#   
# }
# par(opar)

## -------------------------------------------------
print ("COMPLETE")
## END
