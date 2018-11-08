##Author: Christopher Zarzar
##Created: 15-Nov-2016
##Edit: CZ 23-Nov-2016: Adapted example script for the LPR classifications


##NOTES: This script classifies data from the LPR August 2015 imagery. 

## load library requirements
library("rasclass")
#library("car")

# Output all returned values to text file
sink("E:/Research/RWorkspace/classifications.txt")

set.seed(7)
## Read in the data
augbv.data <- read.csv("E:/Research/RWorkspace/datasets/aug2015BV25.csv", header = TRUE)
augrad.data <- read.csv("E:/Research/RWorkspace/datasets/aug2015Rad25.csv", header = TRUE)
augref.data <- read.csv("E:/Research/RWorkspace/datasets/aug2015Ref25.csv", header = TRUE)

## Reclassify columns
#augbv.data$Class <- recode(augbv.data$Class,"'Land'=2")
#augbv.data$Class <- recode(augbv.data$Class,"'Water'=1")
augbv.data$NewClass <- NA
augbv.data$NewClass[augbv.data$Class == "Land"] <- 2 
augbv.data$NewClass[augbv.data$Class == "Water"] <- 1 

## Assign the columns to the respective values
samples <- augbv.data[8]
band1Val <- augbv.data[5]
band2Val <- augbv.data[6]
band3Val <- augbv.data[7]
## Assign only those data I need to a new data frame
newdata <- data.frame(samples, band1Val, band2Val, band3Val)

## convert the samples dataframe to a list to be used later
sampleList = list(samples)

## create a raster based on newdata
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 50, nrows = 80,
                          xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999, 
                          samplename = 'NewClass')
summary(object)

## Classify using multiple algorithms
outlist <- list()
outlist[['maximumLikelihood']] <- classifyRasclass(object, method ='maximumLikelihood')
summary(outlist[['maximumLikelihood']])
mlc.kappa <- outlist[['maximumLikelihood']]@kappa
mlc.overallAccuracy <- outlist[['maximumLikelihood']]@overallAccuracy
mlc.accuracyMatrix <- outlist[['maximumLikelihood']]@accuracyMatrix

outlist[['logit']] <- classifyRasclass(object, method = 'logit') 
summary(outlist[['logit']])
log.kappa <- outlist[['logit']]@kappa
log.overallAccuracy <- outlist[['logit']]@overallAccuracy
log.accuracyMatrix <- outlist[['logit']]@accuracyMatrix

outlist[['neuralNetwork']] <- classifyRasclass(object, method = 'neuralNetwork') 
summary(outlist[['neuralNetwork']])
nn.kappa <- outlist[['neuralNetwork']]@kappa
nn.overallAccuracy <- outlist[['neuralNetwork']]@overallAccuracy
nn.accuracyMatrix <- outlist[['neuralNetwork']]@accuracyMatrix

outlist[['randomForest']] <- classifyRasclass(object, method = 'randomForest') 
summary(outlist[['randomForest']])
ran.kappa <- outlist[['randomForest']]@kappa
ran.overallAccuracy <- outlist[['randomForest']]@overallAccuracy
ran.accuracyMatrix <- outlist[['randomForest']]@accuracyMatrix

outlist[['supportVector']] <- classifyRasclass(object, method = 'supportVector') 
summary(outlist[['supportVector']])
svm.kappa <- outlist[['supportVector']]@kappa
svm.overallAccuracy <- outlist[['supportVector']]@overallAccuracy
svm.accuracyMatrix <- outlist[['supportVector']]@accuracyMatrix

## store the data so it can be displayed
samples.ras <- (new('rasclassRaster'))
samples.ras@grid <- augbv.data$NewClass
samples.ras@nrows <- 80
samples.ras@ncols <- 50
samples.ras@xllcorner <- 0
samples.ras@yllcorner <- 0
samples.ras@cellsize <- 1
samples.ras@NAvalue <- -9999
writeRaster(samples.ras, path = "E:/Research/RWorkspace/datasets/lprRasters/augBVsamples.asc")

## display the resolts of the classifer
## create a multipanel plot
#opar <- par(mfrow = c(2,3))
#image(samples.ras)
#title('Sample Data')
for(i in 1:length(outlist)){
#  image(outlist[[i]]@predictedGrid)
#  title(names(outlist)[[i]])
  if (names(outlist)[[i]] == 'maximumLikelihood'){
    writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_MLC.asc")
  }
  
  if (names(outlist)[[i]] =='logit'){
    writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_LOG.asc")
  }
  
  if (names(outlist)[[i]] =='neuralNetwork'){
    writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_NN.asc")
  }
  
  if (names(outlist)[[i]] =='randomForest'){
    writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_RAN.asc")
  }
  
  if (names(outlist)[[i]] =='supportVector'){
    writeRaster(outlist[[i]]@predictedGrid, path = "E:/Research/RWorkspace/datasets/lprRasters/augBV_SVM.asc")
  }
  
  
}
par(opar)


