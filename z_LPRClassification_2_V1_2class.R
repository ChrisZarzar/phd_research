##Author: Christopher Zarzar
##Created: 15-Nov-2016
##Edit: CZ 23-Nov-2016: Adapted example script for the LPR classifications
##Edit: CZ 25-Nov-2016: Added higher level functionality and loops
##Edit: CZ 5-Dec-2016: Added radiance and reflectance classificaiton to script. 
##Edit: CZ 8-Dec-2016: Added aquatics class to the classification.Had to also update ncol = 75 and nrows = 80 for the extra 2000 samples
##EDIT: CZ 31-Jan-2017: Adjusting script so that it selects N samples from the 2000 for each class and runs classifications train/test 70-30 ratio 
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



## Read in the data
augbv.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015BV25_2class.csv", header = TRUE)
augrad.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Rad25_2class.csv", header = TRUE)
augref.data <- read.csv("/home/chriszarzar/Desktop/RWorkspace/datasets/aug2015Ref25_2class.csv", header = TRUE)

#### ***START BRIGHTNESS CLASSIFICATIONS*** ####


## Reclassify columns
augbv.data$NewClass <- NA
augbv.data$NewClass[augbv.data$Class == "Land"] <- 2
augbv.data$NewClass[augbv.data$Class == "Water"] <- 1
#augbv.data$NewClass[augbv.data$Class == "Aquatics"] <- 3

## Redirect output
sink.cmd <- paste('sink("/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/bv_2class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

#Set up the number of samples and classification techniques to loop through

classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
nSamp <- c(50, 80, 110, 140, 170, 200, 230, 260, 290, 320, 350, 380, 410, 440, 470, 500)

for (k in 1:length(classMethod)){
  set.seed(7)
  for (i in 1:length(nSamp)){
    for (j in 1:1000){
    ## Randomly select nSamp samples from each class
    print(paste("Randomly sampling tables for ", nSamp[i], " samples"))
    #Aqua.samp <- augbv.data[sample(which(augbv.data$Class == "Aquatics") , nSamp[i] ) , ]
    Land.samp <- augbv.data[sample(which(augbv.data$Class == "Land") , nSamp[i]) , ]
    Wat.samp <- augbv.data[sample(which(augbv.data$Class == "Water") , nSamp[i] ) , ]
    augbv.samp <- rbind(Land.samp, Wat.samp)
    
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
    object <- setRasclassData(newdata, ncols = 2*nSamp[i], nrows = 1,
                              xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                              samplename = 'NewClass')
    print("Summary of new raster")
    print(summary(object))
    
    ## Output all returned values to text file
    print(paste("Classifying for class method " , classMethod[k], " with ", nSamp[i], " samples"))
  
        ## Classify using multiple algorithms
        outlist <- list()
        outlist[[classMethod[k]]] <- classifyRasclass(object, splitfraction = .7, method =classMethod[k])
        kappa <- outlist[[classMethod[k]]]@kappa
        overallAccuracy <- outlist[[classMethod[k]]]@overallAccuracy
        accuracyMatrix <- outlist[[classMethod[k]]]@accuracyMatrix
        if (j==1){
          write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/bv_2class/',classMethod[k],nSamp[i],'.txt', sep=''), append = TRUE, sep = ",")
        }
        write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/bv_2class/',classMethod[k],nSamp[i],'.txt', sep=''), append = TRUE, sep = ",")
    }
  }
}
closeAllConnections()






#### ***START RADIANCE CLASSIFICATIONS*** ####


## Reclassify columns
augrad.data$NewClass <- NA
augrad.data$NewClass[augrad.data$Class == "Land"] <- 2
augrad.data$NewClass[augrad.data$Class == "Water"] <- 1
#augrad.data$NewClass[augrad.data$Class == "Aquatics"] <- 3

## Redirect output
sink.cmd <- paste('sink("/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/rad_2class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

#Set up the number of samples and classification techniques to loop through

classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
nSamp <- c(50, 80, 110, 140, 170, 200, 230, 260, 290, 320, 350, 380, 410, 440, 470, 500)

for (k in 1:length(classMethod)){
  set.seed(7)
  for (i in 1:length(nSamp)){
    for (j in 1:1000){
    ## Randomly select nSamp samples from each class
    print(paste("Randomly sampling tables for ", nSamp[i], " samples"))
    #Aqua.samp <- augrad.data[sample(which(augrad.data$Class == "Aquatics") , nSamp[i] ) , ]
    Land.samp <- augrad.data[sample(which(augrad.data$Class == "Land") , nSamp[i]) , ]
    Wat.samp <- augrad.data[sample(which(augrad.data$Class == "Water") , nSamp[i] ) , ]
    augrad.samp <- rbind(Land.samp, Wat.samp)
    
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
    object <- setRasclassData(newdata, ncols = 2*nSamp[i], nrows = 1,
                              xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                              samplename = 'NewClass')
    print("Summary of new raster")
    print(summary(object))
    ## Output all returned values to text file
    print(paste("Classifying for class method " , classMethod[k], " with ", nSamp[i], " samples"))
  
        ## Classify using multiple algorithms
        outlist <- list()
        outlist[[classMethod[k]]] <- classifyRasclass(object, splitfraction = .7, method =classMethod[k])
        kappa <- outlist[[classMethod[k]]]@kappa
        overallAccuracy <- outlist[[classMethod[k]]]@overallAccuracy
        accuracyMatrix <- outlist[[classMethod[k]]]@accuracyMatrix
        if (j==1){
          write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/rad_2class/',classMethod[k],nSamp[i],'.txt', sep=''), append = TRUE, sep = ",")
        }
        write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/rad_2class/',classMethod[k],nSamp[i],'.txt', sep=''), append = TRUE, sep = ",")
    }
  }
}
closeAllConnections()

#### ***START REFLECTANCE CLASSIFICATIONS*** ####
## Reclassify columns
augref.data$NewClass <- NA
augref.data$NewClass[augref.data$Class == "Land"] <- 2
augref.data$NewClass[augref.data$Class == "Water"] <- 1
#augref.data$NewClass[augref.data$Class == "Aquatics"] <- 3

## Redirect output
sink.cmd <- paste('sink("/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/ref_2class/classificationOutput.txt")',sep='')
eval(parse(text=sink.cmd))

#Set up the number of samples and classification techniques to loop through

classMethod <- c('maximumLikelihood','logit','neuralNetwork','randomForest','supportVector')
nSamp <- c(50, 80, 110, 140, 170, 200, 230, 260, 290, 320, 350, 380, 410, 440, 470, 500)

for (k in 1:length(classMethod)){
  set.seed(7)
  for (i in 1:length(nSamp)){
    for (j in 1:1000){
    ## Randomly select nSamp samples from each class
    print(paste("Randomly sampling tables for ", nSamp[i], " samples"))
    #Aqua.samp <- augref.data[sample(which(augref.data$Class == "Aquatics") , nSamp[i] ) , ]
    Land.samp <- augref.data[sample(which(augref.data$Class == "Land") , nSamp[i]) , ]
    Wat.samp <- augref.data[sample(which(augref.data$Class == "Water") , nSamp[i] ) , ]
    augref.samp <- rbind(Land.samp, Wat.samp)
    
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
    object <- setRasclassData(newdata, ncols = 2*nSamp[i], nrows = 1,
                              xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                              samplename = 'NewClass')
    print("Summary of new raster")
    print(summary(object))
    ## Output all returned values to text file
    print(paste("Classifying for class method " , classMethod[k], " with ", nSamp[i], " samples"))
  
        ## Classify using multiple algorithms
        outlist <- list()
        outlist[[classMethod[k]]] <- classifyRasclass(object, splitfraction = .7, method =classMethod[k])
        kappa <- outlist[[classMethod[k]]]@kappa
        overallAccuracy <- outlist[[classMethod[k]]]@overallAccuracy
        accuracyMatrix <- outlist[[classMethod[k]]]@accuracyMatrix
        if (j==1){
          write("Class_Method,Split_Frac,Kappa,Overall_Acc", file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/ref_2class/',classMethod[k],nSamp[i],'.txt', sep=''), append = TRUE, sep = ",")
        }
        write(paste(classMethod[k],0.7,kappa,overallAccuracy, sep =','), file =paste('/home/chriszarzar/Desktop/RWorkspace/lpr/lpr_classification_output/ref_2class/',classMethod[k],nSamp[i],'.txt', sep=''), append = TRUE, sep = ",")
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
