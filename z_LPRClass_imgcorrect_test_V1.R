##Author: Christopher Zarzar
##Created: 20-Mar-2017
##NOTES: This script classifies LPR samples for each mission and calculates the 
##overall accuracy and iterates 1000 times using only the logit classifier with 260
##randomly selected samples and does this for each of the type of image corrections
##
##PURPOSE: Determine whether any mission shows a need for image correction prior to classifications
##
##HISTORY:
##EDIT: 20-Mar-2017 Chris Zarzar: Script created adopting components from the Mosic classification and sample size scripts
## Organizing the files based on date so I can create CI plots based on date rather than image correction type. 

## load library requirements
library("rasclass")
library("tools")

## Set up vectors that will be looped through
loop.files <- c('dec2014','aug2015','dec2015','mar2015','may2015')
loop.imgtype <- c('BV', 'Rad', 'Ref')

## Set up loop for all of the dates available
for (d in 1:length(loop.files)){
  date = loop.files[d]
  var.name = file_path_sans_ext(date)
  date.name = substr(date,1,7)
  print (date)
  
  ## Set up global variables
  mainPath <- "C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/"
  
  ## Make sure required directories exist 
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/',sep=''), showWarnings = FALSE)
  
  ## Set paths
  outClassPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/',sep='')
  
  ## Set up loop to work through each type of image correction
  for (img in 1:length(loop.imgtype)){
    img.name <- loop.imgtype[img]
    
    
    ## Read in the ground reference data
    in.data <- read.csv(paste(mainPath,'/datasets/',date.name, img.name,'25_3class.csv', sep = ''), header = TRUE)
    
    ## Redirect output
    sink.cmd <- paste('sink("', outClassPath, date.name, '_', img.name, '_classificationOutput.txt")',sep='')
    eval(parse(text=sink.cmd))
    
    ## Reclassify columns
    in.data$NewClass <- NA
    in.data$NewClass[in.data$Class == "Land"] <- 2
    in.data$NewClass[in.data$Class == "Water"] <- 1
    in.data$NewClass[in.data$Class == "Aquatics"] <- 3
    
    #Set up the number of samples and classification techniques to loop through
    classMethod <- 'logit'
    nSamp <- 260
    start.time <- Sys.time()
    set.seed(7)
    for (j in 1:1000){
      ## Randomly select nSamp samples from each class
      print(paste("Randomly sampling tables for ", nSamp, " samples"))
      Aqua.samp <- in.data[sample(which(in.data$Class == "Aquatics") , nSamp) , ]
      Land.samp <- in.data[sample(which(in.data$Class == "Land") , nSamp) , ]
      Wat.samp <- in.data[sample(which(in.data$Class == "Water") , nSamp) , ]
      in.samp <- rbind(Land.samp, Wat.samp, Aqua.samp)
      
      ## Assign the columns to the respective values
      samples <- in.samp[8]
      band1Val <- in.samp[5] 
      band2Val <- in.samp[6]
      band3Val <- in.samp[7]
      
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
      print(paste("Classifying for class method " , classMethod, " with ", nSamp, " samples"))
      
      ## Classify data
      outlist <- list()
      outlist[[classMethod]] <- classifyRasclass(object, splitfraction = .7, method =classMethod)
      kappa <- outlist[[classMethod]]@kappa
      overallAccuracy <- outlist[[classMethod]]@overallAccuracy
      accuracyMatrix <- outlist[[classMethod]]@accuracyMatrix
      if (j==1){
        write("Class_Method,Split_Frac,Kappa,Overall_Acc,OW_UA, TV_UA, AV_UA, OW_PA, TV_PA, AV_PA", file =paste(outClassPath, img.name, '_', date.name, '_', classMethod, '_', nSamp,'.txt', sep=''), append = TRUE, sep = ",")
      }
      write(paste(classMethod,0.7,kappa,overallAccuracy,accuracyMatrix[4:4],accuracyMatrix[8:8],accuracyMatrix[12:12],accuracyMatrix[13:13],accuracyMatrix[14:14],accuracyMatrix[15:15], sep =','), file =paste(outClassPath, img.name,  '_', date.name, '_', classMethod, '_', nSamp,'.txt', sep=''), append = TRUE, sep = ",")
      
    
    } 
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste(classMethod[k],",Runtime, Start,",start.time,",End,",end.time,",Total,",time.taken))
    closeAllConnections()
  }
}

  
print ("script complete")
##END##