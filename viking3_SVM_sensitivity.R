
##Author: Christopher Zarzar
##Created: 7-April-2017

##NOTES: This script is a Random Forest Sensitivity Analysis using August 2015 UAS data.
## This script is set up so that any classification technique can be set up for 
## a sensitivity analysis using this script
##EDITED CHRIS ZARZAR 5/12/17; Adding capability to run sensitivity analysis across numerous samples size and splitfractions
##EDITED CHRIS ZARZAR 6/14/17; Major changes to loop and configuration to set up to run on Bazooka at HPC. Big change is that a new writeout textfile is created for every loop to make it easier to determine start and end time. 
##EDITED CHRIS ZARZAR 6/27/17; ##EDITED CHRIS ZARZAR 6/27/17; Changed samp size intervals to .1 to speed up the loop. to speed up the loop. Failures on gri/ drive  required I speed this up to finish in time. I also decreased the cost intervals from (0.001,0.01,0.1,1,5,10,50,100,1000) and decreased the degrees from (1,2,3,4)
##EDITED CHRIS ZARZAR 6/27/17; split up sample ratios between the different computers to decrease processing time. 

## load library requirements
library("rasclass")
library("tools")
library("mlbench")

set.seed(7)

## Set up primary vector to loop through all of the dates available
loop.files <- c('aug2015','dec2014','dec2015','mar2015','may2015') # completed
loop.imgname <- c('bv_3class','rad_3class','ref_3class')
for (d in 1:length(loop.files)){
  date = loop.files[d]
  var.name = file_path_sans_ext(date)
  date.name = substr (date,1,7)
  print (date)
  for (imgName in loop.imgname){
    if (imgName == "bv_3class"){
      img.name <- 'BV'
    }else if (imgName == "rad_3class"){
      img.name <- 'Rad'
    }else if (imgName == "ref_3class"){
      img.name <- 'Ref'
    }
    
    ## Set up global variables
    #mainPath <- "C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/"
    mainPath <- "/gri/general/cmzarzar/RWorkspace/"
    kern.list <- c('poly','radial')
    cost.list <- c(0.01,0.1,1,5,10,50,100,1000)
    degree.list <- c(1,2,3)
    #Set up the number of samples and classification techniques to loop through
    classMethod <- 'supportVector' ## 'neuralNetwork','randomForest' ## Other options for future sensitivity analyses. 
    nSamp.list <- seq(50,500,by=30)
    sampRatio.list <- seq(0.8,0.9,by=0.1)
    
    
    
    ## Make sure required directories exist 
    dir.create(paste(mainPath,'lpr/', sep=''), showWarnings = FALSE)
    dir.create(paste(mainPath,'lpr/lpr_classification_output/', sep=''), showWarnings = FALSE)
    dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/', sep=''), showWarnings = FALSE)
    dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/',imgName,'/', sep=''), showWarnings = FALSE)
    dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep=''), showWarnings = FALSE)
    
    ## Set paths
    outClassPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/',imgName,'/', sep='')
    outImgPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep='') 
    
    
    ## Read in the datasets
    in.data <- read.csv(paste(mainPath,'/datasets/',date.name, img.name,'25_3class.csv', sep = ''), header = TRUE)
    
    ## Reclassify columns
    in.data$NewClass <- NA
    in.data$NewClass[in.data$Class == "Land"] <- 2
    in.data$NewClass[in.data$Class == "Water"] <- 1
    in.data$NewClass[in.data$Class == "Aquatics"] <- 3
    
    
    ## Redirect output
    sink.cmd <- paste('sink("',outClassPath,'classificationOutput_SVM_viking3.txt")',sep='')
    eval(parse(text=sink.cmd))
    
    ## Set file to write out run times
    write("Class_Method,Samp_Size,Split_Frac,kern,kernParm(gamma),degree,cost,Start,End,Total", file =paste(outClassPath,'Runtimes','_',classMethod,'_viking3.txt', sep=''), append = TRUE, sep = ",")
    
    #Set up testing loops
    for (nSamp in nSamp.list){
      for (sampRatio in sampRatio.list){
        for (cost in cost.list){
          for (kern in kern.list){
            if(kern=="poly"){
              kernParm.list <- c(1.00,2.00)
            }else{
              kernParm.list <- c(0.01,0.05,0.1,0.5)
            }
            for (kernParm in kernParm.list){
              for (deg in degree.list){
                start.time <- Sys.time()
                set.seed(7)
                for (k in 1:1000){
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
                  
                  ## Classify the data
                  outlist <- list()
                  outlist[[classMethod]] <- classifyRasclass(object, sampRatio, method = classMethod, kernel=kern, gamma=kernParm, degree=deg, cost=cost)
                  kappa <- outlist[[classMethod]]@kappa
                  overallAccuracy <- outlist[[classMethod]]@overallAccuracy
                  accuracyMatrix <- outlist[[classMethod]]@accuracyMatrix
                  if (k==1){
                    write("Class_Method,Samp_Size,Split_Frac,Kappa,Overall_Acc,kern,kernParm(gamma),degree,cost", file =paste(outClassPath,classMethod,'_',nSamp,'_',sampRatio,'_',kern,'_',kernParm,'_',deg,'_',cost,'.txt', sep=''), append = TRUE, sep = ",")
                  }
                  write(paste(classMethod,nSamp,sampRatio,kappa,overallAccuracy,kern,kernParm,deg,cost,sep =','), file =paste(outClassPath,classMethod,'_',nSamp,'_',sampRatio,'_',kern,'_',kernParm,'_',deg,'_',cost,'.txt', sep=''), append = TRUE, sep = ",")
                }
                ## Calculate and write out the run time of the script
                end.time <- Sys.time()
                time.taken <- end.time - start.time
                write(paste(classMethod,nSamp,sampRatio,kern,kernParm,deg,cost,start.time,end.time,time.taken,sep =','), file =paste(outClassPath,'Runtimes','_',classMethod,'_viking3.txt', sep=''), append = TRUE, sep = ",")
                
              }
            }
          }
        }
      }
    }
    
    closeAllConnections()
  }
}



# #par(mfrow = c(3, 1))
# ## Now create the confidence interval plots for these sensitivity analyses
# file.names <- c('bv_3class', 'rad_3class', 'ref_3class')
# for (fname in file.names){
#   ## List and sort the files
#   file.cmd <- paste('files <- file.info(list.files(path="',mainPath,'lpr/lpr_classification_output/',date.name,'/',fname,'", pattern="*.txt", full.names = TRUE, recursive = FALSE))', sep='')
#   eval(parse(text=file.cmd))
#   details <- files[with(files, order(as.POSIXct(mtime))), ]
#   fileList <- rownames(details)
# 
#   ## Set up global variables
#   ## Create a matrix of three zero placeholds for the future CIs to be attached to
#   plotmatlog <- numeric(3)
#   plotmatneu <- numeric(3)
#   plotmatran <- numeric(3)
#   plotmatmax <- numeric(3)
#   plotmatsup <- numeric(3)
#   count <- 0
# 
#   for (currentFile in fileList){
#     fileBase = basename(currentFile)
#     firstThreeCharacters <- substr(fileBase, 1, 3)
#     if (firstThreeCharacters == "log"){
#       currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
#       currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
#       plotmatlog <- cbind(plotmatlog,currentFile.ci)
#       logplot.title <- fileBase
#     }else if (firstThreeCharacters == "neu"){
#       currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
#       currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
#       plotmatneu <- cbind(plotmatneu,currentFile.ci)
#       neuplot.title <- fileBase
#     }else if (firstThreeCharacters == "ran"){
#       currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
#       currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
#       plotmatran <- cbind(plotmatran,currentFile.ci)
#       ranplot.title <- fileBase
#     }else if (firstThreeCharacters == "max"){
#       currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
#       currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
#       plotmatmax <- cbind(plotmatmax,currentFile.ci)
#       maxplot.title <- fileBase
#     }else if (firstThreeCharacters == "sup"){
#       currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
#       currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
#       plotmatsup <- cbind(plotmatsup,currentFile.ci)
#       supplot.title <- fileBase
#     }
#     count <- count+1
#   }
# 
#   ## Plot for random trees classifier
#   outPut.cmd <- paste('png(file ="',outImgPath,'randomTrees_',fname,'.png", width=800, height=450)',sep='')
#   eval(parse(text=outPut.cmd))
#   plotmat <- plotmatran
#   ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
#   plotmat <- plotmat[,-1]
#   op <- par(mar = c(8.5,6.5,5,3) + 0.1) ## default is c(5,4,4,2) + 0.1
#   #xrange <- c('1-100','1-200','1-300','1-400','1-500','1-600','1-700','1-800','1-900','1-1000','1-1100','1-1200','1-1300','1-1400','1-1500')
#   xrange <- c('1-100','1-500','1-1000','1-1500','1-2000','1-2500','1-3000','1-3500','1-4000','1-4500','2-100','2-500','2-1000','2-1500','2-2000','2-2500','2-3000','2-3500','2-4000','2-4500','3-100','3-500','3-1000','3-1500','3-2000','3-2500','3-3000','3-3500','3-4000','3-4500')
#   lab <- xrange
#   barwidth=0.2
#   n.plots <- length(plotmat[1,])
#   plot(plotmat[2,],main="",xaxt="n",ylim=c(0.7,0.85), xlab="",ylab="",cex.main=2, cex.axis=1.5, pch=16)
#   for (i in 1:n.plots) {
#     lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
#     lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
#     lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
# 
#   }
#   axis(1, at=1:length(lab), labels=FALSE)
#   text(x=1:length(lab), par()$usr[3], labels = lab, srt = 90, adj = 1.25, xpd = TRUE, cex=1.5)
#   if (fname == "bv_3class"){
#     mtext("Brightness Values", side=3, line=1.5, cex=3)
#   }else if (fname == "rad_3class"){
#     mtext("Radiance", side=3, line=1.5, cex=3)
#   }else if (fname == "ref_3class"){
#     mtext("Reflectance", side=3, line=1.5, cex=3)
#   }
#   mtext("mtry-ntree", side=1, line=6.5, cex=2.5)
#   mtext("Overall Accuracy", side=2, line=3.5, cex=2.5)
#   par(op)
#   #abline(h=0.85, col="blue", lty="solid")
#   dev.off()
#   closeAllConnections()
# }
# 
# ## -------------------------------------------------
# print ("COMPLETE")
# 
# 
# 
# ######### Checking my results with the Caret library ###############
# 
# datasetBV <- cbind(inbv.data[5],inbv.data[6],inbv.data[7],inbv.data[2]) #Bind columns. Variables then Class
# datasetRad <- cbind(inrad.data[5],inrad.data[6],inrad.data[7],inrad.data[2]) #Bind columns. Variables then Class
# datasetRef <- cbind(inref.data[5],inref.data[6],inref.data[7],inref.data[2]) #Bind columns. Variables then Class
# 
# x <- datasetBV[,1:3] #Read in the variable data
# y <- datasetBV[,4] #Identify class information
# 
# 
# 
# # Create model with default paramters
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# seed <- 7
# metric <- "Accuracy"
# set.seed(seed)
# mtry <- sqrt(ncol(x))
# tunegrid <- expand.grid(.mtry=mtry)
# rf_default <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control,allowParallel=TRUE)
# print(rf_default)
# 
# # Random Search
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
# set.seed(seed)
# mtry <- sqrt(ncol(x))
# rf_random <- train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
# print(rf_random)
# plot(rf_random)
# dev.off()
# 
# # Controlled Search
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# set.seed(seed)
# tunegrid <- expand.grid(.mtry=c(1:15))
# rf_gridsearch <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_gridsearch)
# plot(rf_gridsearch)
# dev.off()
# 
# # Manual Search
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
# modellist <- list()
# for (ntree in c(1000, 1500, 2000, 2500)) {
#   set.seed(seed)
#   fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# # compare results
# results <- resamples(modellist)
# summary(results)
# dotplot(results)
# dev.off()
# 
# # Manual Search mtry 1
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=1)
# modellist <- list()
# for (ntree in c(1000, 1500, 2000, 2500)) {
#   set.seed(seed)
#   fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# # compare results
# results <- resamples(modellist)
# summary(results)
# dotplot(results)
# 
# # Manual Search mtry 2
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=2)
# modellist <- list()
# for (ntree in c(1000, 1500, 2000, 2500)) {
#   set.seed(seed)
#   fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# # compare results
# results <- resamples(modellist)
# summary(results)
# dotplot(results)
# 
# # Manual Search mtry 3
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=3)
# modellist <- list()
# for (ntree in c(1000, 1500, 2000, 2500)) {
#   set.seed(seed)
#   fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# # compare results
# results <- resamples(modellist)
# summary(results)
# dotplot(results)
# 
# 
# # Customize and extend Carets abilities
# customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
# customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
# customRF$grid <- function(x, y, len = NULL, search = "grid") {}
# customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
# customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
# customRF$sort <- function(x) x[order(x[,1]),]
# customRF$levels <- function(x) x$classes
# customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
# customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
# customRF$grid <- function(x, y, len = NULL, search = "grid") {}
# customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
# customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
# customRF$sort <- function(x) x[order(x[,1]),]
# customRF$levels <- function(x) x$classes
# 
# # train customized model BV
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# tunegrid <- expand.grid(.mtry=seq(0.5,2,by=0.2), .ntree=c(500, 700, 900, 1100))
# set.seed(seed)
# custom <- train(Class~., data=datasetBV, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
# summary(custom)
# png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Rf_Caret_Tuning_BV.png", width=700, height=400)
# plot(custom, pch=16)
# dev.off()
# closeAllConnections()
# 
# # train customized model RADIANCE
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# tunegrid <- expand.grid(.mtry=seq(0.5,2,by=0.2), .ntree=c(500, 700, 900, 1100))
# set.seed(seed)
# custom <- train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
# summary(custom)
# png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Rf_Caret_Tuning_Rad.png", width=700, height=400)
# plot(custom, pch=16)
# dev.off()
# closeAllConnections()
# 
# # train customized model REFLECTANCE
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# tunegrid <- expand.grid(.mtry=seq(0.5,2,by=0.2), .ntree=c(500, 700, 900, 1100))
# set.seed(seed)
# custom <- train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
# summary(custom)
# png(file ="C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/lpr/Rf_Caret_Tuning_Ref.png", width=700, height=400)
# plot(custom, pch=16)
# dev.off()
# closeAllConnections()