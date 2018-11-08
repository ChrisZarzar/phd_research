
##Author: Christopher Zarzar
##Created: 18-July-2017

##NOTES: This script will create confidence intervals for the ANN, RF, and SVM Sensitivity Analysis using August 2015 UAS data.
## CZ 18-July-2017: Changed from old CI script by dynamically grabing the name of the current file from the loops
## CZ 7-August-2017: Added an if statement to make sure the file exists before assigning it to current file. 

## load library requirements
library("rasclass")
library("tools")
library("mlbench")

set.seed(7)

## Set up primary vector to loop through all of the dates available
loop.files <- c('aug2015') #completed ,'dec2014','dec2015','mar2015','may2015'
loop.imgname <- c('bv_3class')#,'rad_3class','ref_3class')
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
  }
  ## Set up global variables
  mainPath <- "D:/Research/RWorkspace/"
  #mainPath <- "/gri/general/cmzarzar/RWorkspace/"
  kern.list <- c('poly','radial')
  cost.list <- c(0.001,0.01,0.1,1,5,10,50,100,1000)
  degree.list <- c(1,2,3,4)
  #Set up the number of samples and classification techniques to loop through
  classMethod <- 'supportVector' ## 'neuralNetwork','randomForest' ## Other options for future sensitivity analyses. 
  nSamp.list <- seq(50,500,by=30)
  sampRatio.list <- seq(0.1,0.90,by=0.1)
  
  ## Make sure required directories exist 
  dir.create(paste(mainPath,'lpr/', sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/', sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/', sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/',imgName,'/', sep=''), showWarnings = FALSE)
  dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep=''), showWarnings = FALSE)
  
  ## Set paths
  outClassPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/',imgName,'/', sep='')
  outImgPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep='') 
  
  
  #Set up testing loops
  xlist <- c()
  ## Set up global variables
  ## Create a matrix of three zero placeholds for the future CIs to be attached to
  plotmatlog <- numeric(3)
  plotmatneu <- numeric(3)
  plotmatran <- numeric(3)
  plotmatmax <- numeric(3)
  plotmatsup <- numeric(3)
  count <- 0
  
  for (sampRatio in sampRatio.list){
    for (nSamp in nSamp.list){
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
          varList <- c(paste(nSamp,',',sampRatio,',',kernParm,',',kern,',',cost, sep=''))
          xlist <- c(xlist, varList)
          
          #par(mfrow = c(3, 1))
          ## Now create the confidence interval plots for these sensitivity analyses
          #file.names <- c('bv_3class', 'rad_3class', 'ref_3class')
          #for (fname in file.names){
          ## List and sort the files
          #file.cmd <- paste('files <- file.info(list.files(path="',mainPath,'lpr/lpr_classification_output/',date.name,'/',fname,'", pattern="*.txt", full.names = TRUE, recursive = FALSE))', sep='')
          #eval(parse(text=file.cmd))
          #details <- files[with(files, order(as.POSIXct(mtime))), ]
          #fileList <- rownames(details)
          ## Test that the file exists before I set the currentFile variable.
          testFile <- paste(outClassPath,classMethod,'_',nSamp,'_',sampRatio,'_',kern,'_',kernParm,'_',deg,'_',cost,'.txt', sep='')
          if (file.exists(testFile)){
            currentFile <- testFile
          }else{
            next
          }                 
          
          
          fileBase = basename(currentFile)
          firstThreeCharacters <- substr(fileBase, 1, 3)
          if (firstThreeCharacters == "log"){
            currentFile.data <- read.table(currentFile, sep="," , header=TRUE, nrows=1000)
            currentFile.ci <- quantile(currentFile.data[,5],probs=c(0.025,0.5,0.975))
            plotmatlog <- cbind(plotmatlog,currentFile.ci)
            logplot.title <- fileBase
          }else if (firstThreeCharacters == "neu"){
            currentFile.data <- read.table(currentFile, sep="," , header=TRUE, nrows=1000, col.names = paste0("V",seq_len(10)), fill = TRUE)
            currentFile.ci <- quantile(currentFile.data[,5],probs=c(0.025,0.5,0.975))
            plotmatneu <- cbind(plotmatneu,currentFile.ci)
            neuplot.title <- fileBase
            #write(currentFile.ci, file=paste(mainPath,'lpr/lpr_classification_output/',date.name,'/ANN_CIs.txt', sep=''), append = TRUE, sep = ",")
          }else if (firstThreeCharacters == "ran"){
            currentFile.data <- read.table(currentFile, sep="," , header=TRUE, nrows=1000)
            currentFile.ci <- quantile(currentFile.data[,5],probs=c(0.025,0.5,0.975))
            plotmatran <- cbind(plotmatran,currentFile.ci)
            ranplot.title <- fileBase
          }else if (firstThreeCharacters == "max"){
            currentFile.data <- read.table(currentFile, sep="," , header=TRUE, nrows=1000)
            currentFile.ci <- quantile(currentFile.data[,5],probs=c(0.025,0.5,0.975))
            plotmatmax <- cbind(plotmatmax,currentFile.ci)
            maxplot.title <- fileBase
          }else if (firstThreeCharacters == "sup"){
            currentFile.data <- read.table(currentFile, sep="," , header=TRUE, nrows=1000)
            currentFile.ci <- quantile(currentFile.data[,5],probs=c(0.025,0.5,0.975))
            plotmatsup <- cbind(plotmatsup,currentFile.ci)
            supplot.title <- fileBase
            #write(paste(classMethod,nSamp,sampRatio,kern,kernParm,deg,cost,currentFile.ci[1],currentFile.ci[2],currentFile.ci[3],sep=','), file=paste(mainPath,'lpr/lpr_classification_output/',date.name,'/SVM_CIs.txt', sep=''), append = TRUE, sep = ",")
          }
          count <- count+1
          
        }
      }
    }
      }
    }
  }
  ## Plot for SVM classifier
  outPut.cmd <- paste('png(file ="',outImgPath,'SVM_',imgName,sampRatio,'.png", width=1366, height=768)',sep='')
  eval(parse(text=outPut.cmd))
  plotmat <- plotmatsup
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  op <- par(mar = c(8.5,6.5,5,3) + 0.1) ## default is c(5,4,4,2) + 0.1
  xrange <- xlist
  lab <- xrange
  barwidth=0.2
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,],main="",xaxt="n",ylim=c(0.6,0.9), xlab="",ylab="",cex.main=2, cex.axis=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
  }
  axis(1, at=1:length(lab), labels=FALSE)
  text(x=1:length(lab), par()$usr[3], labels = lab, srt = 90, adj = 1.25, xpd = TRUE, cex=0.5)
  if (imgName == "bv_3class"){
    mtext("Brightness Values", side=3, line=1.5, cex=3)
  }else if (imgName == "rad_3class"){
    mtext("Radiance", side=3, line=1.5, cex=3)
  }else if (imgName == "ref_3class"){
    mtext("Reflectance", side=3, line=1.5, cex=3)
  }
  mtext("Configuration", side=1, line=6.5, cex=2.5)
  mtext("Overall Accuracy", side=2, line=3.5, cex=2.5)
  par(op)
  #abline(h=0.85, col="blue", lty="solid")
  dev.off()
  closeAllConnections()
  
  
  
  # ## Plot for random trees classifier
  # outPut.cmd <- paste('png(file ="',outImgPath,'randomTrees_',fname,'.png", width=800, height=450)',sep='')
  # eval(parse(text=outPut.cmd))
  # plotmat <- plotmatran
  # ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  # plotmat <- plotmat[,-1]
  # op <- par(mar = c(8.5,6.5,5,3) + 0.1) ## default is c(5,4,4,2) + 0.1
  # xrange <- c('1-100','1-500','1-1000','1-1500','1-2000','1-2500','1-3000','1-3500','1-4000','1-4500','2-100','2-500','2-1000','2-1500','2-2000','2-2500','2-3000','2-3500','2-4000','2-4500','3-100','3-500','3-1000','3-1500','3-2000','3-2500','3-3000','3-3500','3-4000','3-4500')
  # lab <- xrange
  # barwidth=0.2
  # n.plots <- length(plotmat[1,])
  # plot(plotmat[2,],main="",xaxt="n",ylim=c(0.7,0.85), xlab="",ylab="",cex.main=2, cex.axis=1.5, pch=16)
  # for (i in 1:n.plots) {
  #   lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
  #   lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
  #   lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
  #   
  # }
  # axis(1, at=1:length(lab), labels=FALSE)
  # text(x=1:length(lab), par()$usr[3], labels = lab, srt = 90, adj = 1.25, xpd = TRUE, cex=1.5)
  # if (fname == "bv_3class"){
  #   mtext("Brightness Values", side=3, line=1.5, cex=3)
  # }else if (fname == "rad_3class"){
  #   mtext("Radiance", side=3, line=1.5, cex=3)
  # }else if (fname == "ref_3class"){
  #   mtext("Reflectance", side=3, line=1.5, cex=3)
  # }
  # mtext("mtry-ntree", side=1, line=6.5, cex=2.5)
  # mtext("Overall Accuracy", side=2, line=3.5, cex=2.5)
  # par(op)
  # #abline(h=0.85, col="blue", lty="solid")
  # dev.off()
  # closeAllConnections()
  }
closeAllConnections()





## -------------------------------------------------
print ("COMPLETE")

