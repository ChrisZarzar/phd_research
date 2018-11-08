## Author: Christopher Zarzar
## Created: 10-Apr-2017

## NOTES: This script will take the classifications accuracy metrics and will calculate CI and plot them. 

## load library requirements
library("tools")


loop.files <- c('aug2015') #'dec2014','dec2015','mar2015','may2015 completed
for (d in 1:length(loop.files)){
  date = loop.files[d]
  var.name = file_path_sans_ext(date)
  date.name = substr (date,1,7)
  print (date)
## Set up global variables
mainPath <- "C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/"
#mainPath <- "/home/chriszarzar/Desktop/RWorkspace/"

## Make sure required directories exist 
#dir.create(paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep=''), showWarnings = FALSE)
dir.create(paste(mainPath,'lpr/lpr_classification_aug_260Samp/outimgs/',sep=''), showWarnings = FALSE)

## Set paths
#outImgPath <- paste(mainPath,'lpr/lpr_classification_output/',date.name,'/outimgs/',sep='') 
outImgPath <- paste(mainPath,'lpr/lpr_classification_aug_260Samp/outimgs/',sep='') 
#filePath <- paste(mainPath,'lpr/lpr_classification_output/',date.name, sep='')
filePath <- paste(mainPath,'lpr/lpr_classification_aug_260Samp/', sep='')

file.names <- c('bv_3class', 'rad_3class', 'ref_3class') #
class.name <- c('log', 'max', 'ran', 'neu', 'sup')

for (fname in file.names){
## List and sort the files
file.cmd <- paste('files <- file.info(list.files(path="',filePath,'/',fname,'", pattern="*.txt", full.names = TRUE, recursive = FALSE))', sep='')
eval(parse(text=file.cmd))
details <- files[with(files, order(as.POSIXct(mtime))), ]
fileList <- rownames(details)

## Set up global variables
## Create a matrix of three zero placeholds for the future CIs to be attached to
plotmatlog <- numeric(3)
plotmatneu <- numeric(3)
plotmatran <- numeric(3)
plotmatmax <- numeric(3)
plotmatsup <- numeric(3)
count <- 0

  for (currentFile in fileList){
    fileBase = basename(currentFile)
    firstThreeCharacters <- substr(fileBase, 1, 3)
    if (firstThreeCharacters == "log"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      for (k in c(3,4,5,6,7,8,9,10)){
        currentFile.ci <- quantile(currentFile.data[,k],probs=c(0.025,0.5,0.975))
        plotmatlog <- cbind(plotmatlog,currentFile.ci)
      }
      logplot.title <- fileBase
    }else if (firstThreeCharacters == "ran"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      for (k in c(3,4,5,6,7,8,9,10)){
        currentFile.ci <- quantile(currentFile.data[,k],probs=c(0.025,0.5,0.975))
        plotmatran <- cbind(plotmatran,currentFile.ci)
      }
      ranplot.title <- fileBase
    }else if (firstThreeCharacters == "max"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      for (k in c(3,4,5,6,7,8,9,10)){
        currentFile.ci <- quantile(currentFile.data[,k],probs=c(0.025,0.5,0.975))
        plotmatmax <- cbind(plotmatmax,currentFile.ci)
      }
      maxplot.title <- fileBase
    }else if (firstThreeCharacters == "neu"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      for (k in c(3,4,5,6,7,8,9,10)){
        currentFile.ci <- quantile(currentFile.data[,k],probs=c(0.025,0.5,0.975))
        plotmatneu <- cbind(plotmatneu,currentFile.ci)
      }
      neuplot.title <- fileBase
    }else if (firstThreeCharacters == "sup"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      for (k in c(3,4,5,6,7,8,9,10)){
        currentFile.ci <- quantile(currentFile.data[,k],probs=c(0.025,0.5,0.975))
        plotmatsup <- cbind(plotmatsup,currentFile.ci)
      }
      supplot.title <- fileBase
    }
    count <- count+1
  }
  #### Plot for classifier CIs ####
  for (classTec in class.name){
  outPut.cmd <- paste('png(file="',outImgPath,classTec,'_',fname,'.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotmat.cmd <- paste('plotmat <- plotmat',classTec, sep='')
  eval(parse(text=plotmat.cmd))
  #plot.title <- logplot.title
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  xrange <- c('Kappa','OA','UA', 'UA', 'UA', 'PA', 'PA', 'PA')
  xrange2 <- c('OW', 'TV', 'AV', 'OW', 'TV', 'AV')
  lab <- xrange
  lab2 <- xrange2
  barwidth=0.2
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,], main="",type='p',xaxt="n",ylim=c(0.50,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))

  }
  axis(1, at=c(1,2,3,4,5,6,7,8), labels=lab, cex.axis=1)
  axis(3, at=c(3,4,5,6,7,8), labels=lab2, cex.axis=1)
  # if (fname == "bv_3class"){
  #   mtext("Brightness Values", side=3, line=1.5, cex=3)
  # }else if (fname == "rad_3class"){
  #   mtext(expression(paste(plain("Radiance (W sr") ^ plain("-1"), plain(" m") ^ plain("-2"), plain(")"))), side=3, line=1.5, cex=1.5)
  # }else if (fname == "ref_3class"){
  #   mtext("Reflectance", side=3, line=1.5, cex=3)
  # }
  #mtext("Class", adj=.65, side=3, line=2.5, cex=1.5)
  #mtext("Accuracy Metric", side=1, line=5, cex=2.5)
  par(op)
  abline(h=0.85, col="blue", lty="solid")
  dev.off()
  }
  }
  closeAllConnections()
}
print ("Program complete")

## END






