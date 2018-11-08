## Author: Christopher Zarzar
## Created: 28-Nov-2016

## NOTES: This script will take the classifications and will calculate CI and plot them. 

## Edit: CZ 1-Dec-16: Changed it so it is less sophisicated but it works. Forces it to go through each classification
## Edit: CZ 8-Feb-17: Added a loop so that I could more quickly work through the BV, Rad, and Ref data. 
##Also changed the row to plot average accuracy
##Also split between 2 and 3 class results so I could use different threshold lines


## load library requirements



file.names <- c('bv_2class', 'rad_2class', 'ref_2class')
for (fname in file.names){
## List and sort the files
file.cmd <- paste('files <- file.info(list.files(path="C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_percent_classifications/',fname,'", pattern="*.txt", full.names = TRUE, recursive = FALSE))', sep='')
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
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmatlog <- cbind(plotmatlog,currentFile.ci)
      logplot.title <- fileBase
    }else if (firstThreeCharacters == "neu"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmatneu <- cbind(plotmatneu,currentFile.ci)
      neuplot.title <- fileBase
    }else if (firstThreeCharacters == "ran"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmatran <- cbind(plotmatran,currentFile.ci)
      ranplot.title <- fileBase
    }else if (firstThreeCharacters == "max"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmatmax <- cbind(plotmatmax,currentFile.ci)
      maxplot.title <- fileBase
    }else if (firstThreeCharacters == "sup"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmatsup <- cbind(plotmatsup,currentFile.ci)
      supplot.title <- fileBase
    }
    count <- count+1
  }
  #### Plot for logistic classifier ####
  outPut.cmd <- paste('png(file="C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_percent_classifications/outimgs/Logit_',fname,'.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotmat <- plotmatlog
  plot.title <- logplot.title
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  xrange <- seq(0.05,1,by=0.05)
  barwidth=0.2
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.60,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
    
  }
  axis(1, at=1:20, labels=xrange, cex.axis=1.5, las=2)
  abline(h=0.85, col="blue", lty="solid")
  
  dev.off()
  
  #### Plot for neural network classifier #### 
  outPut.cmd <- paste('png(file ="C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_percent_classifications/outimgs/NeuralNetwork_',fname,'.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotmat <- plotmatneu
  plot.title <- neuplot.title
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  xrange <- seq(0.05,1,by=0.05)
  barwidth=0.2
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.60,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
    
  }
  axis(1, at=1:20, labels=xrange, cex.axis=1.5, las=2)
  abline(h=0.85, col="blue", lty="solid")
  dev.off()
  
  ## Plot for random trees classifier 
  outPut.cmd <- paste('png(file ="C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_percent_classifications/outimgs/randomTrees_',fname,'.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotmat <- plotmatran
  plot.title <- ranplot.title
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  xrange <- seq(0.05,1,by=0.05)
  barwidth=0.2
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.60,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
    
  }
  axis(1, at=1:20, labels=xrange, cex.axis=1.5, las=2)
  abline(h=0.85, col="blue", lty="solid")
  dev.off()
  
  ## Plot for maximum likelihood classifier 
  outPut.cmd <- paste('png(file ="C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_percent_classifications/outimgs/maximumLikelihood_',fname,'.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotmat <- plotmatmax
  plot.title <- maxplot.title
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  xrange <- seq(0.05,1,by=0.05)
  barwidth=0.2
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.60,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
    
  }
  axis(1, at=1:20, labels=xrange, cex.axis=1.5, las=2)
  abline(h=0.85, col="blue", lty="solid")
  dev.off()
  
  ## Plot for support vector classifier 
  outPut.cmd <- paste('png(file ="C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_percent_classifications/outimgs/supportVector_',fname,'.png")',sep='')
  eval(parse(text=outPut.cmd))
  plotmat <- plotmatsup
  plot.title <- supplot.title
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  xrange <- seq(0.05,1,by=0.05)
  barwidth=0.2
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.60,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
    
  }
  axis(1, at=1:20, labels=xrange, cex.axis=1.5, las=2)
  abline(h=0.85, col="blue", lty="solid")
  dev.off()
  
  closeAllConnections()
}
print ("Program complete")

## END






