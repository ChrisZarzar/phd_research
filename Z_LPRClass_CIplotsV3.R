## Author: Christopher Zarzar
## Created: 28-Nov-2016

## NOTES: This script will take the classifications and will calculate CI and plot them. 

## EDITED CZ 12-1-16: Changed it so it is less sophisicated but it works. Forces it to go through each classification

## load library requirements

## Set up global variables
## Create a matrix of three zero placeholds for the future CIs to be attached to
plotmatlog <- numeric(3)
plotmatneu <- numeric(3)
plotmatran <- numeric(3)
plotmatmax <- numeric(3)
plotmatsup <- numeric(3)
count <- 0

## List and sort the files
files <- file.info(list.files(path="C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_sampSize_classifications/bv_2class", pattern="*.txt", full.names = TRUE, recursive = FALSE))
details <- files[with(files, order(as.POSIXct(mtime))), ]
fileList <- rownames(details)

for (currentFile in fileList){
  fileBase = basename(currentFile)
  firstThreeCharacters <- substr(fileBase, 1, 3)
  if (firstThreeCharacters == "log"){
    currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
    currentFile.ci <- quantile(currentFile.data[,3],probs=c(0.025,0.5,0.975))
    plotmatlog <- cbind(plotmatlog,currentFile.ci)
    logplot.title <- fileBase
  }else if (firstThreeCharacters == "neu"){
    currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
    currentFile.ci <- quantile(currentFile.data[,3],probs=c(0.025,0.5,0.975))
    plotmatneu <- cbind(plotmatneu,currentFile.ci)
    neuplot.title <- fileBase
  }else if (firstThreeCharacters == "ran"){
    currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
    currentFile.ci <- quantile(currentFile.data[,3],probs=c(0.025,0.5,0.975))
    plotmatran <- cbind(plotmatran,currentFile.ci)
    ranplot.title <- fileBase
  }else if (firstThreeCharacters == "max"){
    currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
    currentFile.ci <- quantile(currentFile.data[,3],probs=c(0.025,0.5,0.975))
    plotmatmax <- cbind(plotmatmax,currentFile.ci)
    maxplot.title <- fileBase
  }else if (firstThreeCharacters == "sup"){
    currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
    currentFile.ci <- quantile(currentFile.data[,3],probs=c(0.025,0.5,0.975))
    plotmatsup <- cbind(plotmatsup,currentFile.ci)
    supplot.title <- fileBase
  }
  count <- count+1
}
## Plot for logistic classifier 
png(file ='C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_sampSize_classifications/outimgs/logit_BV3.png')
plotmat <- plotmatlog
plot.title <- logplot.title
## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
plotmat <- plotmat[,-1]
xrange <- seq(50,320,by=30)
#yrange <- seq(10000,80000,by=10000)
barwidth=0.2
n.plots <- length(plotmat[1,])
plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.40,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
for (i in 1:n.plots) {
  lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
  lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
  lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
  
}
axis(1, at=1:10, labels=xrange, cex.axis=1.5, las=2)
abline(h=0.85, col="blue", lty="solid")
abline(h=0.82, col="blue", lty="dashed")
abline(h=0.81, col="blue", lty="dotted")
abline(h=0.75, col="blue", lty="dotdash")

dev.off()

## Plot for neural network classifier 
png(file ='C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_sampSize_classifications/outimgs/neuralNetwork_BV3.png')
plotmat <- plotmatneu
plot.title <- neuplot.title
## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
plotmat <- plotmat[,-1]
xrange <- seq(50,320,by=30)
#yrange <- seq(10000,80000,by=10000)
barwidth=0.2
n.plots <- length(plotmat[1,])
plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.40,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
for (i in 1:n.plots) {
  lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
  lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
  lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
  
}
axis(1, at=1:10, labels=xrange, cex.axis=1.5, las=2)
abline(h=0.85, col="blue", lty="solid")
abline(h=0.82, col="blue", lty="dashed")
abline(h=0.81, col="blue", lty="dotted")
abline(h=0.75, col="blue", lty="dotdash")
dev.off()

## Plot for random trees classifier 
png(file ='C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_sampSize_classifications/outimgs/randomTrees_BV3.png')
plotmat <- plotmatran
plot.title <- ranplot.title
## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
plotmat <- plotmat[,-1]
xrange <- seq(50,320,by=30)
#yrange <- seq(10000,80000,by=10000)
barwidth=0.2
n.plots <- length(plotmat[1,])
plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.40,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
for (i in 1:n.plots) {
  lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
  lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
  lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
  
}
axis(1, at=1:10, labels=xrange, cex.axis=1.5, las=2)
abline(h=0.85, col="blue", lty="solid")
abline(h=0.82, col="blue", lty="dashed")
abline(h=0.81, col="blue", lty="dotted")
abline(h=0.75, col="blue", lty="dotdash")
dev.off()

## Plot for maximum likelihood classifier 
png(file ='C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_sampSize_classifications/outimgs/maximumLikelihood_BV3.png')
plotmat <- plotmatmax
plot.title <- maxplot.title
## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
plotmat <- plotmat[,-1]
xrange <- seq(50,320,by=30)
#yrange <- seq(10000,80000,by=10000)
barwidth=0.2
n.plots <- length(plotmat[1,])
plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.40,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
for (i in 1:n.plots) {
  lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
  lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
  lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
  
}
axis(1, at=1:10, labels=xrange, cex.axis=1.5, las=2)
abline(h=0.85, col="blue", lty="solid")
abline(h=0.82, col="blue", lty="dashed")
abline(h=0.81, col="blue", lty="dotted")
abline(h=0.75, col="blue", lty="dotdash")
dev.off()

## Plot for support vector classifier 
png(file ='C:/Users/chris/Desktop/Research/RWorkspace/lpr/lpr_sampSize_classifications/outimgs/supportVector_BV3.png')
plotmat <- plotmatsup
plot.title <- supplot.title
## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
plotmat <- plotmat[,-1]
xrange <- seq(50,320,by=30)
#yrange <- seq(10000,80000,by=10000)
barwidth=0.2
n.plots <- length(plotmat[1,])
plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.40,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
for (i in 1:n.plots) {
  lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
  lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
  lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
  
}
axis(1, at=1:10, labels=xrange, cex.axis=1.5, las=2)
abline(h=0.85, col="blue", lty="solid")
abline(h=0.82, col="blue", lty="dashed")
abline(h=0.81, col="blue", lty="dotted")
abline(h=0.75, col="blue", lty="dotdash")
dev.off()

closeAllConnections()
print ("Program complete")

## END






