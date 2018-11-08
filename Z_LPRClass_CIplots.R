## Author: Christopher Zarzar
## Created: 28-Nov-2016

## NOTES: This script will take the classifications and will calculate CI and plot them. 

## load library requirements

## Set up global variables
## Create a matrix of three zero placeholds for the future CIs to be attached to
plotmat <- numeric(3)
count <- 0
files <- list.files(path="C:/Users/Chris/Desktop/lpr_classifications", pattern="*.txt", full.names = TRUE, recursive = FALSE)

for (currentFile in files){
  fileBase = basename(currentFile)
  firstThreeCharacters <- substr(fileBase, 1, 3)
  if (count == 0){
    currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
    currentFile.ci <- quantile(currentFile.data[,3],probs=c(0.025,0.5,0.975))
    plotmat <- cbind(plotmat,currentFile.ci)
    plot.title <- fileBase
  }else if (firstThreeCharacters == previousFileFirstThree){
    currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
    currentFile.ci <- quantile(currentFile.data[,3],probs=c(0.025,0.5,0.975))
    plotmat <- cbind(plotmat,currentFile.ci)
    plot.title <- fileBase
  }else{
    png(file =paste('C:/Users/Chris/Desktop/lpr_classifications/outimgs/',firstThreeCharacters,'.png', sep=''))
    ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
    plotmat <- plotmat[,-1]
    xrange <- seq(0.05,1,by=0.05)
    #yrange <- seq(10000,80000,by=10000)
    barwidth=0.2
    n.plots <- length(plotmat[1,])
    plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.8,1), xlab="",ylab="",cex.main=3, cex.axis=2, cex.lab=2, pch=16)
    for (i in 1:n.plots) {
      lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
      lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
      lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
      
    }
    axis(1, at=1:20, labels=xrange, cex.axis=2, las=2)
#     abline(h=plotmat[2,1], col="blue")
#     abline(h=plotmat[1,1],lty= 2, col="red")
#     abline(h=plotmat[3,1],lty= 2, col="red")
    dev.off()

    
    ## Now that the plot has been plotted and save, I can reset the plotmat for the next classification technique
    plotmat <- numeric(3)
  }
  previousFileFirstThree <- firstThreeCharacters
  count <- count+1
}

print ("Program complete")
closeAllConnections()
## END






