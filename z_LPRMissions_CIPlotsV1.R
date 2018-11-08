## Author: Christopher Zarzar
## Created: 20-Mar-2017

## NOTES: This script will take the classifications from the different missions and 
##will calculate 95% CIs and plot them. This was adapted from LPRClass_CIPlotsV4.R 


## Set up global variables
mainPath <- "C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/"

## Set paths
outClassPath <- paste(mainPath,'lpr/lpr_classification_output/outimgs/',sep='')

file.names <- c('dec2014','aug2015','dec2015','mar2015','may2015')
for (fname in file.names){
  ## List and sort the files
  file.cmd <- paste('files <- file.info(list.files(path="',mainPath,'/lpr/lpr_classification_output/',fname,'", pattern="*.txt", full.names = TRUE, recursive = FALSE))', sep='')
  eval(parse(text=file.cmd))
  details <- files[with(files, order(as.POSIXct(mtime))), ]
  fileList <- rownames(details)
  
  ## Set up global variables
  ## Create a matrix of three zero placeholds for the future CIs to be attached to
  plotmat <- numeric(3)
  count <- 0
  
  for (currentFile in fileList){
    fileBase = basename(currentFile)
    firstThreeCharacters <- substr(fileBase, 1, 3)
    if (firstThreeCharacters == "BV_"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmat <- cbind(plotmat,currentFile.ci)
      plot.title <- fname
    }else if (firstThreeCharacters == "Rad"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmat <- cbind(plotmat,currentFile.ci)
    }else if (firstThreeCharacters == "Ref"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.ci <- quantile(currentFile.data[,4],probs=c(0.025,0.5,0.975))
      plotmat <- cbind(plotmat,currentFile.ci)
    }
    count <- count+1
  }
  #### Plot CIs ####
  outPut.cmd <- paste('png(file="',outClassPath,fname,'.png")',sep='')
  eval(parse(text=outPut.cmd))
  ## Remove the first useless row of placeholder zeros at the start of the plotmat matrix
  plotmat <- plotmat[,-1]
  xrange <- c('BV', 'Rad', 'Ref')
  barwidth=0.075
  n.plots <- length(plotmat[1,])
  plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0.40,1), xlab="",ylab="",cex.main=2, cex.axis=1.5, cex.lab=1.5, pch=16)
  for (i in 1:n.plots) {
    lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
    lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
    lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
    
  }
  axis(1, at=1:3, labels=xrange, cex.axis=1.5, las=2)
  abline(h=0.85, col="blue", lty="solid")
  abline(h=0.82, col="blue", lty="dashed")
  abline(h=0.81, col="blue", lty="dotted")
  abline(h=0.75, col="blue", lty="dotdash")
  
  dev.off()
  
 
  
  closeAllConnections()
}
print ("Program complete")

## END






