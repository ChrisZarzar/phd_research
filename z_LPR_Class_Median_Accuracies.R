## Author: Christopher Zarzar
## Created: 20-Mar-2017

## NOTES: This script will take given inputs and will calculate the medians for each accuracy 
## and put them into a new text file. This is for all mission and a single classifier. 
## EDIT:

## Set up global variables
#mainPath <- "C:/Users/chris/OneDrive/Desktop/Research/RWorkspace/"
mainPath <- "/home/chriszarzar/Desktop/RWorkspace/"

## Make sure required directories exist 
dir.create(paste(mainPath,'lpr/lpr_imgcorrect_class_comparison/outAcc/',sep=''), showWarnings = FALSE)

## Set paths
outClassPath <- paste(mainPath,'lpr/lpr_imgcorrect_class_comparison/outAcc/',sep='')

file.names <- c('dec2014','aug2015','dec2015','mar2015','may2015')
for (fname in file.names){
  ## List and sort the files
  file.cmd <- paste('files <- file.info(list.files(path="',mainPath,'/lpr/lpr_imgcorrect_class_comparison/',fname,'", pattern="*.txt", full.names = TRUE, recursive = FALSE))', sep='')
  eval(parse(text=file.cmd))
  details <- files[with(files, order(as.POSIXct(mtime))), ]
  fileList <- rownames(details)
  count1 <- 0
  count2 <- 0
  count3 <- 0
  for (currentFile in fileList){
    fileBase = basename(currentFile)
    firstThreeCharacters <- substr(fileBase, 1, 3)
    if (firstThreeCharacters == "BV_"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.medianList <- c(median(currentFile.data[,3]), median(currentFile.data[,4]), median(currentFile.data[,5]), median(currentFile.data[,6]), median(currentFile.data[,7]), median(currentFile.data[,8]), median(currentFile.data[,9]), median(currentFile.data[,10]))
      if (count1==0){
        write("Kappa,Overall_Acc,OW_UA, TV_UA, AV_UA, OW_PA, TV_PA, AV_PA", file=paste(outClassPath, 'median',fileBase,'.txt', sep=''), append = TRUE, sep = ",")
      }
      write(currentFile.medianList, file=paste(outClassPath, 'median',fileBase,'.txt', sep=''), append = TRUE, sep = ",")
      count1 <- count1+1
      
    }else if (firstThreeCharacters == "Rad"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.medianList <- c(median(currentFile.data[,3]), median(currentFile.data[,4]), median(currentFile.data[,5]), median(currentFile.data[,6]), median(currentFile.data[,7]), median(currentFile.data[,8]), median(currentFile.data[,9]), median(currentFile.data[,10]))
      if (count2==0){
        write("Kappa,Overall_Acc,OW_UA, TV_UA, AV_UA, OW_PA, TV_PA, AV_PA", file=paste(outClassPath, 'median',fileBase,'.txt', sep=''), append = TRUE, sep = ",")
      }
      write(currentFile.medianList, file=paste(outClassPath, 'median',fileBase,'.txt', sep=''), append = TRUE, sep = ",")
      count2 <- count2+1
      
    }else if (firstThreeCharacters == "Ref"){
      currentFile.data <- read.table(currentFile, sep="," , header=TRUE)
      currentFile.medianList <- c(median(currentFile.data[,3]),median(currentFile.data[,4]),median(currentFile.data[,5]),median(currentFile.data[,6]),median(currentFile.data[,7]),median(currentFile.data[,8]), median(currentFile.data[,9]), median(currentFile.data[,10]))
      if (count3==0){
        write("Kappa,Overall_Acc,OW_UA,TV_UA,AV_UA,OW_PA,TV_PA,AV_PA", file=paste(outClassPath, 'median',fileBase,'.txt', sep=''), append = TRUE, sep = ",")
      }
      write(currentFile.medianList, file=paste(outClassPath, 'median',fileBase,'.txt', sep=''), append = TRUE, sep = ",")
      count3 <- count3+1
    }
  }
  
  closeAllConnections()
}



print ("Program complete")

## END ##






