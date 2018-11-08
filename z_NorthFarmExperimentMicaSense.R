#Author: Christopher Zarzar
#Created: 19-Sep-2016

#NOTES: This script was createdto test
# whether the atmosphere plays a 
# statistically significant role on the 
# imagery collected by the UAS.
# This is part of the North Farm experiment
# started April 2016
#EDITED ON 10-18-2016 CZ: Discovered that I had read in the same 06% spreadsheets 3 time. Adjusted and fixed script and results
#EDITED on 10-26-2016 CZ: Worked with Mercer to simplify the scripts using loops

# Read in the data
panel06.data <- read.csv("E:/RWorkspace/datasets/AltitudesCombined06%_MicaSense_adjusted.csv", header = TRUE)
panel22.data <- read.csv("E:/RWorkspace/datasets/AltitudesCombined22%_MicaSense_adjusted.csv", header = TRUE)
panel44.data <- read.csv("E:/RWorkspace/datasets/AltitudesCombined44%_MicaSense_adjusted.csv", header = TRUE)

# Output all returned values to text file
sink("E:/RWorkspace/NorthFarmTest_Mica.txt")
pdf(file = "E:/RWorkspace/NorthFarmPlots_Mica.pdf")

# Create multipanel image for the plots
par(mfrow = c(5,3))

# Execute and load libraries and functions that I will need
permutationTestMeans <- function(x,y,B=1000) {
  n1 <- length(x)
  n2 <- length(y)
  delta <- abs(mean(x) - mean(y))
  fullset <- c(x,y)
  new.delta <- numeric(B)
  for (i in 1:B) {
    sample.1 <- sample(fullset,n1,replace=T)
    sample.2 <- sample(fullset,n2,replace=T)
    new.delta[i]<- abs(mean(sample.1) - mean(sample.2))
  }
  
  counts <- ifelse(new.delta >= delta,1,0)
  p.value <- sum(counts) / B
  return(p.value)
}

library("boot")

mean.boot <- function(x,d){
  return(mean(x[d]))
}

source('E:/RWorkspace/rcodes/plot.ci.R')

# The analysis for the 6% reflectance panel
# Organize the data by altitude
set.seed(100)
tarp <- c('06', '22', '44')
alt <- c('100','200','300','400','500','600','700','800')
band <- c('Blue', 'Green', 'Red', 'rEdge','NIR')
band.col <- c(5:9)
for (k in 1:length(tarp)){
  for(j in 1:length(band.col)){
    plotmat <- numeric(3)
    for (i in 1:length(alt)){
      
working.cmd <- paste('working.data <- panel',tarp[k],'.data',sep='')
eval(parse(text=working.cmd))
level <- as.double(alt[i])
panel.tmp <- ifelse(working.data[,3]<=level & working.data[,3]>(level-100),working.data[,band.col[j]],NA)
panel.tmp <- panel.tmp[!is.na(panel.tmp)]
if (i==1){
  panel.sfc <- panel.tmp
}else{
  permtest <- permutationTestMeans(panel.sfc,panel.tmp,B=10000)
  print(permtest)
  if(permtest < .05){
    print(paste('Test is significant. We can state that the dataset for the ',band[j],'band ',tarp[k],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
  }else{
    print(paste('Test is not significant. We cannot state that the dataset for the ',band[j],'band ',tarp[k],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
  }
}     

# Run bootstrap confidence interval on data for 6% panel

boot.tmp <- boot(panel.tmp,mean.boot,R=1000)
ci.cmd <- paste('ci.',tarp[k],'_',alt[i],'.',band[j],'<- quantile(boot.tmp$t,probs=c(0.025,0.5,0.975))',sep='')
eval(parse(text=ci.cmd))

#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)

plotmat.cmd <- paste('plotmat <- cbind(plotmat,','ci.',tarp[k],'_',alt[i],'.',band[j],')',sep='')
eval(parse(text=plotmat.cmd))
    }
    # cbind the confidence intervals together and make plots of altitude CI'S
    
    #Plot the Blue band 06% CIs
    plotmat <- plotmat[,-1]
    xticks <- c(100,200,300,400,500,600,700,800)
    plot.ci(plotmat, xaxis="n", min.y=10000, max.y=80000, xlabel=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylabel="Radiance")
    plot.title <- paste('Confidence Intervals for ',band[j],' Band ',tarp[k], ' Panel', sep='')
    title(main=plot.title, cex.main=.3)
    axis(1, at=1:8, labels=xticks, las=2)
    abline(h=plotmat[2,1], col="blue")
    abline(h=plotmat[1,1],lty= 2, col="red")
    abline(h=plotmat[3,1],lty= 2, col="red")
  }
}

dev.off()
closeAllConnections()
#END
