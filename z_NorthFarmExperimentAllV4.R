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
#EDITED on 11-06-2016 CZ: ADjusted script so it outputs individual pngs. 
#EDITED on 11-07-2016 CZ: Ajusted for the Canon Camera
#EDITED on 08-22-2017 CZ: Set up script for all three cameras used in revised North Farm experiment
#EDITED on 08-22-2017 CZ: Adjust ifelse statement to only make those values NA that are not equal to the alt now that I have that all fixed and set
#EDITED on 09-11-2017 CZ: Set up to do just altitude 100-800. 


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


source('D:/Research/RWorkspace/rcodes/plot.ci.R')


# Read in the data
panel06.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined06%_Canon.csv", header = TRUE)
panel22.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined22%_Canon.csv", header = TRUE)
panel44.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined44%_Canon.csv", header = TRUE)

# Output all returned values to text file
sink("D:/Research/RWorkspace/NorthFarm_output/NorthFarmTest_Canon.txt")
#pdf(file = "E:/Research/RWorkspace/NorthFarmPlots_Canon.pdf")

# Create multipanel image for the plots, Need to work on getting just one axis to plot across and putting on letter labels
#par(mfrow = c(5,3))
#par(mar = c(2, 1, 2, 1), oma = c(1, 1, 0, 0))


# The analysis for the 6% reflectance panel
# Organize the data by altitude
set.seed(7)
tarp <- c('06', '22', '44')
alt <- c('100','200','300','400','500','600','700','800')#'30',
band <- c('Green', 'Red','NIR')
band.col <- c(3:5)
for (k in 1:length(band.col)){
  for(j in 1:length(tarp)){
    plotmat <- numeric(3)
    for (i in 1:length(alt)){
      
working.cmd <- paste('working.data <- panel',tarp[j],'.data',sep='')
eval(parse(text=working.cmd))
level <- as.double(alt[i])
panel.tmp <- ifelse(working.data[,2]==level,working.data[,band.col[k]],NA)
panel.tmp <- panel.tmp[!is.na(panel.tmp)]
if (i==1){
  panel.sfc <- panel.tmp
}else{
  permtest <- permutationTestMeans(panel.sfc,panel.tmp,B=10000)
  print(permtest)
  if(permtest < .05){
    print(paste('Test is significant. We can state that the dataset for the ',band[k],' band ',tarp[j],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
  }else{
    print(paste('Test is not significant. We cannot state that the dataset for the ',band[k],' band ',tarp[j],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
  }
}     

# Run bootstrap confidence interval on data for 6% panel

boot.tmp <- boot(panel.tmp,mean.boot,R=1000)
ci.cmd <- paste('ci.',tarp[j],'_',alt[i],'.',band[k],'<- quantile(boot.tmp$t,probs=c(0.025,0.5,0.975))',sep='')
eval(parse(text=ci.cmd))

#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)

plotmat.cmd <- paste('plotmat <- cbind(plotmat,','ci.',tarp[j],'_',alt[i],'.',band[k],')',sep='')
eval(parse(text=plotmat.cmd))

    }
    # cbind the confidence intervals together and make plots of altitude CI'S
    
    #Plot the CIs
    png(file =paste('D:/Research/RWorkspace/NorthFarm_output/NorthFarmPlots_Canon_',band[k],'_',tarp[j], '.png', sep=''))
    plotmat <- plotmat[,-1]
    xrange <- c((seq(100,800,by=100))) #30,
    #yrange <- seq(10000,80000,by=10000)
    barwidth=0.2
    plot.title <- paste(band[k],' Band ',tarp[j], '% Panel', sep='')
    n.plots <- length(plotmat[1,])
    plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0,255), xlab="",ylab="",cex.main=3, cex.axis=2, cex.lab=2, pch=16)
    for (i in 1:n.plots) {
         lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
         lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
         lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))

    }
    axis(1, at=1:8, labels=xrange, cex.axis=2, las=2) #change at= to 1:9 when doing 30 - 800 ft
    abline(h=plotmat[2,1], col="blue")
    abline(h=plotmat[1,1],lty= 2, col="red")
    abline(h=plotmat[3,1],lty= 2, col="red")
    dev.off()
  }
  
}
## COMMENT THE BELOW BACK IN IF YOU WANT IT IN AN ENTIRE SINGLE FILE. 
# dev.off()
closeAllConnections()
#END


###################****************######################
###################****************######################
###################****************######################


## Start for the Phantom Camera
# Read in the data
panel06.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined06%_Phantom.csv", header = TRUE)
panel22.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined22%_Phantom.csv", header = TRUE)
panel44.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined44%_Phantom.csv", header = TRUE)

# Output all returned values to text file
sink("D:/Research/RWorkspace/NorthFarm_output/NorthFarmTest_Phantom.txt")
#pdf(file = "E:/Research/RWorkspace/NorthFarmPlots_Canon.pdf")

# Create multipanel image for the plots, Need to work on getting just one axis to plot across and putting on letter labels
#par(mfrow = c(5,3))
#par(mar = c(2, 1, 2, 1), oma = c(1, 1, 0, 0))

# The analysis
# Organize the data by altitude
set.seed(7)
tarp <- c('06', '22', '44')
alt <- c('100','200','300','400') #'30',
band <- c('Blue','Green', 'Red')
band.col <- c(3:5)
for (k in 1:length(band.col)){
  for(j in 1:length(tarp)){
    plotmat <- numeric(3)
    for (i in 1:length(alt)){
      
      working.cmd <- paste('working.data <- panel',tarp[j],'.data',sep='')
      eval(parse(text=working.cmd))
      level <- as.double(alt[i])
      panel.tmp <- ifelse(working.data[,2]==level,working.data[,band.col[k]],NA)
      panel.tmp <- panel.tmp[!is.na(panel.tmp)]
      if (i==1){
        panel.sfc <- panel.tmp
      }else{
        permtest <- permutationTestMeans(panel.sfc,panel.tmp,B=10000)
        print(permtest)
        if(permtest < .05){
          print(paste('Test is significant. We can state that the dataset for the ',band[k],' band ',tarp[j],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
        }else{
          print(paste('Test is not significant. We cannot state that the dataset for the ',band[k],' band ',tarp[j],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
        }
      }     
      
      # Run bootstrap confidence interval on data for 6% panel
      
      boot.tmp <- boot(panel.tmp,mean.boot,R=1000)
      ci.cmd <- paste('ci.',tarp[j],'_',alt[i],'.',band[k],'<- quantile(boot.tmp$t,probs=c(0.025,0.5,0.975))',sep='')
      eval(parse(text=ci.cmd))
      
      #Now run a permutation test to determine whether the two dataset samples come from the same population
      #Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
      
      plotmat.cmd <- paste('plotmat <- cbind(plotmat,','ci.',tarp[j],'_',alt[i],'.',band[k],')',sep='')
      eval(parse(text=plotmat.cmd))
      
    }
    # cbind the confidence intervals together and make plots of altitude CI'S
    
    #Plot the CIs
    png(file =paste('D:/Research/RWorkspace/NorthFarm_output/NorthFarmPlots_Phantom_',band[k],'_',tarp[j], '.png', sep=''))
    plotmat <- plotmat[,-1]
    xrange <- c((seq(100,400,by=100)))
    #yrange <- seq(10000,80000,by=10000)
    barwidth=0.2
    plot.title <- paste(band[k],' Band ',tarp[j], '% Panel', sep='')
    n.plots <- length(plotmat[1,])
    plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(0,255), xlab="",ylab="",cex.main=3, cex.axis=2, cex.lab=2, pch=16)
    for (i in 1:n.plots) {
      lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
      lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
      lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
      
    }
    axis(1, at=1:4, labels=xrange, cex.axis=2, las=2)
    abline(h=plotmat[2,1], col="blue")
    abline(h=plotmat[1,1],lty= 2, col="red")
    abline(h=plotmat[3,1],lty= 2, col="red")
    dev.off()
  }
  
}
## COMMENT THE BELOW BACK IN IF YOU WANT IT IN AN ENTIRE SINGLE FILE. 
# dev.off()
closeAllConnections()
#END


###################****************######################
###################****************######################
###################****************######################


##Start script for the MICASENSE CAMERA
# Read in the data
panel06.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined06%_MicaSense.csv", header = TRUE)
panel22.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined22%_MicaSense.csv", header = TRUE)
panel44.data <- read.csv("D:/Research/RWorkspace/datasets/AltitudesCombined44%_MicaSense.csv", header = TRUE)

# Output all returned values to text file
sink("D:/Research/RWorkspace/NorthFarm_output/NorthFarmTest_Mica.txt")
#pdf(file = "E:/Research/RWorkspace/NorthFarmPlots_Mica.pdf")
#png(file = "E:/Research/RWorkspace/NorthFarmPlots_Mica.png")

# Create multipanel image for the plots, Need to work on getting just one axis to plot across and putting on letter labels
#par(mfrow = c(5,3))
#par(mar = c(2, 1, 2, 1), oma = c(1, 1, 0, 0))

# The analysis for all altitudes and panel
set.seed(7)
tarp <- c('06', '22', '44')
alt <- c('100','200','300','400','500','600','700','800')
band <- c('Blue', 'Green', 'Red', 'rEdge','NIR')
band.col <- c(3:7)
for (k in 1:length(band.col)){
  for(j in 1:length(tarp)){
    plotmat <- numeric(3)
    for (i in 1:length(alt)){
      
      working.cmd <- paste('working.data <- panel',tarp[j],'.data',sep='')
      eval(parse(text=working.cmd))
      level <- as.double(alt[i])
      panel.tmp <- ifelse(working.data[,2]==level,working.data[,band.col[k]],NA)
      panel.tmp <- panel.tmp[!is.na(panel.tmp)]
      if (i==1){
        panel.sfc <- panel.tmp
      }else{
        permtest <- permutationTestMeans(panel.sfc,panel.tmp,B=10000)
        print(permtest)
        if(permtest < .05){
          print(paste('Test is significant. We can state that the dataset for the ',band[k],' band ',tarp[j],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
        }else{
          print(paste('Test is not significant. We cannot state that the dataset for the ',band[k],' band ',tarp[j],' panel at ', alt[i],' feet is different than the ground reference data',sep=''))
        }
      }     
      
      # Run bootstrap confidence interval on data for 6% panel
      
      boot.tmp <- boot(panel.tmp,mean.boot,R=1000)
      ci.cmd <- paste('ci.',tarp[j],'_',alt[i],'.',band[k],'<- quantile(boot.tmp$t,probs=c(0.025,0.5,0.975))',sep='')
      eval(parse(text=ci.cmd))
      
      #Now run a permutation test to determine whether the two dataset samples come from the same population
      #Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
      
      plotmat.cmd <- paste('plotmat <- cbind(plotmat,','ci.',tarp[j],'_',alt[i],'.',band[k],')',sep='')
      eval(parse(text=plotmat.cmd))
      
    }
    # cbind the confidence intervals together and make plots of altitude CI'S
    
    #Plot the CIs
    png(file =paste('D:/Research/RWorkspace/NorthFarm_output/NorthFarmPlots_Mica_',band[k],'_',tarp[j], '.png', sep=''))
    plotmat <- plotmat[,-1]
    xrange <- c((seq(100,800,by=100)))
    yrange <- seq(10000,80000,by=10000)
    barwidth=0.2
    plot.title <- paste(band[k],' Band ',tarp[j], '% Panel', sep='')
    n.plots <- length(plotmat[1,])
    #plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(10000,80000), xlab="Altitude AGL (feet)",ylab="Radiance",cex.main=1, cex.axis=.90, cex.lab=.90, pch=16)
    plot(plotmat[2,],main=plot.title,xaxt="n",ylim=c(10000,80000), xlab="",ylab="",cex.main=3, cex.axis=2, cex.lab=2, pch=16)
    for (i in 1:n.plots) {
      lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
      lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
      lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
      
    }
    axis(1, at=1:8, labels=xrange, cex.axis=2, las=2)
    abline(h=plotmat[2,1], col="blue")
    abline(h=plotmat[1,1],lty= 2, col="red")
    abline(h=plotmat[3,1],lty= 2, col="red")
    dev.off()
  }
  
}
## COMMET THE BELOW BACK IN IF YOU WANT IT IN AN ENTIRE SINGLE FILE. 
# dev.off()
closeAllConnections()
#END
