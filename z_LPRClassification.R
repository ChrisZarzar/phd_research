*/#Author: Christopher Zarzar
#Created: 15-Nov-2016

#NOTES: This script classifies data from the North Farm
#August 2015 imagery. 
library("rasclass")

#This initial work simply follows the user guide to gain an understanding of the script. 
# create a vector of values of 1,2,3,4. Each values will be replicated (rep) 25 times and will alternate this replication of each value 25 times before moving on
mysample <- c(rep(rep(c(1,2), each = 25), 25), rep(rep(c(3,4), each = 25), 25)) 
# Go through all 2500 values created above and randomly select some based on the probability vector weights (read more here) to decide which to randomly select. 
mysample2 <- mysample + sample(c(0,NA),2500, replace = TRUE, prob = c(1,50))
# create two different vectors of values that go from 0 to 50 and are randomly perturbed based on the randomly addded normal distribution function values calculated
myvar1 <- rep(1:50, each = 50) + rnorm(2500, 0, 5) 
myvar2 <- rep(rep(1:50), 50) + rnorm(2500, 0, 5)
# Give the samples created from the start values based on the two variable vectors created
newdata <- data.frame(mysample2, myvar1, myvar2)

# create a raster based on newdata
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 50, nrows = 50,
xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999, 
samplename = 'mysample2')

# Classify the data using the maximum likelihood classificer
outlist <- list()
outlist[['maximumLikelihood']] <- classifyRasclass(object, method ='maximumLikelihood')
outlist[['maximumLikelihood']]

# store the data so it can be displayed
mysample2.ras <- (new('rasclassRaster'))
mysample2.ras@grid <- mysample2
mysample2.ras@nrows <- 50
mysample2.ras@ncols <- 50
mysample2.ras@xllcorner <- 0
mysample2.ras@yllcorner <- 0
mysample2.ras@cellsize <- 1
mysample2.ras@NAvalue <- -9999

# display the resolts of the classifer
# create a multipanel plot
opar <- par(mfrow = c(2,3))
image(mysample2.ras)
title('Sample Data')
for(i in 1:length(outlist)){
  image(outlist[[i]]@predictedGrid)
  title(names(outlist)[[i]])
  }
par(opar)