#Author: Christopher Zarzar
#Created: 4-12-16

#NOTES: This script was created to set up a
#permutation test to verify my atmospheric
#correction.Therefore, it will test whether
#the means of my predicted and measured dataset
#are statistically similar using resampling
#techniques. 

test.data <- read.csv("E:/RWorkspace/datasets/atmosphericCorrections/testVerification.csv", header = TRUE)
permutationTestMeans(test.data[,1],test.data[,2],B=10000)