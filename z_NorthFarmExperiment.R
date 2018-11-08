#Author: Christopher Zarzar
#Created: 19-Sep-2016

#NOTES: This script was create to test
# whether the atmosphere plays a 
# statistically significant role on the 
# imagery collected by the UAS.
# This is part of the North Farm experiment
# started April 2016


# Read in the data
panel06.data <- read.csv("F:/RWorkspace/datasets/AltitudeCombined06%.csv", header = TRUE)
panel22.data <- read.csv("F:/RWorkspace/datasets/AltitudeCombined06%.csv", header = TRUE)
panel44.data <- read.csv("F:/RWorkspace/datasets/AltitudeCombined06%.csv", header = TRUE)

# Output all returned values to text file
sink("F:/RWorkspace/NorthFarmTest.txt")
pdf(file = "F:/RWorkspace/NorthFarmPlots.pdf")

# The analysis for the 6% reflectance panel
# Organize the data by altitude

panel06_0_Green <- ifelse(panel06.data[,3]<=0,panel06.data[,5],NA)
panel06_0_Red <- ifelse(panel06.data[,3]<=0,panel06.data[,6],NA)
panel06_0_NIR <- ifelse(panel06.data[,3]<=0,panel06.data[,7],NA)
panel06_0_Green <- panel06_0_Green[!is.na(panel06_0_Green)]
panel06_0_Red <- panel06_0_Red[!is.na(panel06_0_Red)]
panel06_0_NIR <- panel06_0_NIR[!is.na(panel06_0_NIR)]

panel06_100_Green <- ifelse(panel06.data[,3]<=100 & panel06.data[,3]>10,panel06.data[,5],NA)
panel06_100_Red <- ifelse(panel06.data[,3]<=100 & panel06.data[,3]>10,panel06.data[,6],NA)
panel06_100_NIR <- ifelse(panel06.data[,3]<=100 & panel06.data[,3]>10,panel06.data[,7],NA)
panel06_100_Green <- panel06_100_Green[!is.na(panel06_100_Green)]
panel06_100_Red <- panel06_100_Red[!is.na(panel06_100_Red)]
panel06_100_NIR <- panel06_100_NIR[!is.na(panel06_100_NIR)]

panel06_200_Green <- ifelse(panel06.data[,3]<=200 & panel06.data[,3]>100,panel06.data[,5],NA)
panel06_200_Red <- ifelse(panel06.data[,3]<=200 & panel06.data[,3]>100,panel06.data[,6],NA)
panel06_200_NIR <- ifelse(panel06.data[,3]<=200 & panel06.data[,3]>100,panel06.data[,7],NA)
panel06_200_Green <- panel06_200_Green[!is.na(panel06_200_Green)]
panel06_200_Red <- panel06_200_Red[!is.na(panel06_200_Red)]
panel06_200_NIR <- panel06_200_NIR[!is.na(panel06_200_NIR)]

panel06_300_Green <- ifelse(panel06.data[,3]<=300 & panel06.data[,3]>200,panel06.data[,5],NA)
panel06_300_Red <- ifelse(panel06.data[,3]<=300 & panel06.data[,3]>200,panel06.data[,6],NA)
panel06_300_NIR <- ifelse(panel06.data[,3]<=300 & panel06.data[,3]>200,panel06.data[,7],NA)
panel06_300_Green <- panel06_300_Green[!is.na(panel06_300_Green)]
panel06_300_Red <- panel06_300_Red[!is.na(panel06_300_Red)]
panel06_300_NIR <- panel06_300_NIR[!is.na(panel06_300_NIR)]

panel06_400_Green <- ifelse(panel06.data[,3]<=400 & panel06.data[,3]>300,panel06.data[,5],NA)
panel06_400_Red <- ifelse(panel06.data[,3]<=400 & panel06.data[,3]>300,panel06.data[,6],NA)
panel06_400_NIR <- ifelse(panel06.data[,3]<=400 & panel06.data[,3]>300,panel06.data[,7],NA)
panel06_400_Green <- panel06_400_Green[!is.na(panel06_400_Green)]
panel06_400_Red <- panel06_400_Red[!is.na(panel06_400_Red)]
panel06_400_NIR <- panel06_400_NIR[!is.na(panel06_400_NIR)]

panel06_500_Green <- ifelse(panel06.data[,3]<=500 & panel06.data[,3]>400,panel06.data[,5],NA)
panel06_500_Red <- ifelse(panel06.data[,3]<=500 & panel06.data[,3]>400,panel06.data[,6],NA)
panel06_500_NIR <- ifelse(panel06.data[,3]<=500 & panel06.data[,3]>400,panel06.data[,7],NA)
panel06_500_Green <- panel06_500_Green[!is.na(panel06_500_Green)]
panel06_500_Red <- panel06_500_Red[!is.na(panel06_500_Red)]
panel06_500_NIR <- panel06_500_NIR[!is.na(panel06_500_NIR)]

panel06_600_Green <- ifelse(panel06.data[,3]<=600 & panel06.data[,3]>500,panel06.data[,5],NA)
panel06_600_Red <- ifelse(panel06.data[,3]<=600 & panel06.data[,3]>500,panel06.data[,6],NA)
panel06_600_NIR <- ifelse(panel06.data[,3]<=600 & panel06.data[,3]>500,panel06.data[,7],NA)
panel06_600_Green <- panel06_600_Green[!is.na(panel06_600_Green)]
panel06_600_Red <- panel06_600_Red[!is.na(panel06_600_Red)]
panel06_600_NIR <- panel06_600_NIR[!is.na(panel06_600_NIR)]

panel06_700_Green <- ifelse(panel06.data[,3]<=700 & panel06.data[,3]>600,panel06.data[,5],NA)
panel06_700_Red <- ifelse(panel06.data[,3]<=700 & panel06.data[,3]>600,panel06.data[,6],NA)
panel06_700_NIR <- ifelse(panel06.data[,3]<=700 & panel06.data[,3]>600,panel06.data[,7],NA)
panel06_700_Green <- panel06_700_Green[!is.na(panel06_700_Green)]
panel06_700_Red <- panel06_700_Red[!is.na(panel06_700_Red)]
panel06_700_NIR <- panel06_700_NIR[!is.na(panel06_700_NIR)]

panel06_800_Green <- ifelse(panel06.data[,3]<=800 & panel06.data[,3]>700,panel06.data[,5],NA)
panel06_800_Red <- ifelse(panel06.data[,3]<=800 & panel06.data[,3]>700,panel06.data[,6],NA)
panel06_800_NIR <- ifelse(panel06.data[,3]<=800 & panel06.data[,3]>700,panel06.data[,7],NA)
panel06_800_Green <- panel06_800_Green[!is.na(panel06_800_Green)]
panel06_800_Red <- panel06_800_Red[!is.na(panel06_800_Red)]
panel06_800_NIR <- panel06_800_NIR[!is.na(panel06_800_NIR)]

# Run bootstrap confidence interval on data for 6% panel

boot.06_0.Green <- boot(panel06_0_Green,mean.boot,R=1000)
boot.06_0.Red <- boot(panel06_0_Red,mean.boot,R=1000)
boot.06_0.NIR <- boot(panel06_0_NIR,mean.boot,R=1000)
ci.06_0.Green <- quantile(boot.06_0.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_0.Red <- quantile(boot.06_0.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_0.NIR <- quantile(boot.06_0.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_100.Green <- boot(panel06_100_Green,mean.boot,R=1000)
boot.06_100.Red <- boot(panel06_100_Red,mean.boot,R=1000)
boot.06_100.NIR <- boot(panel06_100_NIR,mean.boot,R=1000)
ci.06_100.Green <- quantile(boot.06_100.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_100.Red <- quantile(boot.06_100.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_100.NIR <- quantile(boot.06_100.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_200.Green <- boot(panel06_200_Green,mean.boot,R=1000)
boot.06_200.Red <- boot(panel06_200_Red,mean.boot,R=1000)
boot.06_200.NIR <- boot(panel06_200_NIR,mean.boot,R=1000)
ci.06_200.Green <- quantile(boot.06_200.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_200.Red <- quantile(boot.06_200.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_200.NIR <- quantile(boot.06_200.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_300.Green <- boot(panel06_300_Green,mean.boot,R=1000)
boot.06_300.Red <- boot(panel06_300_Red,mean.boot,R=1000)
boot.06_300.NIR <- boot(panel06_300_NIR,mean.boot,R=1000)
ci.06_300.Green <- quantile(boot.06_300.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_300.Red <- quantile(boot.06_300.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_300.NIR <- quantile(boot.06_300.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_400.Green <- boot(panel06_400_Green,mean.boot,R=1000)
boot.06_400.Red <- boot(panel06_400_Red,mean.boot,R=1000)
boot.06_400.NIR <- boot(panel06_400_NIR,mean.boot,R=1000)
ci.06_400.Green <- quantile(boot.06_400.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_400.Red <- quantile(boot.06_400.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_400.NIR <- quantile(boot.06_400.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_500.Green <- boot(panel06_500_Green,mean.boot,R=1000)
boot.06_500.Red <- boot(panel06_500_Red,mean.boot,R=1000)
boot.06_500.NIR <- boot(panel06_500_NIR,mean.boot,R=1000)
ci.06_500.Green <- quantile(boot.06_500.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_500.Red <- quantile(boot.06_500.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_500.NIR <- quantile(boot.06_500.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_600.Green <- boot(panel06_600_Green,mean.boot,R=1000)
boot.06_600.Red <- boot(panel06_600_Red,mean.boot,R=1000)
boot.06_600.NIR <- boot(panel06_600_NIR,mean.boot,R=1000)
ci.06_600.Green <- quantile(boot.06_600.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_600.Red <- quantile(boot.06_600.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_600.NIR <- quantile(boot.06_600.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_700.Green <- boot(panel06_700_Green,mean.boot,R=100)
boot.06_700.Red <- boot(panel06_700_Red,mean.boot,R=1000)
boot.06_700.NIR <- boot(panel06_700_NIR,mean.boot,R=1000)
ci.06_700.Green <- quantile(boot.06_700.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_700.Red <- quantile(boot.06_700.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_700.NIR <- quantile(boot.06_700.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.06_800.Green <- boot(panel06_800_Green,mean.boot,R=1000)
boot.06_800.Red <- boot(panel06_800_Red,mean.boot,R=1000)
boot.06_800.NIR <- boot(panel06_800_NIR,mean.boot,R=1000)
ci.06_800.Green <- quantile(boot.06_800.Green$t,probs=c(0.025,0.5,0.975)) 
ci.06_800.Red <- quantile(boot.06_800.Red$t,probs=c(0.025,0.5,0.975)) 
ci.06_800.NIR <- quantile(boot.06_800.NIR$t,probs=c(0.025,0.5,0.975)) 





# The analysis for the 22% reflectance panel
# Organize the data by altitude

panel22_0_Green <- ifelse(panel22.data[,3]<=0,panel22.data[,5],NA)
panel22_0_Red <- ifelse(panel22.data[,3]<=0,panel22.data[,6],NA)
panel22_0_NIR <- ifelse(panel22.data[,3]<=0,panel22.data[,7],NA)
panel22_0_Green <- panel22_0_Green[!is.na(panel22_0_Green)]
panel22_0_Red <- panel22_0_Red[!is.na(panel22_0_Red)]
panel22_0_NIR <- panel22_0_NIR[!is.na(panel22_0_NIR)]

panel22_100_Green <- ifelse(panel22.data[,3]<=100 & panel22.data[,3]>10,panel22.data[,5],NA)
panel22_100_Red <- ifelse(panel22.data[,3]<=100 & panel22.data[,3]>10,panel22.data[,6],NA)
panel22_100_NIR <- ifelse(panel22.data[,3]<=100 & panel22.data[,3]>10,panel22.data[,7],NA)
panel22_100_Green <- panel22_100_Green[!is.na(panel22_100_Green)]
panel22_100_Red <- panel22_100_Red[!is.na(panel22_100_Red)]
panel22_100_NIR <- panel22_100_NIR[!is.na(panel22_100_NIR)]

panel22_200_Green <- ifelse(panel22.data[,3]<=200 & panel22.data[,3]>100,panel22.data[,5],NA)
panel22_200_Red <- ifelse(panel22.data[,3]<=200 & panel22.data[,3]>100,panel22.data[,6],NA)
panel22_200_NIR <- ifelse(panel22.data[,3]<=200 & panel22.data[,3]>100,panel22.data[,7],NA)
panel22_200_Green <- panel22_200_Green[!is.na(panel22_200_Green)]
panel22_200_Red <- panel22_200_Red[!is.na(panel22_200_Red)]
panel22_200_NIR <- panel22_200_NIR[!is.na(panel22_200_NIR)]

panel22_300_Green <- ifelse(panel22.data[,3]<=300 & panel22.data[,3]>200,panel22.data[,5],NA)
panel22_300_Red <- ifelse(panel22.data[,3]<=300 & panel22.data[,3]>200,panel22.data[,6],NA)
panel22_300_NIR <- ifelse(panel22.data[,3]<=300 & panel22.data[,3]>200,panel22.data[,7],NA)
panel22_300_Green <- panel22_300_Green[!is.na(panel22_300_Green)]
panel22_300_Red <- panel22_300_Red[!is.na(panel22_300_Red)]
panel22_300_NIR <- panel22_300_NIR[!is.na(panel22_300_NIR)]

panel22_400_Green <- ifelse(panel22.data[,3]<=400 & panel22.data[,3]>300,panel22.data[,5],NA)
panel22_400_Red <- ifelse(panel22.data[,3]<=400 & panel22.data[,3]>300,panel22.data[,6],NA)
panel22_400_NIR <- ifelse(panel22.data[,3]<=400 & panel22.data[,3]>300,panel22.data[,7],NA)
panel22_400_Green <- panel22_400_Green[!is.na(panel22_400_Green)]
panel22_400_Red <- panel22_400_Red[!is.na(panel22_400_Red)]
panel22_400_NIR <- panel22_400_NIR[!is.na(panel22_400_NIR)]

panel22_500_Green <- ifelse(panel22.data[,3]<=500 & panel22.data[,3]>400,panel22.data[,5],NA)
panel22_500_Red <- ifelse(panel22.data[,3]<=500 & panel22.data[,3]>400,panel22.data[,6],NA)
panel22_500_NIR <- ifelse(panel22.data[,3]<=500 & panel22.data[,3]>400,panel22.data[,7],NA)
panel22_500_Green <- panel22_500_Green[!is.na(panel22_500_Green)]
panel22_500_Red <- panel22_500_Red[!is.na(panel22_500_Red)]
panel22_500_NIR <- panel22_500_NIR[!is.na(panel22_500_NIR)]

panel22_600_Green <- ifelse(panel22.data[,3]<=600 & panel22.data[,3]>500,panel22.data[,5],NA)
panel22_600_Red <- ifelse(panel22.data[,3]<=600 & panel22.data[,3]>500,panel22.data[,6],NA)
panel22_600_NIR <- ifelse(panel22.data[,3]<=600 & panel22.data[,3]>500,panel22.data[,7],NA)
panel22_600_Green <- panel22_600_Green[!is.na(panel22_600_Green)]
panel22_600_Red <- panel22_600_Red[!is.na(panel22_600_Red)]
panel22_600_NIR <- panel22_600_NIR[!is.na(panel22_600_NIR)]

panel22_700_Green <- ifelse(panel22.data[,3]<=700 & panel22.data[,3]>600,panel22.data[,5],NA)
panel22_700_Red <- ifelse(panel22.data[,3]<=700 & panel22.data[,3]>600,panel22.data[,6],NA)
panel22_700_NIR <- ifelse(panel22.data[,3]<=700 & panel22.data[,3]>600,panel22.data[,7],NA)
panel22_700_Green <- panel22_700_Green[!is.na(panel22_700_Green)]
panel22_700_Red <- panel22_700_Red[!is.na(panel22_700_Red)]
panel22_700_NIR <- panel22_700_NIR[!is.na(panel22_700_NIR)]

panel22_800_Green <- ifelse(panel22.data[,3]<=800 & panel22.data[,3]>700,panel22.data[,5],NA)
panel22_800_Red <- ifelse(panel22.data[,3]<=800 & panel22.data[,3]>700,panel22.data[,6],NA)
panel22_800_NIR <- ifelse(panel22.data[,3]<=800 & panel22.data[,3]>700,panel22.data[,7],NA)
panel22_800_Green <- panel22_800_Green[!is.na(panel22_800_Green)]
panel22_800_Red <- panel22_800_Red[!is.na(panel22_800_Red)]
panel22_800_NIR <- panel22_800_NIR[!is.na(panel22_800_NIR)]

# Run bootstrap confidence interval on data for 22% panel

boot.22_0.Green <- boot(panel22_0_Green,mean.boot,R=100)
boot.22_0.Red <- boot(panel22_0_Red,mean.boot,R=1000)
boot.22_0.NIR <- boot(panel22_0_NIR,mean.boot,R=1000)
ci.22_0.Green <- quantile(boot.22_0.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_0.Red <- quantile(boot.22_0.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_0.NIR <- quantile(boot.22_0.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_100.Green <- boot(panel22_100_Green,mean.boot,R=1000)
boot.22_100.Red <- boot(panel22_100_Red,mean.boot,R=1000)
boot.22_100.NIR <- boot(panel22_100_NIR,mean.boot,R=1000)
ci.22_100.Green <- quantile(boot.22_100.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_100.Red <- quantile(boot.22_100.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_100.NIR <- quantile(boot.22_100.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_200.Green <- boot(panel22_200_Green,mean.boot,R=1000)
boot.22_200.Red <- boot(panel22_200_Red,mean.boot,R=1000)
boot.22_200.NIR <- boot(panel22_200_NIR,mean.boot,R=1000)
ci.22_200.Green <- quantile(boot.22_200.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_200.Red <- quantile(boot.22_200.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_200.NIR <- quantile(boot.22_200.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_300.Green <- boot(panel22_300_Green,mean.boot,R=1000)
boot.22_300.Red <- boot(panel22_300_Red,mean.boot,R=1000)
boot.22_300.NIR <- boot(panel22_300_NIR,mean.boot,R=1000)
ci.22_300.Green <- quantile(boot.22_300.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_300.Red <- quantile(boot.22_300.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_300.NIR <- quantile(boot.22_300.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_400.Green <- boot(panel22_400_Green,mean.boot,R=1000)
boot.22_400.Red <- boot(panel22_400_Red,mean.boot,R=1000)
boot.22_400.NIR <- boot(panel22_400_NIR,mean.boot,R=1000)
ci.22_400.Green <- quantile(boot.22_400.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_400.Red <- quantile(boot.22_400.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_400.NIR <- quantile(boot.22_400.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_500.Green <- boot(panel22_500_Green,mean.boot,R=1000)
boot.22_500.Red <- boot(panel22_500_Red,mean.boot,R=1000)
boot.22_500.NIR <- boot(panel22_500_NIR,mean.boot,R=1000)
ci.22_500.Green <- quantile(boot.22_500.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_500.Red <- quantile(boot.22_500.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_500.NIR <- quantile(boot.22_500.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_600.Green <- boot(panel22_600_Green,mean.boot,R=1000)
boot.22_600.Red <- boot(panel22_600_Red,mean.boot,R=1000)
boot.22_600.NIR <- boot(panel22_600_NIR,mean.boot,R=1000)
ci.22_600.Green <- quantile(boot.22_600.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_600.Red <- quantile(boot.22_600.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_600.NIR <- quantile(boot.22_600.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_700.Green <- boot(panel22_700_Green,mean.boot,R=100)
boot.22_700.Red <- boot(panel22_700_Red,mean.boot,R=1000)
boot.22_700.NIR <- boot(panel22_700_NIR,mean.boot,R=1000)
ci.22_700.Green <- quantile(boot.22_700.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_700.Red <- quantile(boot.22_700.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_700.NIR <- quantile(boot.22_700.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.22_800.Green <- boot(panel22_800_Green,mean.boot,R=1000)
boot.22_800.Red <- boot(panel22_800_Red,mean.boot,R=1000)
boot.22_800.NIR <- boot(panel22_800_NIR,mean.boot,R=1000)
ci.22_800.Green <- quantile(boot.22_800.Green$t,probs=c(0.025,0.5,0.975)) 
ci.22_800.Red <- quantile(boot.22_800.Red$t,probs=c(0.025,0.5,0.975)) 
ci.22_800.NIR <- quantile(boot.22_800.NIR$t,probs=c(0.025,0.5,0.975)) 


# The analysis for the 44% reflectance panel
# Organize the data by altitude

panel44_0_Green <- ifelse(panel44.data[,3]<=0,panel44.data[,5],NA)
panel44_0_Red <- ifelse(panel44.data[,3]<=0,panel44.data[,6],NA)
panel44_0_NIR <- ifelse(panel44.data[,3]<=0,panel44.data[,7],NA)
panel44_0_Green <- panel44_0_Green[!is.na(panel44_0_Green)]
panel44_0_Red <- panel44_0_Red[!is.na(panel44_0_Red)]
panel44_0_NIR <- panel44_0_NIR[!is.na(panel44_0_NIR)]

panel44_100_Green <- ifelse(panel44.data[,3]<=100 & panel44.data[,3]>10,panel44.data[,5],NA)
panel44_100_Red <- ifelse(panel44.data[,3]<=100 & panel44.data[,3]>10,panel44.data[,6],NA)
panel44_100_NIR <- ifelse(panel44.data[,3]<=100 & panel44.data[,3]>10,panel44.data[,7],NA)
panel44_100_Green <- panel44_100_Green[!is.na(panel44_100_Green)]
panel44_100_Red <- panel44_100_Red[!is.na(panel44_100_Red)]
panel44_100_NIR <- panel44_100_NIR[!is.na(panel44_100_NIR)]

panel44_200_Green <- ifelse(panel44.data[,3]<=200 & panel44.data[,3]>100,panel44.data[,5],NA)
panel44_200_Red <- ifelse(panel44.data[,3]<=200 & panel44.data[,3]>100,panel44.data[,6],NA)
panel44_200_NIR <- ifelse(panel44.data[,3]<=200 & panel44.data[,3]>100,panel44.data[,7],NA)
panel44_200_Green <- panel44_200_Green[!is.na(panel44_200_Green)]
panel44_200_Red <- panel44_200_Red[!is.na(panel44_200_Red)]
panel44_200_NIR <- panel44_200_NIR[!is.na(panel44_200_NIR)]

panel44_300_Green <- ifelse(panel44.data[,3]<=300 & panel44.data[,3]>200,panel44.data[,5],NA)
panel44_300_Red <- ifelse(panel44.data[,3]<=300 & panel44.data[,3]>200,panel44.data[,6],NA)
panel44_300_NIR <- ifelse(panel44.data[,3]<=300 & panel44.data[,3]>200,panel44.data[,7],NA)
panel44_300_Green <- panel44_300_Green[!is.na(panel44_300_Green)]
panel44_300_Red <- panel44_300_Red[!is.na(panel44_300_Red)]
panel44_300_NIR <- panel44_300_NIR[!is.na(panel44_300_NIR)]

panel44_400_Green <- ifelse(panel44.data[,3]<=400 & panel44.data[,3]>300,panel44.data[,5],NA)
panel44_400_Red <- ifelse(panel44.data[,3]<=400 & panel44.data[,3]>300,panel44.data[,6],NA)
panel44_400_NIR <- ifelse(panel44.data[,3]<=400 & panel44.data[,3]>300,panel44.data[,7],NA)
panel44_400_Green <- panel44_400_Green[!is.na(panel44_400_Green)]
panel44_400_Red <- panel44_400_Red[!is.na(panel44_400_Red)]
panel44_400_NIR <- panel44_400_NIR[!is.na(panel44_400_NIR)]

panel44_500_Green <- ifelse(panel44.data[,3]<=500 & panel44.data[,3]>400,panel44.data[,5],NA)
panel44_500_Red <- ifelse(panel44.data[,3]<=500 & panel44.data[,3]>400,panel44.data[,6],NA)
panel44_500_NIR <- ifelse(panel44.data[,3]<=500 & panel44.data[,3]>400,panel44.data[,7],NA)
panel44_500_Green <- panel44_500_Green[!is.na(panel44_500_Green)]
panel44_500_Red <- panel44_500_Red[!is.na(panel44_500_Red)]
panel44_500_NIR <- panel44_500_NIR[!is.na(panel44_500_NIR)]

panel44_600_Green <- ifelse(panel44.data[,3]<=600 & panel44.data[,3]>500,panel44.data[,5],NA)
panel44_600_Red <- ifelse(panel44.data[,3]<=600 & panel44.data[,3]>500,panel44.data[,6],NA)
panel44_600_NIR <- ifelse(panel44.data[,3]<=600 & panel44.data[,3]>500,panel44.data[,7],NA)
panel44_600_Green <- panel44_600_Green[!is.na(panel44_600_Green)]
panel44_600_Red <- panel44_600_Red[!is.na(panel44_600_Red)]
panel44_600_NIR <- panel44_600_NIR[!is.na(panel44_600_NIR)]

panel44_700_Green <- ifelse(panel44.data[,3]<=700 & panel44.data[,3]>600,panel44.data[,5],NA)
panel44_700_Red <- ifelse(panel44.data[,3]<=700 & panel44.data[,3]>600,panel44.data[,6],NA)
panel44_700_NIR <- ifelse(panel44.data[,3]<=700 & panel44.data[,3]>600,panel44.data[,7],NA)
panel44_700_Green <- panel44_700_Green[!is.na(panel44_700_Green)]
panel44_700_Red <- panel44_700_Red[!is.na(panel44_700_Red)]
panel44_700_NIR <- panel44_700_NIR[!is.na(panel44_700_NIR)]

panel44_800_Green <- ifelse(panel44.data[,3]<=800 & panel44.data[,3]>700,panel44.data[,5],NA)
panel44_800_Red <- ifelse(panel44.data[,3]<=800 & panel44.data[,3]>700,panel44.data[,6],NA)
panel44_800_NIR <- ifelse(panel44.data[,3]<=800 & panel44.data[,3]>700,panel44.data[,7],NA)
panel44_800_Green <- panel44_800_Green[!is.na(panel44_800_Green)]
panel44_800_Red <- panel44_800_Red[!is.na(panel44_800_Red)]
panel44_800_NIR <- panel44_800_NIR[!is.na(panel44_800_NIR)]

# Run bootstrap confidence interval on data for 44% panel

boot.44_0.Green <- boot(panel44_0_Green,mean.boot,R=100)
boot.44_0.Red <- boot(panel44_0_Red,mean.boot,R=1000)
boot.44_0.NIR <- boot(panel44_0_NIR,mean.boot,R=1000)
ci.44_0.Green <- quantile(boot.44_0.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_0.Red <- quantile(boot.44_0.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_0.NIR <- quantile(boot.44_0.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_100.Green <- boot(panel44_100_Green,mean.boot,R=1000)
boot.44_100.Red <- boot(panel44_100_Red,mean.boot,R=1000)
boot.44_100.NIR <- boot(panel44_100_NIR,mean.boot,R=1000)
ci.44_100.Green <- quantile(boot.44_100.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_100.Red <- quantile(boot.44_100.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_100.NIR <- quantile(boot.44_100.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_200.Green <- boot(panel44_200_Green,mean.boot,R=1000)
boot.44_200.Red <- boot(panel44_200_Red,mean.boot,R=1000)
boot.44_200.NIR <- boot(panel44_200_NIR,mean.boot,R=1000)
ci.44_200.Green <- quantile(boot.44_200.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_200.Red <- quantile(boot.44_200.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_200.NIR <- quantile(boot.44_200.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_300.Green <- boot(panel44_300_Green,mean.boot,R=1000)
boot.44_300.Red <- boot(panel44_300_Red,mean.boot,R=1000)
boot.44_300.NIR <- boot(panel44_300_NIR,mean.boot,R=1000)
ci.44_300.Green <- quantile(boot.44_300.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_300.Red <- quantile(boot.44_300.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_300.NIR <- quantile(boot.44_300.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_400.Green <- boot(panel44_400_Green,mean.boot,R=1000)
boot.44_400.Red <- boot(panel44_400_Red,mean.boot,R=1000)
boot.44_400.NIR <- boot(panel44_400_NIR,mean.boot,R=1000)
ci.44_400.Green <- quantile(boot.44_400.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_400.Red <- quantile(boot.44_400.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_400.NIR <- quantile(boot.44_400.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_500.Green <- boot(panel44_500_Green,mean.boot,R=1000)
boot.44_500.Red <- boot(panel44_500_Red,mean.boot,R=1000)
boot.44_500.NIR <- boot(panel44_500_NIR,mean.boot,R=1000)
ci.44_500.Green <- quantile(boot.44_500.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_500.Red <- quantile(boot.44_500.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_500.NIR <- quantile(boot.44_500.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_600.Green <- boot(panel44_600_Green,mean.boot,R=1000)
boot.44_600.Red <- boot(panel44_600_Red,mean.boot,R=1000)
boot.44_600.NIR <- boot(panel44_600_NIR,mean.boot,R=1000)
ci.44_600.Green <- quantile(boot.44_600.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_600.Red <- quantile(boot.44_600.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_600.NIR <- quantile(boot.44_600.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_700.Green <- boot(panel44_700_Green,mean.boot,R=100)
boot.44_700.Red <- boot(panel44_700_Red,mean.boot,R=1000)
boot.44_700.NIR <- boot(panel44_700_NIR,mean.boot,R=1000)
ci.44_700.Green <- quantile(boot.44_700.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_700.Red <- quantile(boot.44_700.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_700.NIR <- quantile(boot.44_700.NIR$t,probs=c(0.025,0.5,0.975)) 

boot.44_800.Green <- boot(panel44_800_Green,mean.boot,R=1000)
boot.44_800.Red <- boot(panel44_800_Red,mean.boot,R=1000)
boot.44_800.NIR <- boot(panel44_800_NIR,mean.boot,R=1000)
ci.44_800.Green <- quantile(boot.44_800.Green$t,probs=c(0.025,0.5,0.975)) 
ci.44_800.Red <- quantile(boot.44_800.Red$t,probs=c(0.025,0.5,0.975)) 
ci.44_800.NIR <- quantile(boot.44_800.NIR$t,probs=c(0.025,0.5,0.975)) 


# cbind the different confidence intervals together and make plots of altitude CI'S

#Plot the Green band 6% CIs
lab <- c(200,400,600,800)
plotmat.06.Green <- cbind(ci.06_0.Green, ci.06_100.Green, ci.06_200.Green, ci.06_300.Green, ci.06_400.Green, ci.06_500.Green,ci.06_600.Green, ci.06_700.Green, ci.06_800.Green)
plot.ci(plotmat.06.Green, xlab='', ylab='')
title(main="Confidence Intervals for Green Band 06% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.06_0.Green), col="blue")
abline(h=max(ci.06_0.Green),lty= 2, col="red")
abline(h=min(ci.06_0.Green),lty= 2, col="red")



#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel06_0_Green,panel06_100_Green,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel06_0_Green,panel06_200_Green,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel06_0_Green,panel06_300_Green,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel06_0_Green,panel06_400_Green,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel06_0_Green,panel06_500_Green,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel06_0_Green,panel06_600_Green,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel06_0_Green,panel06_700_Green,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel06_0_Green,panel06_800_Green,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the Green band 6% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 6% panel at 800 feet is different than the ground reference data')
}

#Plot the Green band 22% CIs
lab <- c(200,400,600,800)
plotmat.22.Green <- cbind(ci.22_0.Green, ci.22_100.Green, ci.22_200.Green, ci.22_300.Green, ci.22_400.Green, ci.22_500.Green,ci.22_600.Green, ci.22_700.Green, ci.22_800.Green)

plot.ci(plotmat.22.Green, xlab='', ylab='')
title(main="Confidence Intervals for Green Band 22% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.22_0.Green), col="blue")
abline(h=max(ci.22_0.Green),lty= 2, col="red")
abline(h=min(ci.22_0.Green),lty= 2, col="red")


#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel22_0_Green,panel22_100_Green,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel22_0_Green,panel22_200_Green,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel22_0_Green,panel22_300_Green,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel22_0_Green,panel22_400_Green,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel22_0_Green,panel22_500_Green,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel22_0_Green,panel22_600_Green,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel22_0_Green,panel22_700_Green,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel22_0_Green,panel22_800_Green,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the Green band 22% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 22% panel at 800 feet is different than the ground reference data')
}

#Plot the Green band 44% CIs
lab <- c(200,400,600,800)
plotmat.44.Green <- cbind(ci.44_0.Green, ci.44_100.Green, ci.44_200.Green, ci.44_300.Green, ci.44_400.Green, ci.44_500.Green,ci.44_600.Green, ci.44_700.Green, ci.44_800.Green)

plot.ci(plotmat.44.Green, xlab='', ylab='')
title(main="Confidence Intervals for Green Band 44% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.44_0.Green), col="blue")
abline(h=max(ci.44_0.Green),lty= 2, col="red")
abline(h=min(ci.44_0.Green),lty= 2, col="red")


#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel44_0_Green,panel44_100_Green,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel44_0_Green,panel44_200_Green,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel44_0_Green,panel44_300_Green,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel44_0_Green,panel44_400_Green,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel44_0_Green,panel44_500_Green,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel44_0_Green,panel44_600_Green,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel44_0_Green,panel44_700_Green,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel44_0_Green,panel44_800_Green,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the Green band 44% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Green band 44% panel at 800 feet is different than the ground reference data')
}



#Plot the Red band 6% CIs
lab <- c(200,400,600,800)
plotmat.06.Red <- cbind(ci.06_0.Red, ci.06_100.Red, ci.06_200.Red, ci.06_300.Red, ci.06_400.Red, ci.06_500.Red,ci.06_600.Red, ci.06_700.Red, ci.06_800.Red)

plot.ci(plotmat.06.Red, xlab='', ylab='')
title(main="Confidence Intervals for Red Band 06% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.06_0.Red), col="blue")
abline(h=max(ci.06_0.Red),lty= 2, col="red")
abline(h=min(ci.06_0.Red),lty= 2, col="red")



#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel06_0_Red,panel06_100_Red,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel06_0_Red,panel06_200_Red,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel06_0_Red,panel06_300_Red,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel06_0_Red,panel06_400_Red,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel06_0_Red,panel06_500_Red,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel06_0_Red,panel06_600_Red,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel06_0_Red,panel06_700_Red,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel06_0_Red,panel06_800_Red,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the Red band 6% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 6% panel at 800 feet is different than the ground reference data')
}

#Plot the Red band 22% CIs
lab <- c(200,400,600,800)
plotmat.22.Red <- cbind(ci.22_0.Red, ci.22_100.Red, ci.22_200.Red, ci.22_300.Red, ci.22_400.Red, ci.22_500.Red,ci.22_600.Red, ci.22_700.Red, ci.22_800.Red)

plot.ci(plotmat.22.Red, xlab='', ylab='')
title(main="Confidence Intervals for Red Band 22% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.22_0.Red), col="blue")
abline(h=max(ci.22_0.Red),lty= 2, col="red")
abline(h=min(ci.22_0.Red),lty= 2, col="red")


#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel22_0_Red,panel22_100_Red,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel22_0_Red,panel22_200_Red,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel22_0_Red,panel22_300_Red,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel22_0_Red,panel22_400_Red,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel22_0_Red,panel22_500_Red,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel22_0_Red,panel22_600_Red,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel22_0_Red,panel22_700_Red,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel22_0_Red,panel22_800_Red,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the Red band 22% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 22% panel at 800 feet is different than the ground reference data')
}

#Plot the Red band 44% CIs
lab <- c(200,400,600,800)
plotmat.44.Red <- cbind(ci.44_0.Red, ci.44_100.Red, ci.44_200.Red, ci.44_300.Red, ci.44_400.Red, ci.44_500.Red,ci.44_600.Red, ci.44_700.Red, ci.44_800.Red)

plot.ci(plotmat.44.Red, xlab='', ylab='')
title(main="Confidence Intervals for Red Band 44% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.44_0.Red), col="blue")
abline(h=max(ci.44_0.Red),lty= 2, col="red")
abline(h=min(ci.44_0.Red),lty= 2, col="red")


#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel44_0_Red,panel44_100_Red,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel44_0_Red,panel44_200_Red,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel44_0_Red,panel44_300_Red,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel44_0_Red,panel44_400_Red,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel44_0_Red,panel44_500_Red,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel44_0_Red,panel44_600_Red,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel44_0_Red,panel44_700_Red,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel44_0_Red,panel44_800_Red,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the Red band 44% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the Red band 44% panel at 800 feet is different than the ground reference data')
}


#Plot the NIR band 6% CIs
lab <- c(200,400,600,800)
plotmat.06.NIR <- cbind(ci.06_0.NIR, ci.06_100.NIR, ci.06_200.NIR, ci.06_300.NIR, ci.06_400.NIR, ci.06_500.NIR,ci.06_600.NIR, ci.06_700.NIR, ci.06_800.NIR)

plot.ci(plotmat.06.NIR, xlab='', ylab='')
title(main="Confidence Intervals for NIR Band 06% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.06_0.NIR), col="blue")
abline(h=max(ci.06_0.NIR),lty= 2, col="red")
abline(h=min(ci.06_0.NIR),lty= 2, col="red")



#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel06_0_NIR,panel06_100_NIR,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel06_0_NIR,panel06_200_NIR,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel06_0_NIR,panel06_300_NIR,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel06_0_NIR,panel06_400_NIR,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel06_0_NIR,panel06_500_NIR,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel06_0_NIR,panel06_600_NIR,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel06_0_NIR,panel06_700_NIR,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel06_0_NIR,panel06_800_NIR,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 6% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 6% panel at 800 feet is different than the ground reference data')
}

#Plot the NIR band 22% CIs
lab <- c(200,400,600,800)
plotmat.22.NIR <- cbind(ci.22_0.NIR, ci.22_100.NIR, ci.22_200.NIR, ci.22_300.NIR, ci.22_400.NIR, ci.22_500.NIR,ci.22_600.NIR, ci.22_700.NIR, ci.22_800.NIR)

plot.ci(plotmat.22.NIR, xlab='', ylab='')
title(main="Confidence Intervals for NIR Band 22% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.22_0.NIR), col="blue")
abline(h=max(ci.22_0.NIR),lty= 2, col="red")
abline(h=min(ci.22_0.NIR),lty= 2, col="red")


#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel22_0_NIR,panel22_100_NIR,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel22_0_NIR,panel22_200_NIR,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel22_0_NIR,panel22_300_NIR,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel22_0_NIR,panel22_400_NIR,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel22_0_NIR,panel22_500_NIR,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel22_0_NIR,panel22_600_NIR,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel22_0_NIR,panel22_700_NIR,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel22_0_NIR,panel22_800_NIR,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 22% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 22% panel at 800 feet is different than the ground reference data')
}

#Plot the NIR band 44% CIs
lab <- c(200,400,600,800)
plotmat.44.NIR <- cbind(ci.44_0.NIR, ci.44_100.NIR, ci.44_200.NIR, ci.44_300.NIR, ci.44_400.NIR, ci.44_500.NIR,ci.44_600.NIR, ci.44_700.NIR, ci.44_800.NIR)

plot.ci(plotmat.44.NIR, xlab='', ylab='')
title(main="Confidence Intervals for NIR Band 44% Panel ", xlab=expression(Altitude~(ft~'*'~10^{2}~','~AGL)), ylab="Brightness Value (0-255)")
abline(h=median(ci.44_0.NIR), col="blue")
abline(h=max(ci.44_0.NIR),lty= 2, col="red")
abline(h=min(ci.44_0.NIR),lty= 2, col="red")


#Now run a permutation test to determine whether the two dataset samples come from the same population
#Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
permTest100 <- permutationTestMeans(panel44_0_NIR,panel44_100_NIR,B=10000)
print(permTest100)
if(permTest100 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 100 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 100 feet is different than the ground reference data')
}
permTest200 <- permutationTestMeans(panel44_0_NIR,panel44_200_NIR,B=10000)
print(permTest200)
if(permTest200 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 200 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 200 feet is different than the ground reference data')
}
permTest300 <- permutationTestMeans(panel44_0_NIR,panel44_300_NIR,B=10000)
print(permTest300)
if(permTest300 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 300 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 300 feet is different than the ground reference data')
}
permTest400 <- permutationTestMeans(panel44_0_NIR,panel44_400_NIR,B=10000)
print(permTest400)
if(permTest400 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 400 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 400 feet is different than the ground reference data')
}
permTest500 <- permutationTestMeans(panel44_0_NIR,panel44_500_NIR,B=10000)
print(permTest500)
if(permTest500 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 500 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 500 feet is different than the ground reference data')
}
permTest600 <- permutationTestMeans(panel44_0_NIR,panel44_600_NIR,B=10000)
print(permTest600)
if(permTest600 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 600 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 600 feet is different than the ground reference data')
}
permTest700 <- permutationTestMeans(panel44_0_NIR,panel44_700_NIR,B=10000)
print(permTest700)
if(permTest700 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 700 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 700 feet is different than the ground reference data')
}
permTest800 <- permutationTestMeans(panel44_0_NIR,panel44_800_NIR,B=10000)
print(permTest800)
if(permTest800 < .05){
  print('Test is significant. We can state that the dataset for the NIR band 44% panel at 800 feet is different than the ground reference data')
}else{
  print('Test is not significant. We cannot state that the dataset for the NIR band 44% panel at 800 feet is different than the ground reference data')
}
dev.off()
closeAllConnections()
#END
