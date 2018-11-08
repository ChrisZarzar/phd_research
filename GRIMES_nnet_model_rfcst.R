################################################################
######## Perform a neural network to predict RI  ###############
################################################################

source("model_prep_rfcst25.R") ## Gather input variables
source("model_pca_rfcst.R") ## Data reduction of input variables

####### Neural Network ########

#Mix up RI's and NRI's
set.seed(25)
mixed <- sample(c(1:length(predictand)),length(predictand))
final.data <- final.data[mixed,]
library("neuralnet")
library("MASS")
source('contable.R')
source('table.stats.R')
source("brier.R")
hss.val <- matrix(numeric(1000*25),ncol=25)
pod.val <- matrix(numeric(1000*25),ncol=25)
far.val <- matrix(numeric(1000*25),ncol=25)
bias.val <- matrix(numeric(1000*25),ncol=25)

set.seed(50)
for(r in 1:1000) {
	print(paste("I'm on iteration #",r,"!",sep=""))
	train.labs <- sample(c(1:dim(final.data)[1]),length(final.data[,1])*0.85)
	test.labs <- c(1:dim(final.data)[1])[-train.labs]
	train.data <- final.data[train.labs,]
	test.data <- final.data[test.labs,]

	nn.obj <- neuralnet(final.data[,1]~predictors,final.data,hidden=1,rep=100,act.fct='logistic')

	y.hat <- compute(nn.obj,predictors)$net.result
	con.tab <- con.table(test.data[,1],y.hat)

	bss.val[r,1] <- as.matrix(brier(test.data[,1],nn.obj$fitted.values))
	hss.val[r,1]<-as.matrix(table.stats(con.tab))[7,2]
	pod.val[r,1]<-as.matrix(table.stats(con.tab))[5,2]
	far.val[r,1]<-as.matrix(table.stats(con.tab))[4,2]
	bias.val[r,1]<-as.matrix(table.stats(con.tab))[3,2]

}  ## End forecast loop

write(t(hss.val),'hss_nnet_rfcst.txt',ncol=1)
write(t(bss.val),'bss_nnet_rfcst.txt',ncol=1)
write(t(pod.val),'pod_nnet_rfcst.txt',ncol=1)
write(t(far.val),'far_nnet_rfcst.txt',ncol=1)
write(t(bias.val),'bias_nnet_rfcst.txt',ncol=1)



