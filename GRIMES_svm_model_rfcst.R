#############################################################################
########### SVM Model vs Logistic and Linear Discriminant Models ############
#############################################################################

library("e1071")
library("MASS")
source('gefs_pca.R')
source('contable.R')
source('table.stats.R')
source('predictlm.R')
source("brier.R")


# Create empty matrices for  both LogR and SVM model statistics
hss.val <- matrix(numeric(500*19),ncol=19)
pod.val <- matrix(numeric(500*19),ncol=19)
far.val <- matrix(numeric(500*19),ncol=19)
bias.val <- matrix(numeric(500*19),ncol=19)
bss.val <- matrix(numeric(500*19),ncol=19)

kernel <- c("poly","radial") ##V1
kernel.parm <- c(1,2,0.01,0.05,0.1,0.5) ##V1
costs <- c(1,10,100) ##V1

set.seed(25)
for(r in 1:500){

## Read i nsig. regions variables data matrix
	source("sig_input.R")
	wts <- length(predictand.train)/table(predictand.train)
	
	print(paste("I'm on iteration #",r,"!",sep=""))

### Logistic regression--1st col in model statistics
	
	logr.obj <- glm(predictand.train~t(train.data),family=binomial(link='logit')) 
	exponents <- predictlm(logr.obj,t(test.data))
	probs <- exp(exponents)/(1+exp(exponents))
        probs <- ifelse(is.nan(probs),1,probs)

	y.hat.logr <- ifelse(probs>0.5,1,0)
	logr.con.tab <- con.table(predictand.test,y.hat.logr)
	hss.val[r,1]<-as.matrix(table.stats(logr.con.tab))[7,2]
	pod.val[r,1]<-as.matrix(table.stats(logr.con.tab))[5,2]
	far.val[r,1]<-as.matrix(table.stats(logr.con.tab))[4,2]
	bias.val[r,1]<-as.matrix(table.stats(logr.con.tab))[3,2]
	bss.val[r,1] <- as.matrix(brier(predictand.test,probs)[3])

### Linear Discriminant Analysis--2nd col in model statistics
	#train.data <- data.frame(train.mat)
	#test.data <- data.frame(test.mat)
	#lda.obj <- lda(train.data[,1] ~.,data=train.data[,(2:dim(train.data)[2])])
	#exponents.2 <- predict(lda.obj,test.data[,(2:dim(test.data)[2])])
	#junk <- table(test.data[,1],hat$class)
	#probs.2 <- exp(exponents.2)/(1+exp(exponents.2))
	#y.hat.lda <- ifelse(probs.2>=0.5,1,0)
	#lda.con.tab <- con.table(test.data[,1],y.hat.lda)
	#hss.val[r,2]<-as.matrix(table.stats(lda.con.tab))[7,2]
	#pod.val[r,2]<-as.matrix(table.stats(lda.con.tab))[5,2]
	#far.val[r,2]<-as.matrix(table.stats(lda.con.tab))[4,2]
	#bias.val[r,2]<-as.matrix(table.stats(lda.con.tab))[3,2]
	#bss.val[r,2] <- as.matrix(brier(predictand,lda.obj$fitted.values))

### SVM--3rd-19th col in model statistics
	m <- 1 #Change to two when adding in lda

	for(k in 1:length(costs)) {
		
		for(j in 1:length(kernel)) {

                        if(kernel[j]=="poly"){
                        kernel.parm <- c(1.00,2.00)

                        }else{

                                kernel.parm <- c(0.01,0.05,0.10,0.50)
                        }


			for(v in 1:length(kernel.parm)){	
				m <- m+1


				if(length(kernel.parm==2)){
				                                        
					print(paste("I'm on", costs[k],kernel[j],kernel.parm[v],"!",sep=' '))

					svm.obj <- svm(t(train.data),predictand.train,type='C-classification',kernel=kernel[j],degree=kernel.parm[v],cost=costs[k],class.weights=wts,probability=T)

				}else{
                                        print(paste("I'm on", costs[k],kernel[j],kernel.parm[v],"!",sep=' '))

					svm.obj <- svm(t(train.data),predictand.train,type='C-classification',kernel=kernel[j],gamma=kernel.parm[v],cost=costs[k],class.weights=wts,probability=T) 
				}


			y.hat <- predict(svm.obj,t(test.data),probability=T)
			probs.svm <- attr(y.hat,"probabilities")	
                        probs.svm <- probs.svm[,which(colnames(probs.svm)=="1")]
			y.hat.svm <- ifelse(probs.svm>0.1,1,0)
			svm.con.tab <- con.table(predictand.test,y.hat.svm)

			hss.val[r,m]<-as.matrix(table.stats(svm.con.tab))[7,2]
			pod.val[r,m]<-as.matrix(table.stats(svm.con.tab))[5,2]
			far.val[r,m]<-as.matrix(table.stats(svm.con.tab))[4,2]
			bias.val[r,m]<-as.matrix(table.stats(svm.con.tab))[3,2]
			bss.val[r,m] <- as.matrix(brier(predictand.test,probs.svm)[3])

			} # Close different kernel parameter loop
		} # Close different kernel loop
	} # Close cost loop
} #Close bootstrap loop 

write(t(hss.val),'hss_rfcst30_rawGEFS99.txt',ncol=19)
write(t(bss.val),'bss_rfcst30_rawGEFS99.txt',ncol=19)
write(t(pod.val),'pod_rfcst30_rawGEFS99.txt',ncol=19)
write(t(far.val),'far_rfcst30_rawGEFS99.txt',ncol=19)
write(t(bias.val),'bias_rfcst30_rawGEFS99.txt',ncol=19)




