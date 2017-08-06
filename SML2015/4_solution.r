miss.class =function(pred.class,true.class,produceOutput=FALSE){
  confusion.mat = table(pred.class,true.class)
  if(produceOutput){
    return(1-sum(diag(confusion.mat))/sum(confusion.mat))	
  }
  else{
    print('miss-class')
    print(1-sum(diag(confusion.mat))/sum(confusion.mat))
    print('confusion mat')
    print(confusion.mat)
  }
}

load("spam.Rdata")

train = !spam$train

test  = !train

X     = spam$XdataF[train,]
X_0   = spam$XdataF[!train,]
Y     = spam$Y[train]
Y_0   = spam$Y[!train]

#Split data as in 2-fold CV
cvTrain = sample(c(T,F),size=sum(train),replace=T,prob=c(.5,.5))
Xcv   = X[cvTrain,]
Xcv_0 = X[!cvTrain,]
Ycv   = Y[cvTrain]
Ycv_0 = Y[!cvTrain]

covariate_labels = spam$covariate_labels

require(neuralnet)

###
# 2
###
nRep = 3
model.out = as.formula(paste("Ycv ~ ",paste(names(Xcv),collapse='+')))
nn.out    = neuralnet(model.out,data=Xcv, hidden=c(5,5), threshold=0.1,rep=nRep,err.fct='ce',
                      linear.output=F)
for(reps in 1:nRep){
  probHat = compute(nn.out,Xcv_0,rep=reps)$net.result
  Yhat    = rep(0,nrow(Xcv_0))
  Yhat[probHat > .5] = 1
  miss.class(Yhat,Ycv_0)  
}

nn.results = matrix(0,nrow=nrow(Xcv_0),ncol=nRep)                       
for(reps in 1:nRep){
  nn.results[,reps] = compute(nn.out,Xcv_0,rep=reps)$net.result
}
probHat    = apply(nn.results,1,mean)
YcvHat     = rep(0,nrow(Xcv_0))
YcvHat[probHat > .5] = 1
miss.class(YcvHat,Ycv_0)

###
# 3
###
### Choosing threshold
cvF = function(threshold,Xcv,Ycv,Xcv_0,Ycv_0,nRep){
  model.out = as.formula(paste("Ycv ~ ",paste(names(Xcv),collapse='+')))
  nn.out    = neuralnet(model.out,data=Xcv, hidden=c(5,5), threshold=threshold,
                        rep=nRep,err.fct='ce',linear.output=F)
  nn.results = matrix(0,nrow=nrow(Xcv_0),ncol=nRep)                       
  for(reps in 1:nRep){
    nn.results[,reps] = compute(nn.out,Xcv_0,rep=reps)$net.result
  }
  probHat    = apply(nn.results,1,mean)
  YcvHat     = rep(0,nrow(Xcv_0))
  YcvHat[probHat > .5] = 1
  test.error = mean(YcvHat != Ycv_0)
  return(test.error)
}
library(snowfall)
sfInit(restore = TRUE,parallel=TRUE, cpus=2, type="SOCK")

thresholdGrid =  2^seq(-10,1,by=1)

#sfClusterSetupRNG()
#sfExport("sir.adm")
sfLibrary(neuralnet)
nRep = 1
result <- sfLapply(thresholdGrid, cvF,Xcv,Ycv,Xcv_0,Ycv_0,nRep)
threshOpt = thresholdGrid[which.min(unlist(result))]

