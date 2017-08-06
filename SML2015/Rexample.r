tmp  = read.csv("./data/train.csv",nrows=1000,header=T,stringsAsFactors=F)
pTmp = length(tmp)
tmpNames = names(tmp)

grabF = function(j,pTmp){
  grab    = rep("NULL",pTmp)
  grab[j] = NA
  return(grab)
}

nrows    = 30
Xtrain   = NULL
Xtest    = NULL
namesVec = NULL 
isDummy  = NULL
#As ID is first and target is last
for(j in 2:30){#(pTmp-1)){
  grab     = grabF(j,pTmp)
  tmpTrain = read.csv("./data/train.csv",nrows=nrows,header=T,stringsAsFactors=F,colClasses = grab)[[1]]
  tmpTest = read.csv("./data/test.csv",nrows=nrows,header=T,stringsAsFactors=F,colClasses = grab)[[1]]  
  if(class(tmpTrain)=="character"){
    levels  = unique(c(tmpTrain,tmpTest))
    for(level in levels){
      dummy    = tmpTrain == level
      dummy    = dummy + 0 #convert to 0-1
      Xtrain   = cbind(Xtrain,dummy)
      dummy    = tmpTest == level
      dummy    = dummy + 0 #convert to 0-1
      Xtest    = cbind(Xtest,dummy) 
      varName  = paste(c(tmpNames[j],'_',as.character(level)),collapse='')
      namesVec = c(namesVec,varName)
      isDummy  = c(isDummy,TRUE)
    }
  }
  else{
      Xtrain   = cbind(Xtrain,tmpTrain)
      Xtest    = cbind(Xtest,tmpTest)
      namesVec = c(namesVec,tmpNames[j])
      isDummy  = c(isDummy,FALSE)
  }
}

grab = grabF(pTmp,pTmp)
Ytrain  = read.csv("./data/train.csv",nrows=nrows,header=T,stringsAsFactors=F,colClasses = grab)[[1]]
grab = grabF(1,pTmp)
IDtrain = read.csv("./data/train.csv",nrows=nrows,header=T,stringsAsFactors=F,colClasses = grab)[[1]]
grab = grabF(1,pTmp-1)
IDtest  = read.csv("./data/test.csv",nrows=nrows,header=T,stringsAsFactors=F,colClasses = grab)[[1]]  

