load(file='/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/forwardLogisticFull.Rdata')
model.out    = return.list$keepModelVec
chosen.out   = model.out[[length(model.out)]]
rm(model.out,return.list)

selectedCovs = attr(summary(chosen.out)$terms,which="term.labels")
tmp = attr(summary(chosen.out)$terms,which="dataClasses")
#####
# Get names and figure out data type of each column
##### 
fileName = '/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/data/test.csv'
getNames = read.csv(fileName,nrows=1,header=T,stringsAsFactors=T)
namesVec = names(getNames)

getTestColumnsReadIn =  namesVec %in% c("ID",selectedCovs)
print(namesVec[getTestColumnsReadIn])
####
# Process test data in memory
####
nrows      = -1
colClasses = rep('NULL',length(namesVec))
colClasses[getTestColumnsReadIn] = NA
#In memory:
Xtest.df = read.csv(file=fileName,nrows=nrows,stringsAsFactors=T,colClasses=colClasses) 
format(object.size(get("Xtest.df")),units='auto') #[1] "12.7 Mb"  145232  x 23

IDtest   = subset(Xtest.df,select=ID)
Xtest.df = subset(Xtest.df,select=-c(ID))

###
# Transform the defacto NAs into NA: Note: seems like only NAs and -1
###

print(dim(Xtest.df))

print(sum(Xtest.df == '[]',na.rm = TRUE) )
print(mean(Xtest.df == '[]',na.rm = TRUE) )
print(sum(Xtest.df == ' ',na.rm = TRUE))
print(mean(Xtest.df == ' ',na.rm = TRUE))
print(sum(Xtest.df == -1,na.rm = TRUE))

print(sum(is.na(Xtest.df)))
print(mean(is.na(Xtest.df)))

rowWithNA = apply(Xtest.df,1,anyNA)

Xtest.dfNoNA = Xtest.df[!rowWithNA,]
Xtest.dfNA   = Xtest.df[rowWithNA,]

set.seed(1)
subSelectObs = sample(1:nrow(Xtest.dfNoNA),1000)

YhatNoNA = predict(chosen.out,Xtest.dfNoNA,type='response')

YhatNoNA[YhatNoNA > .5] = 1
YhatNoNA[YhatNoNA < .5] = 0

isFactor = sapply(Xtest.dfNA,is.factor)
for(iNA in 1:nrow(Xtest.dfNA)){
  if(iNA %% 100 == 0) print(iNA)
  X.NA = Xtest.dfNA[iNA,!isFactor]
  NAvec = is.na(X.NA)
  X.NAcomplete = X.NA[!NAvec]
  distResults = rep(0,length(subSelectObs))
  iSweep = 0
  for(i in subSelectObs){
  	iSweep = iSweep + 1
    X = Xtest.dfNoNA[i,!isFactor]
    X = X[!NAvec]
    distResults[iSweep] = sum((X - X.NAcomplete)**2)
  }
  i_star = subSelectObs[which.min(distResults)]
  Xtest.dfNA[iNA,] = Xtest.dfNoNA[i_star,]
}

Xtest    = rbind(Xtest.dfNA,Xtest.dfNoNA)
ID       = c(IDtest[rowWithNA,1],IDtest[!rowWithNA,1])

Yhat  = predict(chosen.out,Xtest,type='response')
Yhat[Yhat > .5] = 1
Yhat[Yhat < .5] = 0

ID.order = order(ID)

IDsubmit   = ID[ID.order]
YhatSubmit = Yhat[ID.order]

submission = data.frame(ID=IDsubmit)
submission$target = YhatSubmit

write.csv(submission,'trial_submission.csv')