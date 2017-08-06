# Based on Ben Hamner script from Springleaf
# https://www.kaggle.com/benhamner/springleaf-marketing-response/random-forest-example

library(readr)
library(randomForest)

set.seed(616)

cat("reading the train and test data\n")
train = read.csv("train.csv",header=T,nrows=-1,stringsAsFactors =F)
str(train)

nrows = -1
colClasses = c('integer','factor','Date','numeric',
               'numeric','factor','factor','factor',
               'factor')
train = read.csv("train.csv",header=T,nrows=nrows,
                 colClasses = colClasses)
colClasses = c('integer','integer','factor','Date',
               'factor','factor','factor',
               'factor')
test  = read.csv("test.csv",header=T,nrows=nrows,
                 colClasses = colClasses)

store = read.csv("store.csv",header=T,nrows=10,stringsAsFactors =F)
str(store)

colClasses = c('integer', 'factor', 'factor','numeric',
               'numeric','numeric','factor',
               'numeric','numeric','factor')
store = read.csv("store.csv",header=T,nrows=nrows)

train = merge(train,store)
test  = merge(test,store)

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   = 0
test[is.na(test)]   = 0

details = FALSE
if(details){
  cat("train data column names and details\n")
  names(train)
  str(train)
  summary(train)
  cat("test data column names and details\n")
  names(test)
  str(test)
  summary(test)
}
# looking at only stores that were open in the train set
# may change this later
train = train[ which(train$Open=='1'),]

# seperating out the elements of the date column for the train set
train$month = as.integer(format(train$Date, "%m"))
train$year = as.integer(format(train$Date, "%y"))
train$day = as.integer(format(train$Date, "%d"))
test$month = as.integer(format(test$Date, "%m"))
test$year = as.integer(format(test$Date, "%y"))
test$day = as.integer(format(test$Date, "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
#train = train[,-c(3,8)]
train = subset(train,select=-c(Date,StateHoliday))
test  = subset(test,select=-c(Date,StateHoliday))

feature.names = names(train)#[c(1,2,6,8:12,14:19)]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  print(f)
  if (class(train[[f]])=="factor") {
    unique.levels = unique(c(train[[f]], test[[f]]))
    for(level in unique.levels){
      featureName          = paste(c(f,'_',as.character(level)),collapse='')
      train[[featureName]] = as.integer(train[[f]] == level)
      test[[featureName]]  = as.integer(test[[f]] == level)
    }
    train = subset(train,select=-get(f))
    test  = subset(test,select=-get(f))
  }
  print(names(train))
}

# imputation to the mean (assuming 0 is missing value not lack of competition)

# this did not improve performance -- some zeroes are possibly missing data bit others may be no competitor

# train$CompetitionOpenSinceMonth[train$CompetitionOpenSinceMonth==0] = mean(train$CompetitionOpenSinceMonth, na.rm=TRUE)
# train$CompetitionOpenSinceYear[train$CompetitionOpenSinceYear==0] = mean(train$CompetitionOpenSinceYear, na.rm=TRUE)
#train$Promo2SinceWeek[train$Promo2SinceWeek==0] = mean(train$Promo2SinceWeek, na.rm=TRUE)
# train$Promo2SinceYear[train$Promo2SinceYear==0] = mean(train$Promo2SinceYear, na.rm=TRUE)

#test$CompetitionOpenSinceMonth[test$CompetitionOpenSinceMonth==0] = mean(test$CompetitionOpenSinceMonth, na.rm=TRUE)
#test$CompetitionOpenSinceYear[test$CompetitionOpenSinceYear==0] = mean(test$CompetitionOpenSinceYear, na.rm=TRUE)
#test$Promo2SinceWeek[test$Promo2SinceWeek==0] = mean(test$Promo2SinceWeek, na.rm=TRUE)
#test$Promo2SinceYear[test$Promo2SinceYear==0] = mean(test$Promo2SinceYear, na.rm=TRUE)

cat("checking all stores are accounted for\n")
length(unique(train$Store))

cat("train data column names after slight feature engineering\n")
names(train)
cat("test data column names after slight feature engineering\n")
names(test)


####
# Important note: Customers is in train but not test.  Customers is a very 
#                 important feature if used in training randomForest
####

#Note, here is a grid search to look at OOB error rates
Ytrain = train$Sales
Xtrain = subset(train,select=-c(Sales,Customers))

mtryGrid = c(2,4,6,8,10)
nTreeGrid = c(1:5)*100
results = matrix(0,nrow=length(mtryGrid),ncol=length(nTreeGrid))
mtrySweep = 0
nTreeSweep = 0
for(mtry in mtryGrid){
  mtrySweep = mtrySweep + 1
  for(nTree in nTreeGrid){
    nTreeSweep = nTreeSweep + 1
    clf = randomForest(x = Xtrain,y = log(Ytrain+1),
                       mtry=mtry,
                       ntree=nTree)
    results[mtrySweep,nTreeSweep] = mean(clf$ms)
  }
}


print('oob error ')
mean(clf$ms)

cat("model stats\n")
clf
cat("Importance 1\n")
varImpPlot(clf,cex=.5)
cat("Permutation Importance Unscaled\n")
importance(clf, type = 1)
cat("GINI Importance\n")
importance(clf, type = 2)
cat("Plot Model\n")
plot(clf)

cat("Predicting Sales\n")

Xtest = subset(test,select=-Id)
pred = exp(predict(clf, Xtest)) -1
submission = data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write.csv(submission, "rf1.csv")
