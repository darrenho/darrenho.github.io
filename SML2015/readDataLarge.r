inst_pkgs = load_pkgs =  c("ff","ffbase","biglm")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

setwd('/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/')

# Check temporary directory ff will write to
getOption("fftempdir")

# Set new temporary directory
options(fftempdir = "/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/temp/")

getNames = read.csv("./data/train.csv",nrows=1,header=T,stringsAsFactors=F)

namesVec = names(getNames)

nrows     = 5000
next.rows = 500
first.rows = next.rows
# Load in the small data
Xtrain.ffx = read.csv.ffdf(file='/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/data/train.csv', # File Name
                      nrows=nrows,
                      header=TRUE,     # No variable names are included in the file
                      fill = TRUE,      # Missing values are represented by NA
                      first.rows = first.rows,
                      next.rows = next.rows,
                      VERBOSE = TRUE,
                      colClasses=NA
)
colnames(Xtrain.ffx) = namesVec
XtrainTmp = Xtrain.ffx[,-1]
#plot(apply(apply(XtrainTmp,2,is.na),2,sum))

tooFewLevels = lapply(apply(XtrainTmp,2,unique),length) == 1
tooManyNAs   = apply(apply(XtrainTmp,2,is.na),2,sum) > 100

XtrainTmp    = XtrainTmp[,!tooFewLevels & !tooManyNAs]

remainingNAs = apply(XtrainTmp,1,anyNA)

Xtrain       = XtrainTmp[!remainingNAs,]

subSelect = c(1:2,ncol(Xtrain))

glm.big = bigglm(terms(target ~ ., data = Xtrain[,subSelect]),data = Xtrain[,subSelect])
glm.out = glm(terms(target ~ ., data = Xtrain[,subSelect]),data = Xtrain[,subSelect])
glm.out = glm(target ~ ., data = Xtrain[,subSelect])

Yhat.big = predict(glm.big,Xtrain[,subSelect],type='response')
Yhat.out = predict(glm.out,type='response')

plot(Yhat.big,Yhat.out)