inst_pkgs = load_pkgs =  c("ff","ffbase")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

tempDir = '/Users/darrenho/Dropbox/research/kaggle/springLeafTmp/temp/'
ffDir = '/Users/darrenho/Dropbox/research/kaggle/springLeafTmp/ff/'

# Check temporary directory ff will write to
#getOption("fftempdir")

# Set new temporary directory
options(fftempdir = tempDir)

#####
# Get names and figure out data type of each column
##### 
fileName = '/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/data/train.csv'
getNames = read.csv(fileName,nrows=1,header=T,stringsAsFactors=T)
namesVec = names(getNames)

####
# Convert database to FF
####
nrows      = 50000
#In memory:
Xtrain.df = read.csv(file=fileName,nrows=nrows,stringsAsFactors=T) 
format(object.size(get("Xtrain.df")),units='auto') #1080 MB, 145231 x 1665
tmp  = sapply(Xtrain.df,is.factor)
tmp2 = sapply(Xtrain.df[,tmp],levels)
tmp3 = sapply(tmp2,length)

tooManyLevels = names(tmp3)[tmp3 > 5]

select = names(Xtrain.df) %in% tooManyLevels

#get rid of ID
Xtrain.df = Xtrain.df[,!select]
Xtrain.df = subset(Xtrain.df,select=-c(ID))


#get rid of features with too many NAs
naF = function(x){
	return(mean(is.na(x)))
}
NAs = apply(Xtrain.df,2,naF)
removeFeatureNAs = NAs > .05
Xtrain.dfNA = Xtrain.df[,!removeFeatureNAs] #1073 MB, 145231 x 1923

#Some features have strangely large numbers, let's eliminate these
maxF = function(x){
	if(is.factor(x)){
		return(0)
	}else{
		return(max(abs(x[!is.na(x)])))		
	}
}
findLargeNumbers = sapply(Xtrain.dfNA,maxF)
Xtrain.dfMax = Xtrain.dfNA[,findLargeNumbers < 6e+08] #1073 MB, 145231 x 1665
format(object.size(get("Xtrain.df")),units='auto')  #1080 MB, 145231 x 1665

#now, let's get rid of observations with NAs
tmp = apply(Xtrain.dfMax,1,anyNA)
#> mean(tmp)
#[1] 0.00646

subSelectColumns = 1:ncol(Xtrain.dfMax)
Xtrain = Xtrain.dfMax[!tmp,subSelectColumns]
format(object.size(get("Xtrain")),units='auto') #1080 MB, 145231 x 1665

Xtrain.ff = as.ffdf(Xtrain)

format(object.size(get("Xtrain.ff")),units='auto')

#save.ffdf(Xtrain.ff, dir=ffDir, overwrite=TRUE)

write.csv(Xtrain,file='/Users/darrenho/Dropbox/research/kaggle/springLeafTmp/Xtrain.csv')
XtrainSubSample = Xtrain[sample(1:nrow(Xtrain),ncol(Xtrain)*10,replace=T),]

save(XtrainSubSample,file='/Users/darrenho/Dropbox/research/kaggle/springLeafTmp/XtrainSubSample.Rdata')
