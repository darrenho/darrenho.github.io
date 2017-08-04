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
fileName = '/Users/darrenho/Dropbox/research/kaggle/springLeafTmp/Xtrain.csv'
getNames = read.csv(fileName,nrows=1,header=T,stringsAsFactors=T)
namesVec = names(getNames)
isFactor = sapply(getNames,is.factor)
dataType = rep(NA,length(isFactor))
dataType[isFactor] = 'factor'

####
# Convert database to FF
####
nrows      = -1
next.rows  = 5000
first.rows = 3000
# Load in the small data
Xtrain.ff = read.csv.ffdf(file=fileName, # File Name
                      nrows=nrows,
                      header=TRUE,     
                      fill = TRUE,      # Missing values are represented by NA
                      first.rows = first.rows,
                      next.rows = next.rows,
                      VERBOSE = TRUE,
                      appendLevels = TRUE,
                      colClasses=dataType
)
colnames(Xtrain.ff) = namesVec
save.ffdf(Xtrain.ff, dir=ffDir, overwrite=TRUE)