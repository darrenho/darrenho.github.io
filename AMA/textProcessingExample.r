library(tm)

load(file="docs.Rdata")

docs = docs[1:8]

docLens = sapply(docs,nchar)

### Choose a number for this.  Something between 2000 and 3000, but not equal to 2500
nCharInDoc = ?????

Y         = c()
splitDocs = c()
iter = 0
for(doc in docs){
  iter = iter + 1
  notAtEnd = T
  iterSplit = 0
  while(notAtEnd){
    if(iter <= 4){
      Y = c(Y,'tmnt')
    }else{
      Y = c(Y,'artist')
    }
    iterSplit = iterSplit + 1
    newChunk = substr( doc,((iterSplit - 1)*nCharInDoc+1),(iterSplit*nCharInDoc) )
    splitDocs = c(splitDocs,newChunk)
    if(nchar(newChunk) < nCharInDoc){
      notAtEnd = F
    }
  }
}

corp = VCorpus(VectorSource(splitDocs))

###########
# IDF weighting, stemming not removed
############
Sys.setenv(NOAWT=TRUE)
#library(Snowball)
library(RWeka)
library(rJava)
# Using document length weighting
dtm = DocumentTermMatrix(corp,
        control=list(tolower=TRUE,
                     removePunctuation=list(preserve_intra_word_dashes=TRUE),
                     removeNumbers=TRUE,
                     stemming=TRUE,
                     stopwords = TRUE,
                     weighting=weightTfIdf,
                     wordLengths = c(3,10))
)
mydtm = as.matrix(dtm)


########## PCA
pca.out = svd(scale(mydtm,scale=F))
scores   = pca.out$u %*% diag(pca.out$d)
loadings = pca.out$v

color = rep(0,nrow(mydtm))
pallet = rainbow(2)
color[Y == 'tmnt']   = pallet[1]
color[Y == 'artist'] = pallet[2]

plot(scores[,1:2],type='n')
text(scores[,1:2],label=Y,col=color)


### Find some interesting points on the previous plot and investigate the corresponding documents.  Why might they
#   be near each other in the PC space?
plot(scores[,1:2],type='n')
text(scores[,1:2],label=Y,col=color)
identify(scores[,1],scores[,2],n=11)

# Here were mine, for example.  You'll have different ones
groupTMNT1   = c(17,28,10,35)
groupArtist1 = c(65,86,102,108)
groupTMNT2   = 8
groupArtist2 = 109
groupArtist3 = 63

splitDocs[groupTMNT2] ##This will give you the documents from the above investigation


##Now we can look at the loadings.  What do you see?
plot(loadings[,1:2],type='n')
text(loadings[,1:2],label=colnames(mydtm),cex=.75)


#### Sparse logistic regression
require(glmnet)
out.glm  = cv.glmnet(x=mydtm,y=Y,family='binomial')

Yhat.glm = predict(out.glm,newx=mydtm,s='lambda.min',type='response')
plot(Yhat.glm,col=color)

beta.glm         = coef(out.glm,s='lambda.min')[-1]
beta.glm.nonzero = beta.glm[abs(beta.glm)>0]
words.glm        = colnames(mydtm)[abs(beta.glm)>0]
print(words.glm)
which(mydtm[,which(colnames(mydtm) == 'wait')]>0)

negPos = beta.glm.nonzero > 0
colors = rep('red',length(beta.glm.nonzero))
colors[negPos] = 'blue'
library(wordcloud)
#### A wordcloud with the word size/color given by the magnitude/sign of the coefficient
wordcloud(words.glm,abs(beta.glm.nonzero), scale=c(4,.5),
          colors = colors,ordered.colors=T,
          max.words=100, random.order=T, rot.per=.15)


#### Clustering

### docs
Delta      = dist(mydtm)
Delta.plot = as.matrix(dist(mydtm,diag=T,upper=T))
image(Delta.plot)

clust.docs = hclust(Delta,method='average')
plot(clust.docs,labels=Y,cex=.65)


cluster.sol = kmeans(mydtm,centers=2)
table(cluster.sol$clust)

Y.clust = ifelse(cluster.sol$clust==1,'tmnt','artist')

Y.num   = ifelse(Y=='tmnt',1,2)
table(Y.clust,Y)

#### words
Delta = dist(t(mydtm))

# let's look for extreme words
clust.words = hclust(Delta,method='single')
cluster.sol = cutree(clust.words,k=10)
table(cluster.sol)

for(k in unique(cluster.sol)){
  if(table(cluster.sol)[k] <= 3){
    print(which(cluster.sol==k))
  }
}

# major clusters
clust.words = hclust(Delta,method='complete')
cluster.sol = cutree(clust.words,k=5)
table(cluster.sol)

k    = 2
comp = 2
wordcloud(colnames(mydtm)[cluster.sol==k],
          abs(loadings[cluster.sol==k,comp]), 
          scale=c(3,.5),max.words=40)



plot(1:nrow(mydtm),cumsum(pca.out$d^2)/sum(pca.out$d^2),type='l' )
nComps = which.min(which(cumsum(pca.out$d^2)/sum(pca.out$d^2) > .75))
Delta = dist(loadings[,1:nComps])
clust.words = hclust(Delta,method='complete')
cluster.sol = cutree(clust.words,k=5)
table(cluster.sol)

k    = 1
comp = 1
wordcloud(colnames(mydtm)[cluster.sol==k],abs(loadings[cluster.sol==k,comp]), scale=c(3,.5),max.words=40)

