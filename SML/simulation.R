nTrain = 10000
p      = 400
rho    = .5
s      = 10

nTest = 100

n = nTrain + nTest

X = matrix(rnorm(p*n),nrow=n)
covMat = matrix(rho,nrow=p,ncol=p)
diag(covMat) = 1

X = X%*%covMat


b = rep(0,p)
b[1:s] = (1:s)^(-2)


Y = X %*% b + rnorm(n,sd=.1)

Xtrain = X[1:nTrain,]
Xtest  = X[(nTrain+1):n,]
Ytrain = X[1:nTrain]
Ytest  = X[(nTrain+1):n]

save(Xtrain,file='homework1_X.Rdata')
save(Xtest,file='homework1_Xtest.Rdata')
save(Ytrain,file='homework1_Y.Rdata')
save(Ytest,file='homework1_Ytest.Rdata')
