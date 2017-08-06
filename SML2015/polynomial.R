nGrid = c(50,100,250,1000)
regressionF = function(X){
  return(sin(X*2*pi))
}

pGridF = function(n,exponential=F){
  if(exponential){
    return(round(exp(seq(0,log(n/100),length=11)))[-1])
  }else{
    return(2:10)
  }
}

PhiF = function(X,p){
  return(X^(1:p))
}

varAndBiasF = function(featureMat,f_star){
  svd.out = svd(featureMat)
  U       = svd.out$u
  D       = svd.out$d
  return(list('var'=sum(D^(-2)),
              'bias'=f_star - U %*% t(U) %*% f_star,
              'd'=D))
}

n = 10000
X = (1:n)/n  
pGrid  = pGridF(n)
f_star = regressionF(X)
biasOut = rep(0,length(pGrid))
varOut  = rep(0,length(pGrid))
pSweep = 0
for(p in pGrid){
  print(p)
  pSweep          = pSweep + 1
  featureMat      = scale(t(sapply(X,PhiF,p)))
  varAndBias      = varAndBiasF(featureMat,f_star)
  biasOut[pSweep] = sum(varAndBias$bias**2)
  varOut[pSweep]  = varAndBias$var
}


plot(pGrid,biasOut,type='l',col='red')
lines(pGrid,varOut,col='blue')
risk = biasOut+varOut
points(pGrid,risk,col='green')
