library(glmnet)
?glmnet
### HIV data
load("../data/hiv.rda")
X = hiv.train$x
Y = hiv.train$y

X_0 = hiv.test$x
Y_0 = hiv.test$y

p = ncol(X)
n = nrow(X)

###
# glmnet: lasso
###

lasso.cv.glmnet = cv.glmnet(X,Y,alpha=1,standardize=F)
lasso.glmnet    = lasso.cv.glmnet$glmnet.fit

Yhat.glmnet    = predict(lasso.cv.glmnet,X_0,s='lambda.min')
betaHat.glmnet = coef(lasso.cv.glmnet,s='lambda.min')

print(mean((Yhat.glmnet - Y_0)**2))
print(min(lasso.cv.glmnet$cvm))
###
# glmnet: elastic net
###

elasticNet.cv.glmnet = cv.glmnet(X,Y,alpha=.5,standardize=F)

Yhat.elasticNet    = predict(elasticNet.cv.glmnet,X_0,s='lambda.min')
betaHat.elasticNet = coef(elasticNet.cv.glmnet,s='lambda.min')

print(mean((Yhat.elasticNet - Y_0)**2))

elasticNet.cv.glmnet = cv.glmnet(X,Y,alpha=.9,standardize=F)

Yhat.elasticNet    = predict(elasticNet.cv.glmnet,X_0,s='lambda.min')
betaHat.elasticNet = coef(elasticNet.cv.glmnet,s='lambda.min')

print(mean((Yhat.elasticNet - Y_0)**2))


###
# glmnet: refitted lasso
###

lasso.cv.glmnet  = cv.glmnet(X,Y,alpha=1,standardize=F)

betaHat.temp     = coef(lasso.cv.glmnet,s='lambda.1se')[-1] #intercept
selectedCovs     = which(abs(betaHat.temp) > 1e-16)
refitted.lm      = lm(Y~.,data=data.frame(X[,selectedCovs]))
Yhat.refitted    = predict(refitted.lm,data.frame(X_0[,selectedCovs]))
betaHat.refitted = refitted.lm$coefficients

#betaHat.refitted
print(mean((Yhat.refitted - Y_0)**2))

###
# ssr
###
library(scalreg)
Ytilde = Y - mean(Y)
lasso.ssr = scalreg(X = X,y = Y,LSE=T)
print(which(abs(lasso.ssr$coefficients)>1e-16))
# p35  p83 p122 p123 p135 p184 p200 p211 
#  35   75  105  106  115  158  169  179 

Yhat.ssr    = predict(lasso.ssr,X_0)
print(mean((Yhat.ssr - Y_0)**2))

Yhat.ssr.refitted = X_0 %*% lasso.ssr$lse$coefficients
print(mean((Yhat.ssr.refitted - Y_0)**2))