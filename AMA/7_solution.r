### HIV data
load("../../../data/hiv.rda")
X = hiv.train$x
Y = hiv.train$y

X_0 = hiv.test$x
Y_0 = hiv.test$y

p = ncol(X)
n = nrow(X)

thresh = log(25,10)
Y_class = rep(0,n)
Y_class[Y<thresh] = 1

Y_0_class = rep(0,nrow(X_0))
Y_0_class[Y_0 < thresh] = 1
###
# LDA
###
library(MASS)

out.lda = lda(Y_class~.,data=data.frame(X))

out0 = apply(X[Y_class==0,],2,sd) > 1e-16
out1 = apply(X[Y_class==1,],2,sd) > 1e-16

nonConstantVars = out0*out1

X.lda   = X[,nonConstantVars] 
X_0.lda = X_0[,nonConstantVars] 
out.lda = lda(Y_class~.,data=data.frame(X.lda))

Yhat.lda = predict(out.lda,data.frame(X_0.lda))

print(mean(Yhat.lda$class  == Y_0_class ) )

###
# glmnet: lasso
###
out         = cv.glmnet(X,Y_class,family='binomial',alpha=1,standardize=F)
Yhat.glmnet = predict(out,X_0,s='lambda.min',type='class')
print(mean(Yhat.glmnet  == Y_0_class ) )

betaHat.glmnet = coef(lasso.cv.glmnet,s='lambda.min')
which(abs(betaHat.glmnet) > 0)

#For the last one:

which(betaHat.glmnet < 0)
