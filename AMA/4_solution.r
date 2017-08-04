library(glmnet)
?glmnet
### HIV data
load("../../../data/hiv.rda")
X = hiv.train$x
Y = hiv.train$y

p = ncol(X)
n = nrow(X)

dim(X)
table(X)


library(leaps)
out.for = regsubsets(x=X,y=Y,nvmax=p,method='forward')
sum.for = summary(out.for)
model.for = sum.for$which[which.min(sum.for$bic),]


library(glmnet)
ridge.cv.glmnet = cv.glmnet(X,Y,alpha=0)
min.lambda = min(ridge.cv.glmnet$lambda)
lambda.new = seq(min.lambda, min.lambda*0.01,length=100)
ridge.cv.glmnet = cv.glmnet(x = X, y = Y, alpha = 0,lambda = lambda.new)

X_0 = hiv.test$x
Y_0 = hiv.test$y

Yhat.test = predict(ridge.cv.glmnet,X_0,s='lambda.min')

Yhat.test = predict(ridge.cv.glmnet$glmnet.fit,X_0)#,s='lambda.min')

pred.error = apply( (Yhat.test - Y_0)**2,2,mean)

pdf('CVonlyPlot.pdf')
plot(ridge.cv.glmnet$lambda,ridge.cv.glmnet$cvm,ylab='CV',xlab='lambda',ylim=c(0,.12),pch=16,col='red',cex=.6)
abline(v=ridge.cv.glmnet$lambda[which.min(ridge.cv.glmnet$cvm)],col='red')
abline(h=min(ridge.cv.glmnet$cvm),col='red',lty=2)
dev.off()

pdf('CVandPredErrorPlot.pdf')
plot(ridge.cv.glmnet$lambda,ridge.cv.glmnet$cvm,ylab='CV',xlab='lambda',ylim=c(0,.12),pch=16,col='red',cex=.6)
points(ridge.cv.glmnet$lambda,pred.error,pch=17,col='blue',cex=.6)
abline(v=ridge.cv.glmnet$lambda[which.min(pred.error)],col='blue')
abline(v=ridge.cv.glmnet$lambda[which.min(ridge.cv.glmnet$cvm)],col='red')
abline(h=min(ridge.cv.glmnet$cvm),col='red',lty=2)
abline(h=min(pred.error),col='blue',lty=2)
dev.off()



#CV estimate of the prediction error: 
min(ridge.cv.glmnet$cvm)


