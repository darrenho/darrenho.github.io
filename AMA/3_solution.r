LAozone = read.table('/Users/darrenho/Dropbox/teaching/STAT460/data/LAozone.txt',sep=",",head=T)
Y = LAozone$ozone
X = as.matrix(LAozone[,names(LAozone)!='ozone'])

# Scale the data
X = scale(X, center = T, scale = T)
n = nrow(X)

X.df = LAozone[,names(LAozone)!= 'ozone']

lambda.grid = seq(1/(2*n)-.001,1/(2*n)+.001,length=1000)
ridge.out = glmnet(x = X, y = Y, alpha = 0,standardize=F,intercept=T,lambda=lambda.grid)
#coef(ridge.out, s = 1)
pred.ridge = predict(ridge.out, X, s = 1)


Ytilde = c(Y,rep(0,9))
X_matrix = rbind(X, diag(9))
X_matrix = cbind(c(rep(1,n),rep(0,9)),X_matrix) #adds intercept, but doesn't penalize it
str(X_matrix)
Xtilde = as.data.frame(X_matrix)
lm.out = lm(Ytilde~-1+.,data = Xtilde) #removes intercept from lm fit

X.new         = cbind(rep(1,n),X)#adds intercept
penalty.mat = diag(10)
penalty.mat[1,1] = 0 #removes penality on intercept
A = t(X.new) %*% X.new + penalty.mat
Y_matrix = as.matrix(LAozone$ozone)
b = t(X.new) %*% Y_matrix

coef(ridge.out, s = 1/(2*n))
coef(lm.out)

solve(A,b)
