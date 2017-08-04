LAozone = read.table('/Users/darrenho/Dropbox/teaching/STAT460/data/LAozone.txt',sep=",",head=T)
library(leaps)

Y = ozone$ozone
X = ozone[,names(ozone)!=c('ozone')]

X.poly = cbind(X,X**2)
main.effects = names(X)
sq.effects   = paste(main.effects,'.Sq',sep='')
names(X.poly) = c(main.effects,sq.effects)


#1a
summary(lm(Y~.,data=X))

###
#1b
###

#i
regfit.exh     = regsubsets ( x = X,y = Y, nvmax =19 ,method ="exhaustive")
regfit.exh.sum = summary(regfit.exh)
regfit.exh.sum$which[which.min(regfit.exh.sum$bic),]
plot(regfit.exh)
plot(regfit.exh.sum$cp,xlab = "Number of Variables", ylab ="Cp",type='l',main='All Subsets')
abline(v=which.min(regfit.exh.sum$cp))

#ii
##ii A
lm_full = lm(Y~.,data=X)
lm_null = lm(Y~1,data=X)
step(lm_null,scope=list(lower=lm_null,upper=lm_full),direction='forward')
##ii B
regfit.for = regsubsets ( x = X,y = Y, nvmax =19 ,method ="forward")
regfit.for.sum = summary(regfit.for)
regfit.for.sum$which[which.min(regfit.for.sum$cp),]
#iii
regfit.bac = regsubsets ( x = X,y = Y, nvmax =19 ,method ="backward")
regfit.bac.sum = summary(regfit.bac)
regfit.bac.sum$which[which.min(regfit.bac.sum$cp),]
# iv
lm_full = lm(Y~.,data=X)
lm_null = lm(Y~1,data=X)
step(lm_null,scope=list(lower=lm_null,upper=lm_full),direction='both')

###
# 1d
### 
#i
regfit.exh = regsubsets ( x = X.poly,y = Y, nvmax =19 ,method ="exhaustive")
regfit.exh.sum = summary(regfit.exh)
regfit.exh.sum$which[which.min(regfit.exh.sum$bic),]
plot(regfit.exh)


#1c Make a plot
pdf('/Users/darrenho/Dropbox/teaching/STAT460/homeworks/homework1problem1c.pdf')
par(mfrow=c(3,1))
plot(regfit.exh.sum$cp,xlab = "Number of Variables", ylab ="Cp",type='l',main='All Subsets')
abline(v=which.min(regfit.exh.sum$cp))
plot(regfit.for.sum$cp,xlab = "Number of Variables", ylab ="Cp",type='l',main='Forward')
abline(v=which.min(regfit.for.sum$cp))
plot(regfit.bac.sum$cp,xlab = "Number of Variables", ylab ="Cp",type='l',main='Backward')
abline(v=which.min(regfit.bac.sum$cp))
dev.off()

summary(lm(Y~.,data=X.poly))