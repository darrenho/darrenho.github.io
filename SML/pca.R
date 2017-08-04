# Most of the code adapted from faraway 2004

# Principle Components Regression Example
data(meatspec, package="faraway") # load data
rmse <- function(x,y) sqrt(mean((x-y)^2)) # Define root mean sequared error function

# Split the data into a training set and a test set
trainmeat <- meatspec[1:172,]
testmeat <- meatspec[173:215,]

# Run a simple least squares and check the rmse for training and testing data sets
modlm <- lm(fat ~ ., trainmeat)
summary(modlm)$r.squared
rmse(fitted(modlm), trainmeat$fat)
rmse(predict(modlm,testmeat), testmeat$fat)

# Run stepwise variable selection and check the rmse for training and testing data sets
modsteplm <- step(modlm,trace=0)
length(modsteplm$coefficients)-1 # Check how many variables we end up selecting 
rmse(modsteplm$fit, trainmeat$fat)
rmse(predict(modsteplm,testmeat), testmeat$fat)

# Performing PCA using SVD

# Scale the data for PCA
x.center <- apply(trainmeat[,-101],2,mean)
trainx <- sweep(trainmeat[,-101],2,x.center)/sqrt(172-1)
trainx <- as.matrix(trainx)
meatsvd <- svd(trainx) # Perform SVD for X
meatsvd$d # Standard deviations for each principle component
head(meatsvd$v[,1:4]) # First few princple components

# Performing PCA using 'prcomp' function

meatpca <- prcomp(trainmeat[,-101])
meatpca$sdev # Standard deviations for each principle component
head(meatpca$rot[,1:4]) # First few princple components
round(meatpca$sdev,3) # Round standard devitions for each principle
matplot(1:100, meatpca$rot[,1:3], type="l", xlab="Frequency", ylab="", col=1) # Plot first few principle components
# Fit the first few PC's to data and check fit using RMSE
model3 <- lm(fat ~ meatpca$x[,1:4], meatspec[1:172,])
rmse(model3$fit,meatspec$fat[1:172])
svb <- meatpca$rot[,1:4] %*% model3$coef[-1] # Principle components scores

# Peforming PCA using PLS package

require(pls)
pcrmod <- pcr(fat ~ ., data=trainmeat, ncomp=50)
rmse(predict(pcrmod, ncomp=4), trainmeat$fat)
# Plot LS coefficients
plot(modlm$coef[-1],xlab="Frequency",ylab="Coefficient",type="l")
# Plot PCA coefficients
coefplot(pcrmod, ncomp=4, xlab="Frequency",main="")

# Plot scree plot using 'screeplot' and using 'plot'
screeplot(meatpca,type="lines")
plot(meatpca$sdev[1:10],type="l",ylab="SD of PC", xlab="PC number")
# Calculate RMSE on test data using 4 PC and also using 'RMSEP' function
rmse(predict(pcrmod, testmeat, ncomp=4), testmeat$fat)
pcrmse <- RMSEP(pcrmod, newdata=testmeat)
# Plot RMSE for PCR for each number of PC's.
plot(pcrmse,main="")
# Number of PC's with least RMSE
which.min(pcrmse$val)
# The value of the least RMSE
pcrmse$val[28]

# Use CV to decide on number of PC's.
pcrmod <- pcr(fat ~ ., data=trainmeat, validation="CV", ncomp=50)
pcrCV <- RMSEP(pcrmod, estimate="CV")
# Plot OF CV RMSE
plot(pcrCV,main="")
# Which number of components has the least RMSE
which.min(pcrCV$val)
# Calculate prediction and RMSE for the prediction
ypred <- predict(pcrmod, testmeat, ncomp=18)
rmse(ypred, testmeat$fat)

# Partial Least Squares Example

# Run PLS on the data
plsmod <- plsr(fat ~ ., data=meatspec[1:172,], ncomp=50,  validation="CV")
# Plot coefficients values for PLS
coefplot(plsmod, ncomp=4, xlab="Frequency")
# Calculate CV RMSE and plot it
plsCV <- RMSEP(plsmod, estimate="CV")
plot(plsCV,main="")
# Calculate the prediction and RMSE for both training and testing data set.
ypred <- predict(plsmod,ncomp=15)
rmse(ypred, trainmeat$fat)
ytpred <- predict(plsmod, testmeat, ncomp=15)
rmse(ytpred, testmeat$fat)
