forEachModelFunc = function(X,Y,n,validationSets,model){
	allObs = 1:n
	sweep = 1
	CV = rep(0,length(validationSets))
	for(i in validationSets){
		train  = allObs[-i]
		Xtrain = as.matrix(X[train,model],nrow= length(train),ncol=sum(model))
		Xtest  = as.matrix(X[i,model],nrow=length(i),ncol=sum(model))
		colnames(Xtrain) = names(X)[model]
		colnames(Xtest) = names(X)[model]
		out    = lm(Y[train]~.,data=data.frame(Xtrain))
		yHat   = predict(out,data.frame(Xtest))
		AMSEi  = mean((yHat - Y[i])^2)
		CV[sweep] = AMSEi
	}
	return(mean(CV))
}

CVfunc = function(X,Y,n,validationSets,models,print=FALSE){
	nModels = nrow(models)
	allCV = rep(0,nModels)
	for(i in 1:nModels){
		if(print == TRUE){
			print(i)
		}
		model = models[i,]
		allCV[i] = forEachModelFunc(X,Y,n,validationSets,model)
	}
	return(allCV)
}

