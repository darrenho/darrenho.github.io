subSample = FALSE
full      = TRUE

if(subSample){
  load(file='/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/XtrainSubSample.Rdata')
  Xtrain = XtrainSubSample
}
if(full){
  Xtrain = read.csv(file='/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/Xtrain.csv')  
}


Ytrain = Xtrain$target
Xtrain = subset(Xtrain,select=-target)

p = ncol(Xtrain)

#eachMarginalModel = vector('list',p)

#Get rid of 1 factor covariates or covariates with insufficient variation
pGridF = function(p,verbose=FALSE){
  pGrid = 1:p
  for(j in p:1){
    X = Xtrain[,j]
    if(is.factor(X)){
      X.tmp = as.character(X)
      X.tmp = as.factor(X.tmp)
      if(nlevels(X.tmp)==1){
        if(verbose)cat('factor: ', j, '\n')
        pGrid = pGrid[-j]
      }
    }
    if(is.numeric(X)){
      X.sd = sd(X)
      if(X.sd < 1e-14){
        if(verbose)cat('numeric: ', j, '\n')
        pGrid = pGrid[-j]
      }
    }
  }
  return(pGrid)
}
AIC_old = -1
AIC_new = 0
j_starVec = NULL
out.df_starVec = NULL
allCovNames = names(Xtrain)
keepModelVec = list()
modelSweep = 0
keepGoingFlag = TRUE


pGrid_0 = pGridF(p)
pGrid = pGridF(p)
while(AIC_new > AIC_old & keepGoingFlag){
  modelSweep = modelSweep + 1
  start.tm = proc.time()[3]
  min.deviance = 0
  for(j in pGrid){
    if(j %% 100 == 0){
      print(j)
    }
    select = c(j_starVec,j)
    fmla   = as.formula(paste("Ytrain ~ ", paste(allCovNames[select], collapse= "+")))
    out.glm = glm(fmla,family=binomial(),data=Xtrain)
    #eachMarginalModel[[j]] = out.glm
    out.deviance = out.glm$null.deviance - out.glm$deviance
    if(out.deviance < 1e-16){
      next
    }
    out.df = out.glm$df.null - out.glm$df.residual
    
    if(out.deviance/out.df > min.deviance){
      min.deviance      = out.deviance/out.df
      j_star            = j
      out.deviance_star = out.deviance
      out.df_star       = out.df
      keepModel         = out.glm
    }
  }
  j_starVec                  = c(j_starVec,j_star)
  out.df_starVec             = c(out.df_starVec,out.df_star)
  keepModelVec[[modelSweep]] = keepModel
  AIC_old = AIC_new
  AIC_new = out.deviance_star - 2*sum(out.df_starVec)
  stop.tm = proc.time()[3]
  cat('time: ',stop.tm-start.tm,'\n')
  pGrid = pGrid[-which(pGrid==j_star)]
  print(j_star)
  print(pGrid_0[-pGrid])
  #if(stop.tm-start.tm > 500){
  #  keepGoingFlag = FALSE
  #}
}

return.list = list()
return.list$AIC = AIC_old
return.list$j_starVec = j_starVec
return.list$out.df_starVec = out.df_starVec
return.list$keepModelVec = keepModelVec

save(return.list,file='/Users/darrenho/Dropbox/research/kaggle/springLeafMarketing/forwardLogisticFull.Rdata')

debug.tmp = FALSE
if(debug.tmp){
  fmla   = as.formula(paste("Ytrain ~ ", paste(allCovNames[j_starVec], collapse= "+")))
  out.glm = glm(fmla,family=binomial(),data=Xtrain)
}
