miss.class =function(pred.class,true.class,produceOutput=FALSE){
  confusion.mat = table(pred.class,true.class)
  if(produceOutput){
    return(1-sum(diag(confusion.mat))/sum(confusion.mat))	
  }
  else{
    print('miss-class')
    print(1-sum(diag(confusion.mat))/sum(confusion.mat))
    print('confusion mat')
    print(confusion.mat)
  }
}

load("spam.Rdata")

train = spam$train
test  = !train
X     = spam$XdataF[train,]
X_0   = spam$XdataF[!train,]
Y     = factor(spam$Y[train])
Y_0   = factor(spam$Y[!train])

covariate_labels = spam$covariate_labels

require(tree)
##############
# TREE THINGS
##############
out.tree = tree(Y~.,data=X)
Y.hat    = predict(out.tree,data.frame(X_0),'class')
miss.class(Y.hat,Y_0)

#pdf('../lectures/figures/classification_trees_intro_HIV.pdf')
tmp.tree = prune.tree(out.tree,best=3)
plot(tmp.tree)
text(tmp.tree)
#dev.off()

out.tree.orig   = tree(Y~.,data=X)
out.tree.cv     = cv.tree(out.tree.orig,FUN=prune.misclass)
best.size       = out.tree.cv$size[which.min(out.tree.cv$dev)]
best.size       = out.tree.cv$size[max(which(out.tree.cv$dev == min(out.tree.cv$dev)))]
best.size
out.tree        = prune.misclass(out.tree.orig,best=best.size)
class.tree      = predict(out.tree,X_0,type='class')
class.tree.orig = predict(out.tree.orig,X_0,type='class')
miss.class(class.tree,Y_0)
miss.class(class.tree.orig,Y_0)
