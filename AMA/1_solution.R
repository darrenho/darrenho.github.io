###STAT 460 Homework 1 Solutions: Last

##creates the matrix, calls it A
A = matrix(1:12,ncol=4,nrow=3)


## gets column means via different methods:

##### hard coding #############

#first column mean
(A[1,1]+ A[2,1] + A[3,1])/3
#second column mean
(A[1,2]+ A[2,2] + A[3,2])/3
#third column mean
(A[1,3]+ A[2,3] + A[3,3])/3
#fourth column mean
(A[1,4] + A[2,4] + A[3,4])/3


#### for loop #################

for (j in 1:4){
  print(mean(A[,j]))
}
#comment: using for loops is not usually the optimal solution in R
#or using just for loops
colMeans = rep(0,ncol(A))
for (i in 1:nrow(A)){
  for (j in 1:ncol(A)){
	  colMeans[j] = colMeans[j] + A[i,j]/nrow(A)
  }
}

####
# Note: the above code will work for ANY matrix A, no "hard coding" 
#       nor case-specific changes necessary
####


###using the apply function ####
#Note: use 2 because we want the column means, use 1 for row means
apply(A,2,mean)
