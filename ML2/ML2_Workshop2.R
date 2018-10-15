# Machine Learning 2 - Workshop 2
# Tree Models: Ensemble Methods

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

###############################################
# Exercise 1 Understanding bootstrap sampling #
###############################################

func <- function(n,B) {
  result=matrix(nrow = B, ncol = 2)
  for (i in seq(1:B)) {
    vec<-1:n
    bs.samp<-sample(vec,size=n,replace=T)
    n.OOB<-n-sum(table((table(bs.samp))))
    result[i,1]=n.OOB
    result[i,2]=n.OOB/n
  }
  plot(cumsum(result[,2]),type="l")
  return (result)
}

res = func(n=100, B=5)
res = func(n=100, B=10)
res = func(n=100, B=25)
res = func(n=100, B=50)
res = func(n=100, B=100)

###################################################
# Exercise 2 Bagging a regression tree “longhand” #
###################################################

library(rpart)
library(MASS)

# Recreate the same training sample as in Workshop 1.
?Boston
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Put this in a loop and run B = 100 times, storing the above information 
# for each iteration (the predictions should be stored in a matrix).
B=100
result = matrix(nrow=B, ncol=3)
for (i in 1:B) {
  # Obtain one bagged (bootstrap) sample of the data
  bag.samp<-sample(train,size=length(train),replace=T)
  oob.samp<-train[-bag.samp]
  tree.bag=rpart(medv~.,Boston,subset=bag.samp)
  
  # Store the fitted values (=predicted values) and the out of bag predictions for this “bag”. 
  pred.bag = predict(tree.bag, newdata=Boston[bag.samp,])
  pred.oob = predict(tree.bag, newdata=Boston[oob.samp,])
  
  # Also store the predicted values for the training data.
  pred.test <- predict(tree.bag, newdata=Boston[-train,])
  
  # store the results
  result[i,1] = mean((Boston$medv[bag.samp]-pred.bag)^2) # in bag MSE
  result[i,2] = mean((Boston$medv[oob.samp]-pred.oob)^2) # out of bag MSE
  result[i,3] = mean((Boston$medv[-train]-pred.test)^2) # test MSE
}


par(mfrow=c(1,3))
plot(result[,1], main="In-bag MSE", ylim=c(min(result), max(result)))
plot(result[,2], main="Out-of-bag MSE", ylim=c(min(result), max(result)))
plot(result[,3], main="Test MSE", ylim=c(min(result), max(result)))
par(mfrow=c(1,1))

# MSE of the test set from last week
# test MSE non-pruned:  25.35825
# test MSE pruned:      25.82207

mean(result[,3]) # mean bagging test MSE
