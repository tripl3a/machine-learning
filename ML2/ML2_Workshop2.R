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
library(data.table)
library(dplyr)

# Recreate the same training sample as in Workshop 1.
?Boston
Boston$ID <- seq.int(nrow(Boston)) # add ID column for later use
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Put this in a loop and run B = 100 times, storing the above information 
# for each iteration (the predictions should be stored in a matrix).
B=100
result.bag = data.table(NULL)
for (i in 1:B) {
  # Obtain one bagged (bootstrap) sample of the data and fit a tree
  bag.samp<-sample(train,size=length(train),replace=T)
  oob.samp<-train[-bag.samp]
  tree.bag=rpart(medv~.,Boston,subset=bag.samp)
  
  # Store the fitted values (=predicted values) and the out of bag predictions for this “bag”. 
  pred.bag = predict(tree.bag, newdata=Boston[bag.samp,])
  pred.oob = predict(tree.bag, newdata=Boston[oob.samp,])
  
  # Also store the predicted values for the test data.
  pred.test <- predict(tree.bag, newdata=Boston[-train,])
  
  # append results to data.table
  result.bag = rbind(result.bag, data.table(bag=B, obs=names(pred.bag), value=unname(pred.bag), pred.type="bag"))
  result.bag = rbind(result.bag, data.table(bag=B, obs=names(pred.oob), value=unname(pred.oob), pred.type="oob"))
  result.bag = rbind(result.bag, data.table(bag=B, obs=names(pred.test), value=unname(pred.test), pred.type="test"))
}
result.bag$obs = as.integer(result.bag$obs) # assign consistent names for observations which occured multiple times within a bag
result.bag = unique(result.bag) # and now remove the duplicates

# take the average as the ensemble's prediction for each observations
pred.bag = result.bag %>%
  group_by(obs,pred.type) %>%
  summarise(avg.pred = mean(value))
pred.bag = as.data.table(pred.bag)

# calculate the MSE for the test set
bag_test_mse = mean((Boston[-train,"medv"] - pred.bag[pred.type=="test",]$avg.pred)^2)
bag_test_mse

# for comparison:
# MSE of the test set from last week
# test MSE non-pruned:  25.35825
# test MSE pruned:      25.82207

#############################
# Exercise 3 Bagging with R #
#############################

library(MASS)
library(randomForest)
library(rpart)

# Recreate the same training sample as in Workshop 1.
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# mtry = 13 specifies that all 13 of the possible explanatory variables are considered, 
# thus generating bagged trees
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

pred.test <- predict(bag.boston, newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test)^2) # test MSE
# => randomForest gives a noticeably better result. The tree fitting options have been optimised for
# bagging, which for simplicity we did not do in Exercise 2.

#######################################################
# Exercise 4 Random forest regression trees “longhand #
#######################################################

B=100
result.rf = data.table(NULL)
for (i in 1:B) {
  # In each bag, sample four variables and include the 14th variable which is medv.
  var.samp<-c(sample(1:13,size=3,replace=FALSE),14)
  data=Boston[train,var.samp]
  
  # Obtain one bagged (bootstrap) sample of the data and fit a tree
  bag.samp<-sample(train, size=length(train), replace=T)
  oob.samp<-train[-bag.samp]
  tree.bag=rpart(medv~., data, subset=bag.samp)
  
  # Store the fitted values (=predicted values) and the out of bag predictions for this “bag”. 
  pred.bag = predict(tree.bag, newdata=Boston[bag.samp,var.samp])
  pred.oob = predict(tree.bag, newdata=Boston[oob.samp,var.samp])
  
  # Also store the predicted values for the test data.
  pred.test <- predict(tree.bag, newdata=Boston[-train,var.samp])
  
  # append results to data.table
  result.rf = rbind(result.rf, data.table(bag=B, obs=names(pred.bag), value=unname(pred.bag), pred.type="bag"))
  result.rf = rbind(result.rf, data.table(bag=B, obs=names(pred.oob), value=unname(pred.oob), pred.type="oob"))
  result.rf = rbind(result.rf, data.table(bag=B, obs=names(pred.test), value=unname(pred.test), pred.type="test"))
}
result.rf$obs = as.integer(result.rf$obs) # assign consistent names for observations which occured multiple times within a bag
result.rf = unique(result.rf) # and now remove the duplicates

# take the average as the ensemble's prediction for each observations
pred.rf = result.rf %>%
  group_by(obs,pred.type) %>%
  summarise(avg.pred = mean(value))
pred.rf = as.data.table(pred.rf)

# calculate the MSE for the test set
rf_test_mse = mean((Boston[-train,"medv"] - pred.rf[pred.type=="test",]$avg.pred)^2)
rf_test_mse


####################################
# Exercise 5 Random forest using R #
####################################

library(MASS)
library(randomForest)
library(rpart)

# Recreate the same training sample as in Workshop 1.
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# we'll use 3 variables for each tree here (mtry=3)
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=3, importance=TRUE)
rf.boston

pred.test <- predict(rf.boston, newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test)^2) # test MSE

##################################
# Exercise 6 Variable importance #
##################################

library(rpart.plot)

importance(bag.boston)
varImpPlot(bag.boston)

importance(rf.boston)
varImpPlot(rf.boston)

tree.boston=rpart(medv~.,Boston,subset=train)
tree.boston$variable.importance #  includes variables that do not appear in the final tree
rpart.plot(tree.boston)
