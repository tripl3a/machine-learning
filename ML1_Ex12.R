# Machine Learning 1
# Workshop 12
# Tree Models: regression and classification trees

#############
# Exercise 1
#############

library(rpart)
library(rpart.plot)

library(ISLR)
attach(Carseats)

High=ifelse(Sales<=8,"no","yes")
Carseats=data.frame(Carseats,High)

tree.carseats = rpart(High~.-Sales,Carseats)
tree.carseats
summary(tree.carseats)
rpart.plot(tree.carseats)

# train-/test-split and evaluation of the prediction performance
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test= Carseats[-train,]
High.test = High[-train]
tree.carseats = rpart(High~.-Sales, Carseats, subset=train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(90+64)/length(tree.pred) # ratio of correct predictions

# With rpart the cross-validation is already processed, we just need to access the results. 
printcp(tree.carseats)
plotcp(tree.carseats)
# dotted line at: last value + 1*sd

# prune the tree using the first cp value which falls below the dotted line
pruned.tree = prune(tree.carseats,cp=0.032)
pruned.tree
rpart.plot(pruned.tree)

pruned.pred = predict(pruned.tree, Carseats.test, type="class")
table(pruned.pred, High.test)
(93+56)/length(pruned.pred) # ratio of correct predictions

# In general the settings in rpart() lead to a tree with less nodes than using tree(), as a result the
# rpart() tree is closer to the optimal tree, and pruning is not as essential. In these two example the
# prediction accuracy on the test data is slightly worse for the pruned tree, you should not assume that 
# this is always the case.


#############################
# Exercise 2 
# Brexit referendum results
#############################

setwd("~/git-reps/machine-learning")
load("data/Brexit.Rda")


