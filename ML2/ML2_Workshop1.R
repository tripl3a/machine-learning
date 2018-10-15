# Machine Learning 2 - Workshop 1
# Tree Models: Revision

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(MASS)
?Boston

dim(Boston)
summary(Boston)
data=Boston


#########################
# Pairs
#########################

library(psych)
pairs.panels(data)
pairs.panels(data[,1:5])
# indus and nox are associated

pairs.panels(data[,5:10])
# age and dis and nox are associated
# rad and tax are associated strongly

pairs.panels(data[,10:14])
# lstat and medv are associated

pairs.panels(data[,c("dis","age","nox","indus")])
pairs.panels(data[,c("tax","rad")])

#########################
# Correlation
#########################

cormat <- round(cor(data),2)

library(leaps)
library(corrplot)
corrplot(cor(data), method = c("circle"))
corrplot(cor(data), method = c("number"))
cormat[,1]
# rad and tax have the strongest association with crim

corPlot(data, numbers=T)

###########################
# Do any of the suburbs of Boston appear to have particularly high crime rates? 
# Tax rates? Pupilteacher ratios?

boxplot(data$crim, main="crime rate")
plot(density(data$crim))
rug(jitter(data$crim))

boxplot(data$tax, main="tax")
plot(density(data$tax))
rug(jitter(data$tax))

boxplot(data$ptratio, main="pupil-teacher ratio")
plot(density(data$rad))
rug(jitter(data$rad))

#####
# How many of the suburbs in this data set bound the Charles river?
#####

dim(data[data$chas==1,])[1]

#####
# (f) What is the median pupil-teacher ratio among the towns in this data set?
#####

median(data$ptratio)

# (g) Which suburb of Boston has lowest median value of owner occupied homes? 
# What are the values of the other predictors for that suburb, and how do those 
# values compare to the overall ranges for those predictors ?

summary(data$age) # age = proportion of owner-occupied units built prior to 1940.
boxplot(data$age)
plot(density(data$age))
rug(jitter(data$age))

min(data$age)

# (h) In this data set, how many of the suburbs average more than seven rooms per dwelling? More
# than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per
# dwelling.

summary(data$rm) # rm = average number of rooms per dwelling.
boxplot(data$rm)
plot(density(data$rm)); rug(jitter(data$rm))

data[data$rm>8,]


#####
# Fit a regression tree
#####

library(rpart)
library(rpart.plot)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston=rpart(medv~.,Boston,subset=train)
print(tree.boston)
# the output indicates that only three of the variables have been used in constructing the tree

rpart.plot(tree.boston)

### would pruning improve the tree?

printcp(tree.boston)
plotcp(tree.boston)
# The rule suggested by the authors of the rpart package is to choose the smallest number of 
# nodes (largest cp value) which lies within 1 standard deviation of the smallest deviance, 
# i.e. lies below the dotted line.
prune.boston=prune(tree.boston,cp=0.016)
prune.boston
rpart.plot(prune.boston)

# Compare the mean square error (MSE) for the unpruned and pruned tree.

pred.train<-predict(tree.boston,newdata=Boston[train,])
mean((Boston$medv[train]-pred.train)^2)
pred.train.prune<-predict(prune.boston,newdata=Boston[train,])
mean((Boston$medv[train]-pred.train.prune)^2)

# Obtain the predictions for the full tree applied to the test data and for the 
# pruned tree applied to the test data. Calculate the MSE in both cases.

pred.test<-predict(tree.boston,newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test)^2)
pred.test<-predict(prune.boston,newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test)^2)

# Notice that for the test data the MSE for the pruned tree is only a little larger 
# than for the unpruned tree, but we have gained a slightly simpler model.

# The test set MSE associated with the regression tree is 25.82. The square root of 
# the MSE is therefore around 5.08, indicating that this model leads to test predictions 
# that are within around $5 080 of the true median home value for each suburb.

# Plot the observed median values medv against the pruned tree predictions (test data).

boston.test=Boston[-train,"medv"]
plot(pred.test,boston.test)
abline(c(0,1))

###################################################
# Exercise 2 Classification tree: prostate cancer #
###################################################

library(rpart)
?stagec

table(stagec$pgstat)

# Lets code this variable sa a factor variable, so that no progression 
# has the label “No” and progression has the label “Prog”.
stagec$progstat <- factor(stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))

# => This makes reading the output easier and rpart will recognise that the outcome variable 
# is a factor variable and so will use the Gini coefficient to calculate the loss statistic, 
# in order to determine each splits. As pgstat is numeric rpart would assume that a 
# regression tree is wanted, and will use mean square error for the loss function.

# Plot progstat against the other variables (ignoring pgtime).
# (time could be modelled using survival analysis methods.)
plot(pgstat~progstat,data=stagec)
plot(age~progstat,data=stagec)
plot(eet~progstat,data=stagec)
plot(g2~progstat,data=stagec)
plot(grade~progstat,data=stagec)
plot(gleason~progstat,data=stagec)
barplot(table(stagec$ploidy,stagec$progstat),beside=TRUE,legend.text=TRUE)

# As this data set has too few observations for a sensible test data set, 
# we will use the whole data for thetraining set.
# Fit the full tree and output it as text and a diagram.
c.tree <- rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,
                data = stagec)
rpart.plot(c.tree)
print(c.tree)

# As with the regression tree we should look to see if pruning the tree is better.
printcp(c.tree)
plotcp(c.tree)
c.pruned<-prune(c.tree,cp=0.076)
print(c.pruned)
rpart.plot(c.pruned)

# We will use a classifier with α=0.5, i.e. the mostlikely of the two outcomes is predicted
stagec$predict<-(predict(c.pruned)[,2]>0.5)
table(stagec$progstat,stagec$predict)
tt<-table(stagec$progstat,stagec$predict)
sens<-tt[2,2]/sum(tt[2,]);sens
spec<-tt[1,1]/sum(tt[1,]);spec

# => Comment on these values:
# speci[F]icty is very high, meaning the classifier predicts most of the negatives correcly
# at α = 0.5 level.
# sensi[T]ivity is pretty low, so the classifier often assigns wrong labels to the positives.
levels(stagec$progstat)
levels(stagec$progstat)[1] # negatives
levels(stagec$progstat)[2] # positives

#  The following code uses the ROCR package, to produce the ROC diagram and the AUC.

library(ROCR)
p <- predict(c.pruned)[,2]
#rpart function to get the prediction for Yes
pr <- prediction(p, stagec$progstat) #convert the predictions into ROCR format
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#ROCR function calculates everything for the ROC curve
plot(prf) #plot the ROC curve
abline(c(0,1))
AUC<-performance(pr, measure ="auc")@y.values[[1]];AUC
#RORC function calculates the AUC

# Use your values of the specificity and sensitivity in the previous part 
# to find where on the ROC curve this classifier sits.

# true positive rate (synonyms: 1−Type II error, power, sensitivity, recall)
sens
# false positive rate (synonyms: Type I error, 1−Specificity)
1-spec

points(1-spec,sens)


