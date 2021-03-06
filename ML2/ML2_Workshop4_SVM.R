#############################
# Support Vector Classifier #
#############################

# create data set
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))
dat=data.frame(x=x, y=as.factor(y))

library(e1071)
svmfit=svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)

# use different value for `cost`
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index

# tuning the `cost` hyper-parameter 
set.seed(1)
tune.out=tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 5, 10, 100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

# generate test data
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

# make predictions
ypred=predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)
