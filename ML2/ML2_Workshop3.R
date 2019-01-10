#Machine Learning 2 Workshop 3 
#Exercise 1 Boosting

#a)
library (gbm)
set.seed (1)

#b)one iteration
#i) 
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=1,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
#ii)
mtcars.boost

#iii)
#mse of null model
mean((mtcars$mpg-mean(mtcars$mpg))^2)
#mse of first boost
mtcars.boost$train.error

#iv)
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(19,22),ylab="fitted values",xlab="observed values")
abline(h=mean(mtcars$mpg))
abline(c(0,1))
#v)
plot(mtcars.boost)

#c)
#two iterations
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=2,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
mtcars.boost$train.error
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(19,22),ylab="fitted values",xlab="observed values")
points(mtcars$mpg,predict(mtcars.boost,n.trees=2),pch=2)

#d) three iterations
#i)
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=3,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
mtcars.boost$train.error
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(18,23),ylab="fitted values",xlab="observed values")
points(mtcars$mpg,predict(mtcars.boost,n.trees=2),pch=2)
points(mtcars$mpg,predict(mtcars.boost,n.trees=3),pch=3)
#ii)
summary(mtcars.boost)
#iii)
plot(mtcars.boost,i.var="cyl")
plot(mtcars.boost,i.var="disp")

#e) ten iterations
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=10,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
#i)
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(18,23),ylab="fitted values",xlab="observed values")
points(mtcars$mpg,predict(mtcars.boost,n.trees=4),pch=3)
summary(mtcars.boost, n.trees=4)
plot(mtcars.boost,i.var="cyl",n.trees=4)
plot(mtcars.boost,i.var="disp",n.trees=4)
plot(mtcars.boost,i.var="hp",n.trees=4)

#ii)
summary(mtcars.boost, n.trees=5)
plot(mtcars.boost,i.var="cyl",n.trees=5)
#iii)
matplot(t(predict(mtcars.boost,n.trees=1:5)),type="l",ylab="fitted values",xlab="iteration")
#iv)
summary(mtcars.boost, n.trees=10)
plot(mtcars.boost,i.var="cyl")
plot(mtcars.boost,i.var="disp")
plot(mtcars.boost,i.var="hp")
plot(mtcars.boost,i.var="wt")
matplot(t(predict(mtcars.boost,n.trees=1:10)),type="l",ylab="fitted values",xlab="iteration")


#f)
mtcars.boost<-gbm.more(mtcars.boost,n.new.trees = 90)
matplot(t(predict(mtcars.boost,n.trees=1:100)),type="l",ylab="fitted values",xlab="iteration")
summary(mtcars.boost)
plot(1:100,mtcars.boost$train.error,type="l")


#g)
mtcars.boost<-gbm.more(mtcars.boost,n.new.trees = 900)
matplot(t(predict(mtcars.boost,n.trees=1:1000)),type="l",ylab="fitted values",xlab="iteration")
summary(mtcars.boost)
plot(1:1000,mtcars.boost$train.error,type="l")

mtcars.boost$train.error[c(1,100,1000)]
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1000),ylab="fitted values",xlab="observed values")
abline(h=mean(mtcars$mpg))
abline(c(0,1))


#h)
error0.1<-mtcars.boost$train.error
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=1000,distribution="gaussian",shrinkage=0.01,bag.fraction=1)
plot(1:1000,error0.1,type="l")
lines(1:1000,mtcars.boost$train.error,col=3)
