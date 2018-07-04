# Machine Learning 1
# Workshop 5
# K-Nearest Neighbour Regression

require(FNN)
require(rgl)

############################
# One explanatory variable #
############################

x<-1:20
y<-rnorm(length(x),mean=x+10)
xgrid<-data.frame(x)

# Fit the KNN regression with K = 1 and compare the fitted values with the outcome variable
knnr.out<-knn.reg(x,test=xgrid,y=y,k=1)
round(cbind(x,y,knnr.out$pred),2)

# Repeat this with the value k = 3
knnr.out<-knn.reg(x,test=xgrid,y=y,k=3)
round(cbind(x,y,knnr.out$pred),2)
(28.44+29.72+30.01)/3 # the two last fitted values


# To plot the predicted values as a function, we will specify a much finer grid for the predicted values.
# Again start with k = 1.
xgrid<-data.frame(x=seq(0,21,0.05))
knnr.out<-knn.reg(x,test=xgrid,y=y,k=1)
plot(x,y)
lines(xgrid$x,knnr.out$pred)
# Gradually increase the value of K and observe what happens to the predictor function. Which values of
# K correspond to under fitting and which values to over fitting?

par(mfrow=c(2,2))
for (k in 2:5) {
  knnr.out<-knn.reg(x,test=xgrid,y=y,k=k)
  plot(x,y,main = paste("k=",k))
  lines(xgrid$x,knnr.out$pred)
}
par(mfrow=c(1,1))
# k=4 looks quite good as the predictions follow the general trend and 
# don't jump too much if there is one observation outside that trend 

# For completeness and a bit of revision we will have a look at the linear regression for these data.
lm.obj<-lm(y~x)
summary(lm.obj)
plot(x,y)
abline(lm.obj,col=2)
knnr.out<-knn.reg(x,test=xgrid,y=y,k=19)
lines(xgrid[,1],knnr.out$pred,col=3)
# Note that the linear regression model fits the data very well and requires only two parameters. Although
# for non-parametric models we do not use the concept of the number of "parameters" a comparison would
# be using just two constant functions i.e. k = 19 which massively under fits these data. This is somewhat
# unfair though, as the example data is ideal for a linear regression model.

#############################
# Two explanatory variables #
#############################

fit<-as.data.frame(matrix(c(1,87, 42,6, 73, 43,7, 66, 44,15,62,54,12,
                            68,45,4,92,46,12,60,50,13,70,46,14,71,54,10,64,47),byrow=T,ncol=3))
names(fit)<-c("fitness","weight","lungvol")
summary(fit)

# The linear model for fitness dependent on weight and lung volume is fitted using:
lm.fitness <- lm(fitness~weight+lungvol, data=fit)
summary(lm.fitness)
# Both explanatory variables are significant at the 5% level. 
# Use this output to write the predictor function as a function of the two predictor variables.

coeffs <- summary(lm.fitness)$coefficients
beta0 <- coeffs[1]
beta1 <- coeffs[2]
beta2 <- coeffs[3]
y.hat = beta0 + beta1*fit$weight + beta2*fit$lungvol 
y.hat - fit$fitness
summary(lm.fitness)$residuals

# We will plot the two variables using (i) a perspective plot and (ii) a 3-d plot. In both cases we need to
# compute the predicted values at points on a grid with weight taking values between 55 and 95 Kg and
# lung volume between 40 and 55 dl.

m1<-seq(55,95,length=20)
m2<-seq(40,55,length=20)
Xgrid<-expand.grid(weight=m1,lungvol=m2)
pred.grid<-predict(lm.fit,newdata=Xgrid)
tt<-cbind(Xgrid,pred.grid)
res<-persp(m1,m2,matrix(pred.grid,nrow=length(m1)),border=grey(0.6),
           xlab="Weight",ylab="Lungvol",zlab="fitness",theta=0,phi=15)
points(trans3d(fit$weight,fit$lungvol,fit$fitness,pmat=res),
       pch=16,col = c("DarkRed","orange")[1.5+.5*sign(lm.fit$residuals)])

#################### ... to be continued ... ####################