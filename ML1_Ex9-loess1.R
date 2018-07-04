library(MASS)

#the definintion of the tricube weight function
K<-function(d,maxd) (1 - (abs(d)/maxd)^3)^3

#define your x variable
x<-mcycle$times
#define your outcome variable variable
y<-mcycle$accel

#loess parameter
span<-0.75
#the x value to estimate f(x) using local regression 
x0<-14
n<-length(x)
ninwindow<-round(span*n)

#Find the index numbers of the ninwindow elements closest to x0
xnearidx<-order(abs(x-x0))[1:ninwindow]; xnearidx # tricky statement
#The x values of these nearby elements are 
xnear<-x[xnearidx]; xnear
#and their distances from x0 are 
neardist<-abs(xnear-x0); neardist

#define the weights using K if near and zero if not
weight<-rep(0,n)
weight[xnearidx]<-K(neardist,max(neardist))
print(round(weight,3))

#fit a weighted linear regression 
lmx0<-lm(y~x,weights=weight)

prx0=predict(lmx0,newdata=list(x=x0))
plot(x,y, main=paste("span =",span,",","x0 =",x0))
abline(lmx0)
points(x0,prx0,col=2,pch=16)
