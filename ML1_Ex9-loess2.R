library(MASS)

#the definintion of the tricube weight function
K<-function(d,maxd) (1 - (abs(d)/maxd)^3)^3

#define your x variable
x<-mcycle$times
#define your outcome variable variable
y<-mcycle$accel
  
##define x.grid the output coordinates for the loess curve
x.grid<-seq(min(x),max(x),length=100)  


span<-0.4
n<-length(x)
ninwindow<-round(span*n)
yloess<-rep(0,length(x.grid))
for(i in 1:length(x.grid)){
  x0<-x.grid[i] 
  xnearidx<-order(abs(x-x0))[1:ninwindow] # tricky statement
  xnear<-x[xnearidx]
  neardist<-abs(xnear-x0)
  weight<-rep(0,n)
  weight[xnearidx]<-K(neardist,max(neardist))
  
  lmx0<-lm(y~x,weights=weight)
  
  yloess[i]<-predict(lmx0,data.frame(x=x0))
  
}

plot(x,y)
lines(x.grid, yloess,col ="blue")

# Use the R-command loess() to replicate your algorithm in in one step. 
# Because you have fitted a linear local regression, 
# you need to specify the argument degree=1 in the loess function call.
fit = loess(accel~times,data=mcycle, degree=1, span=span)
lines(x.grid, predict(fit,data.frame(times=x.grid)), col="red")
