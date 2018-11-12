setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("data/NNdatasets.Rda")

###################
#Exercise 1
#define the sigmoid function (inverse-logit)

inv.logit<-function(v){
  1 / (1 + exp(-v))
}

#Define the neural network function
NN<-function(param,x1,x2)
{
  #This function is the neural network
  #Input data is x1 and x2
  #weights are in the param vector 
  
  ## unpick the param vector
  whl11<-param[1]
  whl12<-param[2]
  bhl1<-param[3]
  wol1<-param[4]
  bol<-param[5]
  
  #  hidden layer
  z1<-whl11*x1+whl12*x2+bhl1
  # activation
  a1<-inv.logit(z1)
  
  #output layer
  a2<-wol1*a1+bol
  return(a2)
}  


#initialise parameters
whl11.curr<-0.01
whl12.curr<-0.01
bhl1.curr<-0
wol1.curr<-10
bol.curr<-40
n<-length(x1)

bestSSE<-Inf

niter<-2000
window<-0.1
for(iter in 1:niter){
  #We update parameter in turn and select the update if it gives a lower SSE   
  
  Delta<-rep(0,5)  #initialise the change vector
  # define which parameter to perturb
  j<- (iter %% 5) +1
  
  window<-max(window*0.999,0.001) #for later reduce the window size incrementally
  
  Delta[j]<-rnorm(1,0,window) #generate the change in parameter and assign it
  
  #define the new parameter vector 
  whl11<-whl11.curr+Delta[1]
  whl12<-whl12.curr+Delta[2]
  bhl1<-bhl1.curr+Delta[3]
  wol1<-wol1.curr+Delta[4]
  bol<-bol.curr+Delta[5]
  
  
  
  #call the NN function. uses R-vector arithmetic 
  fitted<-NN(c(whl11,whl12,bhl1,wol1,bol),x1,x2)
  
  #loss function
  SSE<-sum((fitted-y)^2)
  
  #if SSE is better, then update 
  if(SSE<bestSSE){
    whl11.curr<-whl11
    whl12.curr<-whl12
    bhl1.curr<-bhl1
    wol1.curr<-wol1
    bol.curr<-bol
    bestSSE<-SSE
    best.fitted<-fitted
    
    #print(bestSSE)
  }
  
  
}

print(bestSSE)
plot(y,best.fitted);abline(c(0,1))


#for the "best" parameter vector so far, obtain the predicted values
test.predicted<-NN(c(whl11.curr,whl12.curr,bhl1.curr,wol1.curr,bol.curr),x1test,x2test)
testSSE<-sum((test.predicted-ytest)^2)
print(testSSE)
plot(ytest,test.predicted);abline(c(0,1))


#The best parameters are
whl11.curr
whl12.curr
bhl1.curr
wol1.curr
bol.curr

###########################################
#Exercise 2
#Adding a second node to the hidden layer

NN2<-function(param,x1,x2)
{
  #This function is the neural network
  #Input data is x1 and x2
  #weights are in the param vector 
  
  ## unpick the param vector
  whl11<-param[1]
  whl12<-param[2]
  whl21<-param[3]
  whl22<-param[4]
  bhl1<-param[5]
  bhl2<-param[6]
  wol1<-param[7]
  wol2<-param[8]
  bol<-param[9]
  
  #  hidden layer
  z1<-whl11*x1+whl12*x2+bhl1
  z2<-??????
  # activation
  a11<-inv.logit(z1)
  a12<-???
  
  #output layer
  a2<-????
  return(a2)
}  

#initialise parameters
whl11.curr<-0.01
whl12.curr<-0.01
whl21.curr<-0.01
whl22.curr<-0.01
bhl1.curr<-0
bhl2.curr<-0
wol1.curr<-30
wol2.curr<-30
bol.curr<-20


bestSSE<-Inf

niter<-10
window<-0.1
for(iter in 1:niter){
  #We update parameter in turn and select the update if it gives a lower SSE   
  
  Delta<-rep(0,9)  #initialise the change vector
  # define which parameter  to perturb
  j<- (iter %% 9) +1
  
  #  window<-max(window*0.999,0.001) #for later
  
  Delta[j]<-rnorm(1,0,window) #generate the change in parameter and assign it
  whl11<-whl11.curr+Delta[1]
  whl12<-whl12.curr+Delta[2]
  whl21<-whl21.curr+Delta[3]
  whl22<-whl22.curr+Delta[4]
  bhl1<-bhl1.curr+Delta[5]
  bhl2<-bhl2.curr+Delta[6]
  wol1<-wol1.curr+Delta[7]
  wol2<-wol2.curr+Delta[8]
  bol<-bol.curr+Delta[9]
  
  param<-c(whl11,whl12,whl21,whl22,bhl1,bhl2,wol1,wol2,bol)
  
  #call NN2 for each observation
  fitted<-???
  
  
  SSE<-sum((fitted-y)^2)
  
  #if SSE is better update 
  if(SSE<bestSSE){
    whl11.curr<-whl11
    whl12.curr<-whl12
    whl21.curr<-whl21
    whl22.curr<-whl22
    bhl1.curr<-bhl1
    bhl2.curr<-bhl2
    wol1.curr<-wol1
    wol2.curr<-wol2
    bol.curr<-bol
    bestSSE<-SSE
    best.fitted<-fitted
    
    #    print(bestSSE)
  }
  
  
}

print(bestSSE)
plot(y,best.fitted);abline(c(0,1))


test.predicted<-NN2(c(whl11.curr,whl12.curr,whl21.curr,whl22.curr,bhl1.curr,bhl2.curr,wol1.curr,wol2.curr,
                                       bol.curr),x1test,x2test)
testSSE<-sum((test.predicted-ytest)^2)
print(testSSE)


#The best so far parameters are
whl11.curr
whl12.curr
whl21.curr
whl22.curr
bhl1.curr
bhl2.curr
wol1.curr
wol2.curr
bol.curr


#####
#exercise 3  classifier NN with K=3

NN3<-function(param,x1,x2)
{
  #  This function is the neural network
  #Input data is x1 and x2
  #weights are in the param vector 
  
  ## unpick the param vector
  whl11<-param[1]
  whl12<-param[2]
  whl21<-param[3]
  whl22<-param[4]
  bhl1<-param[5]
  bhl2<-param[6]
  wol11<-param[7]
  wol12<-param[8]
  wol21<-param[9]
  wol22<-param[10]
  wol31<-param[11]
  wol32<-param[12]
  bol1<-param[13]
  bol2<-param[14]
  bol3<-param[15]
  
  #  hidden layer
  z1<-??
  z2<-??
  # activation
  a11<-??
  a12<-??
  
  #output layer
  a21<-??
  a22<-??
  a23<-??
  
  #and produce the fitted probabilities
  pimat<-t(apply(cbind(a21,a22,a23),1,prop.table)) ##calculates the proportions for each row
  
  return(pimat)
}  


#initialise parameters
whl11.curr<-0.01
whl12.curr<-0.01
whl21.curr<-0.01
whl22.curr<-0.01
bhl1.curr<-2
bhl2.curr<--1
wol11.curr<-0.01
wol12.curr<-0.01
wol21.curr<-0.01
wol22.curr<-0.01
wol31.curr<-0.01
wol32.curr<-0.01
bol1.curr<-2
bol2.curr<-0
bol3.curr<- -2
n<-length(xcl1)


bestLoss<-Inf
fitted.probs<-matrix(NA,n,3)

niter<-15
window<-0.01 #a smaller window seems to be batter
for(iter in 1:niter){
  #We update parameter in turn and select the update if it gives a lower SSE   
  
  Delta<-rep(0,15)  #initialise the change vector
  # define which parameter to perturb
  j<- (iter %% 15) +1
  

  
  Delta[j]<-rnorm(1,0,window) #generate the change in parameter and assign it
  
  whl11<-whl11.curr+Delta[1]
  whl12<-whl12.curr+Delta[2]
  whl21<-whl21.curr+Delta[3]
  whl22<-whl22.curr+Delta[4]
  bhl1<-bhl1.curr+Delta[5]
  bhl2<-bhl2.curr+Delta[6]
  wol11<-wol11.curr+Delta[7]
  wol12<-wol12.curr+Delta[8]
  wol21<-wol21.curr+Delta[9]
  wol22<-wol22.curr+Delta[10]
  wol31<-wol31.curr+Delta[11]
  wol32<-wol32.curr+Delta[12]
  bol1<-bol1.curr+Delta[13]
  bol2<-bol2.curr+Delta[14]
  bol3<-bol3.curr+Delta[15]
  
  param<-c(whl11,whl12,whl21,whl22,bhl1,bhl2,  wol11,
           wol12,wol21,wol22,wol31,wol32,bol1,bol2,bol3)
  
  
  
  
  #call NN for each observation
  for(i in 1:n) fitted.probs[i,]<-NN3(param,xcl1[i],xcl2[i])
  
  #calculate the loss for each class in turn
  Loss<-rep(NA,n)
  Loss[ycl==1]<- -log(fitted.probs[ycl==1,1])
  Loss[ycl==2]<- -log(fitted.probs[ycl==2,2])
  Loss[ycl==3]<- -log(fitted.probs[ycl==3,3])
  
  sumLoss<-sum(Loss)
  
  #if sumLoss is better, update 
  if(sumLoss<bestLoss){
    whl11.curr<-whl11
    whl12.curr<-whl12
    whl21.curr<-whl21
    whl22.curr<-whl22
    bhl1.curr<-bhl1
    bhl2.curr<-bhl2
    wol11.curr<-wol11
    wol12.curr<-wol12
    wol21.curr<-wol21
    wol22.curr<-wol22
    wol31.curr<-wol31
    wol32.curr<-wol32
    bol1.curr<-bol1
    bol2.curr<-bol2
    bol3.curr<-bol3
    bestLoss<-sumLoss
    best.fitted.probs<-fitted.probs
    #    print(bestLoss)
    #   break
  }

}
print(bestLoss)

boxplot(best.fitted.probs[,ycl]~ycl)


pred.ycl<-apply(best.fitted.probs,1,which.max)

table(ycl,pred.ycl)

ntest<-length(xcl1test)
test.fitted.probs<-NN3(param,xcl1test,xcl2test)


Loss<-rep(NA,n)
Loss[ycltest==1]<- -log(test.fitted.probs[ycltest==1,1])
Loss[ycltest==2]<- -log(test.fitted.probs[ycltest==2,2])
Loss[ycltest==3]<- -log(test.fitted.probs[ycltest==3,3])
sum(Loss)

pred.ycltest<-apply(test.fitted.probs,1,which.max)
table(ycltest,pred.ycltest)



