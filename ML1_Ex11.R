# Machine Learning 1
# Workshop 11
# Classification: Linear and quadratic discriminant analysis

setwd("~/git-reps/machine-learning")

# Exercise 2

#computes the posterior distribution of Y given x
posterior<-function(x, pi0=0.5, mu0=4, mu1=5, sigma=1)
{
  px0 = dnorm(x,mean=mu0,sd=sigma) 
  px1 = dnorm(x,mean=mu1,sd=sigma)
  
  (px0*pi0)/(px0*pi0+px1*(1-pi0))
}

curve(posterior(x),2,7)
curve(posterior(x,pi=0.25),2,7)
curve(posterior(x,pi=0.75),2,7)

posterior(4.5)
posterior(4.5,pi0=0.25)
posterior(4.5,pi0=0.75)
