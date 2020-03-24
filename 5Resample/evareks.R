library(boot)
B=100
folds=c(2,4,5,10,20,25,50,100)
nfolds=length(folds)
# fixed x but random Y
n=100
x=seq(1,10,length=n)
beta0=1
beta1=2

resmat=matrix(ncol=nfolds,nrow=B)
set.seed(123)

for(b in 1:B)
{
  print(b)
  epsilon=rnorm(n,0,1)
  y=beta0+beta1*x+epsilon

  ds=data.frame(x,y)
# now we calculated the mean test MSE based on 2, 5, 10 and 100 fold CV.

  glm.fit=glm(y~x,data=ds)

  j=0
  for (i in c(2,4,5,10,20,25,50,100))
  { 
    j=j+1
    resmat[b,j]=cv.glm(ds,glm.fit,K=i)$delta[1]
  }
}

CVmean=apply(resmat,2,mean)
CVvar=apply(resmat,2,var)


#Xmat=cbind(rep(1,n),x)
#trueMSE=1^2*solve(t(Xmat)%*%Xmat)

#also random x, uniformly?

set.seed(123)

n=100
beta0=1
beta1=2

resmatR=matrix(ncol=nfolds,nrow=B)

for(b in 1:B)
{
  print(b)
  x=runif(n,1,10)
  
  epsilon=rnorm(n,0,1)
  y=beta0+beta1*x+epsilon
  
  ds=data.frame(x,y)
  # now we calculated the mean test MSE based on 2, 5, 10 and 100 fold CV.
  
  glm.fit=glm(y~x,data=ds)
  
  j=0
  for (i in c(2,4,5,10,20,25,50,100))
  { 
    j=j+1
    resmatR[b,j]=cv.glm(ds,glm.fit,K=i)$delta[1]
  }
}

CVmeanR=apply(resmatR,2,mean)
CVvarR=apply(resmatR,2,var)
