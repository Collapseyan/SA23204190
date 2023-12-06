## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
set.seed(12345)
f1<-function(N,b1,b2,b3,f0){
  x1 <- rpois(N,1) 
  x2 <- rexp(N,1)
  x3 <- sample(0:1,N,replace=TRUE)
  g <- function(alpha){
  tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
  p <- 1/(1+tmp)
  mean(p)-f0
  }
  solution <- uniroot(g,c(-15,0))
  round(unlist(solution),5)[1:3]
}

N<-1e6;b1<-0;b2<-1;b3<--1;f0<-c(0.1,0.01,0.001,0.0001)
f1(N,b1,b2,b3,f0[1])
f1(N,b1,b2,b3,f0[2])
f1(N,b1,b2,b3,f0[3])
f1(N,b1,b2,b3,f0[4])


## -----------------------------------------------------------------------------
rw.Metropolis<-function(sigma,x0,N){
  
  x<-numeric(N)
  x[1]<-x0
  u<-runif(N)
  k<-0
  for (i in 2:N) {
    y<-rnorm(1,x[i-1],sigma)
    if(u[i]<=exp(-abs(y)+abs(x[i-1]))){
      x[i]<-y
      k<-k+1}
    else{
      x[i]<-x[i-1]
     
    }
  }
  return(list(x=x,k=k))
}

N=2000
sigma<-c(0.05,0.5,2,16)
x0<-10
rw1<-rw.Metropolis(sigma = sigma[1],x0,N)
rw2<-rw.Metropolis(sigma = sigma[2],x0,N)
rw3<-rw.Metropolis(sigma = sigma[3],x0,N)
rw4<-rw.Metropolis(sigma = sigma[4],x0,N)
print(c(rw1$k,rw2$k,rw3$k,rw4$k)/N)


## -----------------------------------------------------------------------------
N<-5000
burn<-1000
X<-matrix(0,N,2)
rho<-0.9
mu1<-mu2<-0
sigma1<-sigma2<-1
s1<-sqrt(1-rho^2)*sigma1
s2<-sqrt(1-rho^2)*sigma2

X[1,]<-c(mu1,mu2)
for(i in 2:N){
x2<-X[i-1,2]
m1<-+rho*(x2 - mu2)*sigma1/sigma2
X[i,1]<-rnorm(1,m1,s1)
x1<-X[i,1]
m2<-mu2+rho*(x1-mu1)*sigma2/sigma1
X[i,2]<-rnorm(1,m2,s2)
}
b<-burn+1
x<-X[b:N,]
plot(x)
y<-lm(x[,2]~x[,1])
summary(y)

## -----------------------------------------------------------------------------
Gelman.Rubin<-function(psi){
psi<-as.matrix(psi)
n<-ncol(psi)
k<-nrow(psi)

psi.means<-rowMeans(psi)
B<-n*var(psi.means)
psi.w<-apply(psi,1,"var")
W<-mean(psi.w)
v.hat<-W*(n-1)/n+(B/n)
r.hat<-v.hat/W
return(r.hat)
}

Rayleigh.chain<-function(sigma,N){
x<-numeric(N)
x[1]<-1
u<-runif(N)

for(i in 2:N){
xt<-x[i-1]
y<-rnorm(1,xt,sigma)
if(u[i]<=y/xt*exp((-y^2+xt^2)/2*sigma^2))
  x[i]<-y
  else{
  x[i]<-x[i-1]
  }
}
return(x)
}

sigma<-0.2
k<-4
n<-15000
b<-1000


X<-matrix(0,nrow=k,ncol=n)
for (i in 1:k) {
X[i,]<-Rayleigh.chain(sigma,n)
}
psi<-t(apply(X,1,cumsum))
for(i in 1:nrow(psi))
  psi[i,]<-psi[i,]/(1:ncol(psi))
print(Gelman.Rubin(psi))

rhat<-numeric(n)
for(j in (b+1):n){
rhat[j]<-Gelman.Rubin(psi[,1:j])
}

plot(rhat[(b+1):n])
abline(h=1.1,lty=2)


