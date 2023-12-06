## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
m<-1e6;est<-sd<-numeric(2)
g<-function(x){x*x*(2*pi)^(-0.5)*exp(-0.5*x^2)}*(x>1)
f1<-function(x){exp(-(x-1))}
f2<-function(x){0.5*exp(-0.5*(x-1))}
x<-rexp(m,1)+1
est[1]<-mean(g(x)/f1(x))
sd[1]<-sd(g(x)/f1(x))
x<-rexp(m,0.5)+1
est[2]<-mean(g(x)/f2(x))
sd[2]<-sd(g(x)/f2(x))
res <- rbind(est=round(est,3), sd=round(sd,3))
  colnames(res) <- paste0('f',1:2)
  res
x<-seq(1,10,0.01)
w<-2
plot(x,g(x),type="l",ylim=c(0,1.5),lwd= w,col=1,main = '(A)')
lines(x,f1(x),lty=2,lwd=w,col=2)
lines(x,f2(x),lty=3,lwd=w,col=3)

## -----------------------------------------------------------------------------
m<-1e7
g<-function(x){x*x*(2*pi)^(-0.5)*exp(-0.5*x^2)}*(x>1)
f1<-function(x){(2*pi)^(-0.5)*exp(-0.5*x^2)}
x<-rnorm(m)
paste("采用重要性采样方法得到的估计值为：",mean(g(x)/f1(x)))

## -----------------------------------------------------------------------------
M <- 10000; k <- 10 
r <- M/k 
N <- 50 
T2 <- numeric(k)
est <- matrix(0, N, 2)
g<-function(x){x^(-4)*(2*pi)^(-0.5)*exp(-0.5*x^(-2))*(x<1)*(x>0)}
for (i in 1:N) {
     est[i, 1] <- mean(g(runif(M)))
     for(j in 1:k){T2[j]<-mean(g(runif(M/k,(j-1)/k,j/k)))}
     est[i, 2] <- mean(T2)
 }
round(apply(est,2,mean),4)

## -----------------------------------------------------------------------------
m<-1e5;k<-1;num<-0
set.seed(12345)
while (k<=m) {
  x<-rchisq(20,2)
  x1<-mean(x)-sd(x)*20^(-0.5)*qt(0.975,19)
  x2<-mean(x)+sd(x)*20^(-0.5)*qt(0.975,19)
  if(2<=x2){
    if(2>=x1){
      num<-num+1
    }
  }
  k<-k+1
}
prb=num/m
prb

## -----------------------------------------------------------------------------
alpha<-0.05
m<-1e5;num1<-num2<-num3<-0
for (i in 1:m) {
  x1<-rchisq(20,1)
  x2<-runif(20,0,2)
  x3<-rexp(20,1)
  T1<-20^(0.5)*(mean(x1)-1)/sd(x1)
  T2<-20^(0.5)*(mean(x2)-1)/sd(x2)
  T3<-20^(0.5)*(mean(x3)-1)/sd(x3)
  if(abs(T1)>qt(0.975,19)){
    num1<-num1+1
  }
  if(abs(T2)>qt(0.975,19)){
    num2<-num2+1
  }
  if(abs(T3)>qt(0.975,19)){
    num3<-num3+1
  }
}
c<-c(num1/m,num2/m,num3/m)
paste("(i)(ii)(iii)的结果分别为：",c[1],c[2],c[3])

