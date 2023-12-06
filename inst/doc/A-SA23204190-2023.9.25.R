## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
set.seed(12345)
K<-1
pi1=rep(0,100)
pi2=rep(0,100)
pi3=rep(0,100)
while (K<=100) {
  l1 <- 1;l2<-1;l3<-1
  d1 <- 1;d2<-2;d3<-3
  m <- 1e6
  X1 <- runif(m,0,d1/2)
  Y <- runif(m,0,pi/2)
  pihat1 <- 2*l1/d1/mean(l1/2*sin(Y)>X1)
  X2 <- runif(m,0,d2/2)
  pihat2 <- 2*l2/d2/mean(l2/2*sin(Y)>X2)
  X3 <- runif(m,0,d3/2)
  pihat3 <- 2*l3/d3/mean(l3/2*sin(Y)>X3)
  pi1[K]<-pihat1
  pi2[K]<-pihat2
  pi3[K]<-pihat3
  K<-K+1
}
paste("\rho = 1时，方差为：",var(pi1),"\rho = 1/2时，方差为：",var(pi2),"\rho=1/3时，方差为：",var(pi3))


## ----echo=FALSE---------------------------------------------------------------
Percentage<-1-(10*exp(1)-3*exp(2)-5)/(4*exp(1)-3-exp(2))
paste("计算结果为：",Percentage)

## -----------------------------------------------------------------------------
m <- 10000
set.seed(12345)
U<-runif(m)
U1<-runif(m/2)
T1<-exp(U)
T2<-(exp(U1)+exp(1-U1))
perhat<-1-var(T2)/(2*var(T1))
paste("仿真结果为：",perhat)

