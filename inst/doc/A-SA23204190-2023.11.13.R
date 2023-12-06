## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
u<-c(11,8,27,13,16,17,0,23,10,24,2)
v<-c(12,9,28,14,17,18,1,24,11,25,3)

f<-function(lambda){
  sum((v*exp(-lambda*v)-u*exp(-lambda*u))/(exp(-lambda*u)-exp(-lambda*v)))
}

uniroot(f,interval = c(0.05,1))

## -----------------------------------------------------------------------------

library(boot)
A<-matrix(c(0,-2,-2,3,0,0,4,0,0,
            2,0,0,0,-3,-3,4,0,0,
            2,0,0,3,0,0,0,-4,-4,
            -3,0,-3,0,4,0,0,5,0,
            0,3,0,-4,0,-4,0,5,0,
            0,3,0,0,4,0,-5,0,-5,
            -4,-4,0,0,0,5,0,0,6,
            0,0,4,-5,-5,0,0,0,6,
            0,0,4,0,0,5,-6,-6,0),9,9)

  min.A<-min(A)
  A<-A-min.A
  max.A<-max(A)
  A<-A/max(A)
  m<-nrow(A)
  n<-ncol(A)
  it<-n^3
  a<-c(rep(0,m),1)
  A1<- -cbind(t(A),rep(-1,n))
  b1<-rep(0,n)
  A3<-t(as.matrix(c(rep(1,m),0)))
  b3<-1
  sx<-simplex(a=a,A1=A1,b1=b1,A3=A3,b3=b3,
              maxi=TRUE,n.iter=it)
  a<-c(rep(0,n),1)
  A1<-cbind(A,rep(-1,m))
  b1<-rep(0,m)
  A3<-t(as.matrix(c(rep(1,n),0)))
  b3<-1
  sy<-simplex(a=a,A1=A1,b1=b1,A3=A3,b3=b3,
              maxi=FALSE,n.iter=it)
  soln<-list("A" = A * max.A + min.A,
             "x" = sx$soln[1:m],
             "y" = sy$soln[1:n],
             "v" = sx$soln[m+1] * max.A + min.A)
  round(cbind(soln$x,soln$y),7)
  
  
  B<-A+2
  min.B<-min(B)
  B<-B-min.B
  max.B<-max(B)
  B<-B/max(B)
  a1<-c(rep(0,m),1)
  B1<- -cbind(t(B),rep(-1,n))
  b12<-rep(0,n)
  B3<-t(as.matrix(c(rep(1,m),0)))
  c3<-1
  sx<-simplex(a=a1,A1=B1,b1=b12,A3=B3,b3=c3,
              maxi=TRUE,n.iter=it)
  a1<-c(rep(0,n),1)
  B1<-cbind(B,rep(-1,m))
  b12<-rep(0,m)
  B3<-t(as.matrix(c(rep(1,n),0)))
  c3<-1
  sy<-simplex(a=a1,A1=B1,b1=b12,A3=B3,b3=c3,
              maxi=FALSE,n.iter=it)
  soln1<-list("B" = B * max.B + min.B,
             "x" = sx$soln[1:m],
             "y" = sy$soln[1:n],
             "v" = sx$soln[m+1] * max.B + min.B)
  round(cbind(soln1$x,soln1$y),7)
  

