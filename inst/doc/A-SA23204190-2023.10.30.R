## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

n<-length(x);m<-length(y)#各自数据的长度
c1<-numeric(n);c2<-numeric(m)
F<-ecdf(x);G<-ecdf(y)
for (i in 1:n) {
  c1[i]<-(F(x[i])-G(x[i]))^2
}
for (j in 1:m) {
  c2[j]<-(F(y[j])-G(y[j]))^2
}
w2<-n*m/(m+n)^2*(sum(c1)+sum(c2))
w2

## -----------------------------------------------------------------------------
n1<-20;n2<-50
mu1<-mu2<-0
sigma1<-sigma2<-1
reps<-numeric(1000)

x<-rnorm(n1,mu1,sigma1);y<-rnorm(n2,mu2,sigma2)
z<-c(x,y)
tx<-sum(x>max(y))+sum(x<min(y))
ty<-sum(y>max(x))+sum(y<min(x))

for (i in 1000) {
  xy<-sample(z)
  x1<-xy[1:n1];y1<-xy[-(1:n1)]
  outx<-sum(x1>max(y1))+sum(x1<min(y1))
  outy<-sum(y1>max(x1))+sum(y1<min(x1))
  reps[i]<-as.integer(max(c(outx,outy))>max(c(tx,ty)))
}
p<-mean(reps)
paste("检验的p值为：",p)

