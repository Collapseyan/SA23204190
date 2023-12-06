## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
u<-list(1,2,3,4)
x<-unlist(u)
y<-as.vector(u)
x
y

## -----------------------------------------------------------------------------
test=matrix(data=NA,nrow = 0,ncol = 3)
test=as.data.frame(test)
colnames(test)=c("col1","col2","col3")
test

test2<-matrix(data = NA,nrow=3,ncol = 0)
test2<-as.data.frame(test2)
rownames(test2)=c("row1","row2","row3")
test2

## -----------------------------------------------------------------------------

scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
d <- data.frame(
     name=c("李明", "张聪", "王建"), 
     age=c(30, 35, 28), 
     height=c(180, 162, 175),
     stringsAsFactors=FALSE)

lapply(d[lapply(d,is.numeric)==TRUE],scale01)


## -----------------------------------------------------------------------------
#对于数据框
d1<-data.frame(1:5,1:5)
vapply(d1,sd,numeric(1))

#针对混合数据框

d <- data.frame(
     name=c("李明", "张聪", "王建"), 
     age=c(30, 35, 28), 
     height=c(180, 162, 175),
     stringsAsFactors=FALSE)
vapply(d[lapply(d,is.numeric)==TRUE],sd,numeric(1))



## -----------------------------------------------------------------------------
N<-5000
burn<-1000
X<-matrix(0,N,2)
a<-b<-3;n<-100#固定a=3,b=3,n=100


X[1,]<-c(1,0.5)
for (i in 2:N) {
  x2<-X[i-1,2]
  X[i,1]<-rbinom(1,n,x2)
  x1<-X[i,1]
  X[i,2]<-rbeta(1,x1+a,n-x1+b)
}

b<-burn+1
x<-X[b:N,]



plot(x,main = "",cex=.5,xlab = bquote(X[1]),
     ylab = bquote(X[2]),ylim = range(x[,2]))


