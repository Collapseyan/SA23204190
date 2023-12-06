## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=TRUE----------------------------------------------------------------
my.sample<-function(x,size,Prob=rep(1/length(x),length(x))){
  if(sum(Prob)!=1){
    stop("输入的概率之和必须为1！")
  }
  if(length(x)!=length(Prob)){
    stop("输入的x与Prob个数不对应！")
  }
  if(length(size)!=1){
    stop("输入的size必须是一个数字！")
  }
  if(!is.numeric(size)){
    stop("输入的size必须是数字型！")
  }
  n<-1:length(x)
  u<-runif(size)
  f<-cumsum(Prob)
  output<-x[findInterval(u,f)+1]
  return(output)
}


## ----echo=FALSE---------------------------------------------------------------
set.seed(12345)
x<-c(1,2,3,4);Prob<-c(0.1,0.1,0.2,0.6);n<-10000
result1<-my.sample(x,n,Prob = Prob)
print("结果中数字1的数目有：")
summary(result1==1)
print("结果中数字2的数目有：")
summary(result1==2)
print("结果中数字3的数目有：")
summary(result1==3)
print("结果中数字4的数目有：")
summary(result1==4)

## ----echo=FALSE---------------------------------------------------------------
x<-c(1,2,3,4);n<-10000
result2<-my.sample(x,n)
print("结果中数字1的数目有：")
summary(result2==1)
print("结果中数字2的数目有：")
summary(result2==2)
print("结果中数字3的数目有：")
summary(result2==3)
print("结果中数字4的数目有：")
summary(result2==4)

## ----echo=FALSE---------------------------------------------------------------
x<-c("a","b","c");n<-10000
result3<-my.sample(x,n)
print("结果中等于‘a’的数目有：")
summary(result3=="a")
print("结果中等于‘b’的数目有：")
summary(result3=="b")
print("结果中等于‘c’的数目有：")
summary(result3=="c")

## -----------------------------------------------------------------------------
u<-runif(1000)
x<-ifelse(u<0.5,log(2*u),-log(2-2*u))
hist(x,prob=TRUE,xlim=c(-10,10),main=expression(f(x)==0.5*exp(-abs(x))))
curve(0.5*exp(-abs(x)),col="green",add=TRUE)

## ----echo=TRUE----------------------------------------------------------------
my.beta<-function(n,x1,y1){
  #n为生成的随机数数目，x1是贝塔分布的第一个参数，x2是贝塔分布的第二个参数。
  y<-rep(0,n)#生成长度为n的初始向量y
  k<-0 #k为计数器，记录生成的随机数的数目
  c=(x1-1)^(x1-1) *(y1-1)^(y1-1) *(x1+y1-2)^(2-x1-y1) #根据两个参数算出合适的c值
  while (k<n) {
    u<-runif(1)
    x<-runif(1)
    parametor<- x^(x1-1) *(1-x)^(y1-1)
    if(parametor/c >u){
      k <- k+1
      y[k] <- x
    }
  }
  return(y) #返回最后结果
}
x<-my.beta(1000,2,3)
hist(x,prob=TRUE,main=expression(f(x)==12*x(1-x)^2))
curve(12*x*(1-x)^2,col="green",add=TRUE)

## -----------------------------------------------------------------------------
my.fe<-function(n){
  #n为生成的随机数数目
  y<-rep(0,n)#生成长度为n的初始向量y
  k<-0 #k为计数器，记录生成的随机数的数目
  while (k<n){
    u<-runif(1,min=-1,max = 1)
    x<-runif(1,min = -1,max = 1)
    parametor<- 1-x^2
    if(parametor >u){
      k <- k+1
      y[k] <- x
    }
  }
  return(y) #返回最后结果
}
  x<-my.fe(1000)
  hist(x,prob=TRUE,main=expression(f(x)==0.75(1-x^2)))
  curve(0.75*(1-x^2),col="green",add=TRUE)

