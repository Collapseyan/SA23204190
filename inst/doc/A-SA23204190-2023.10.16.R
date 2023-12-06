## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
set.seed(12345)
TPR.BH<-numeric(1000)
TPR.bonf<-numeric(1000)
FDR.BH<-numeric(1000)
FDR.bonf<-numeric(1000)
VBH<-0;SBH<-0;UBH<-0;TBH<-0
vbonf<-0;sbonf<-0;ubonf<-0;tbonf<-0
for (i in 1:1000) {
  x<-runif(950,0,1)
  y<-rbeta(50,0.1,1)
  p<-sort(c(x,y))
  plocation<-order(c(x,y))
  padj1<-p.adjust(p,method = "BH")
  padj2<-p.adjust(p,method = "bonferroni")
  for (j in 1:950) {
    if(padj1[plocation[j]]<0.1){VBH<-VBH+1}
    else{UBH<-UBH+1}
  }
  for (j in 951:1000) {
    if(padj1[plocation[j]]<0.1){SBH<-SBH+1}
    else{TBH<-TBH+1}
  }
   for (j in 1:950) {
    if(padj2[plocation[j]]<0.1){vbonf<-vbonf+1}
    else{ubonf<-ubonf+1}
  }
  for (j in 951:1000) {
    if(padj2[plocation[j]]<0.1){sbonf<-sbonf+1}
    else{tbonf<-tbonf+1}
  }
  TPR.BH[i]<-SBH/(SBH+TBH)
  TPR.bonf[i]<-sbonf/(sbonf+tbonf)
  FDR.BH[i]<-VBH/(VBH+SBH)
  FDR.bonf[i]<-vbonf/(vbonf+sbonf)
}

round(c(TPR.BH=mean(TPR.BH),TPR.bonf=mean(TPR.bonf),FDR.BH=mean(FDR.BH),FDR.bonf=mean(FDR.bonf)),4)


## -----------------------------------------------------------------------------
set.seed(12345)
m<-1000;B<-1000;n<-5;lambda<-2
x<-rexp(n,2)
hatlambda<-1/mean(x)
se.samp<-2*n/(n-1)*(n-2)^{-0.5}
lambdastar<-numeric(B)
bias<-numeric(m)
se.boot<-numeric(m)
for (i in 1:m) {
for (b in 1:B) {
  xstar<-sample(x,replace = TRUE)
  lambdastar[b]<-1/mean(xstar)

}
 bias[i]<-mean(lambdastar)-lambda
 se.boot[i]<-sd(lambdastar)
}

round(c(bias=mean(bias),se.boot=mean(se.boot),se.samp=se.samp),3)



## -----------------------------------------------------------------------------
mm<-1000;B<-1000;n<-10;lambda<-2
x<-rexp(n,2)
hatlambda<-1/mean(x)
se.samp<-2*n/(n-1)*(n-2)^{-0.5}
lambdastar<-numeric(B)
bias<-numeric(m)
se.boot<-numeric(m)
for (i in 1:m) {
for (b in 1:B) {
  xstar<-sample(x,replace = TRUE)
  lambdastar[b]<-1/mean(xstar)

}
 bias[i]<-mean(lambdastar)-lambda
 se.boot[i]<-sd(lambdastar)
}

round(c(bias=mean(bias),se.boot=mean(se.boot),se.samp=se.samp),3)

## -----------------------------------------------------------------------------
m<-1000;B<-1000;n<-20;lambda<-2
x<-rexp(n,2)
hatlambda<-1/mean(x)
se.samp<-2*n/(n-1)*(n-2)^{-0.5}
lambdastar<-numeric(B)
bias<-numeric(m)
se.boot<-numeric(m)
for (i in 1:m) {
for (b in 1:B) {
  xstar<-sample(x,replace = TRUE)
  lambdastar[b]<-1/mean(xstar)

}
 bias[i]<-mean(lambdastar)-lambda
 se.boot[i]<-sd(lambdastar)
}

round(c(bias=mean(bias),se.boot=mean(se.boot),se.samp=se.samp),3)

## -----------------------------------------------------------------------------
library(bootstrap)
set.seed(12345)
B<-1000
alpha<-0.05
se.theta<-0.115
n<-nrow(law)
theta<-cor(law[1],law[2])
thetastar<-numeric(B)
se.thetastar<-numeric(B)
boot<-numeric(B)
gpa<-numeric(B)
for (i in 1:B) {
  j<-sample(1:n,replace = TRUE)
  last<-law$LSAT[j]
  gpa<-law$GPA[j]
  thetastar[i]<-cor(gpa,last)
}

for (i in 1:B) {
  for (j in 1:B) {
    k<-sample(1:n,replace = TRUE)
    last<-law$LSAT[k]
    gpa<-law$GPA[k]
    boot[j]<-cor(last,gpa)
  }
  se.thetastar[i]<-sd(boot)
}
tstar<-unname(quantile((thetastar-theta)/se.thetastar,1-alpha/2))
d1<-theta-tstar*se.theta
d2<-theta+tstar*se.theta
paste("最终得到的置信区间为[",d1,",",d2,"]")


