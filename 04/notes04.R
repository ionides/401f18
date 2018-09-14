## ----setup,echo=F,results=F,cache=F--------------------------------------
library(broman) # used for myround 

## ----hist100,echo=T------------------------------------------------------
hist(replicate(sample(1:6,size=1),n=100),
  main="", prob=TRUE, breaks=0.5:6.5, xlab="n=100")

## ------------------------------------------------------------------------
plot(1:10, 
  main="", prob=TRUE, 
  breaks=0.5:6.5, xlab="n=100")

## ----hist100eval,out.width="60mm",fig.width=4,fig.height=3,echo=F--------
hist(replicate(sample(1:6,size=1),n=100),
  main="", prob=TRUE, breaks=0.5:6.5, xlab="n=100")

## ----hist10000eval,out.width="60mm",fig.width=4,fig.height=3,echo=F------
hist(replicate(sample(1:6,size=1),n=10000),
  main="", prob=TRUE, breaks=0.5:6.5, xlab="n=10000")

