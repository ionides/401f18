## ----setup,echo=F,results=F,cache=F--------------------------------------
library(broman) # used for myround 

## ----hist100,echo=T,eval=F,purl=TRUE-------------------------------------
## hist(replicate(n=100,sample(1:6,size=1) ),
##   main="",prob=TRUE,breaks=0.5:6.5,xlab="n=100",ylim=c(0,0.21))

## ----hist10000,eval=F,echo=F,purl=TRUE-----------------------------------
## hist(replicate(sample(1:6,size=1),n=10000),
##   main="", prob=TRUE, breaks=0.5:6.5, xlab="n=10000",ylim=c(0,0.21))

