## ----setup,echo=F,results=F,cache=F--------------------------------------
library(broman) # used for myround 

## ----hist100,echo=T,eval=F,purl=TRUE-------------------------------------
## hist(replicate(n=100,sample(1:6,size=1) ),
##   main="",prob=TRUE,breaks=0.5:6.5,xlab="n=100",ylim=c(0,0.21))

## ----hist10000,eval=F,echo=F,purl=TRUE-----------------------------------
## hist(replicate(sample(1:6,size=1),n=10000),
##   main="", prob=TRUE, breaks=0.5:6.5, xlab="n=10000",ylim=c(0,0.21))

## ----rnorm_example,echo=T------------------------------------------------
rnorm(n=10,mean=20,sd=5)

## ----rnorm100,echo=T,eval=F,purl=TRUE------------------------------------
## hist(rnorm(n=100,mean=20,sd=5),
##   main="",xlab="n=100")

## ----rnorm10000,eval=F,echo=F,purl=TRUE----------------------------------
## hist(rnorm(n=10000,mean=20,sd=5),
##   main="",xlab="n=10000")

## ----normal_pdf,out.width="60mm",fig.width=3.5,fig.height=3,echo=F,purl=T----
# Create data for the area to shade
pnorm_plot <- function(q,mean,sd,...) {
  coord.x <- seq(from=mean-3*sd,to=mean+3*sd,by=sd/100)
  plot(coord.x,dnorm(coord.x,mean,sd),type="l",...)
  shaded.coord.x <- coord.x[coord.x <= q]
  polygon.x <- c(mean-3*sd,shaded.coord.x,q)
  polygon.y <- c(0,dnorm(shaded.coord.x,mean,sd),0)  
  polygon(polygon.x,polygon.y,col='skyblue')
}
pnorm_plot(25,20,5,main="",xlab="",ylab="probability density")

## ----runif,echo=T,eval=F,purl=TRUE---------------------------------------
## hist(runif(100))

## ----rexp,eval=F,echo=T,purl=TRUE----------------------------------------
## hist(rexp(100))

## ----eval=F--------------------------------------------------------------
## NA

