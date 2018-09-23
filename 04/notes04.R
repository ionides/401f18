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

## ----dice_rolls----------------------------------------------------------
dice2 <- replicate(50000,sum(sample(1:6,2,replace=TRUE)))
dice3 <- replicate(50000,sum(sample(1:6,3,replace=TRUE)))
dice10 <- replicate(50000,sum(sample(1:6,10,replace=TRUE)))
dice20 <- replicate(50000,sum(sample(1:6,20,replace=TRUE))) 

## ----two_dice,echo=T,eval=F,purl=T---------------------------------------
## hist(dice2,prob=TRUE,breaks=(min(dice2)-0.5):(max(dice2)+0.5))
## normal.x <- seq(from=min(dice2),to=max(dice2),length=100)
## normal.y <- dnorm(normal.x,mean=mean(dice2),sd=sd(dice2))
## lines(normal.x,normal.y,col="blue")

## ----echo=F,purl=T-------------------------------------------------------
dice_hist <- function(dat,...){
  hist(dat,probability=TRUE,breaks=seq(min(dat)-0.5,max(dat)+0.5,by=1),...)
  normal.x <- seq(from=min(dat),to=max(dat),length=100)
  normal.y <- dnorm(normal.x,mean=mean(dat),sd=sd(dat)) 
  lines(normal.x,normal.y,col="blue")
}

## ----echo=F--------------------------------------------------------------
set.seed(1)

## ------------------------------------------------------------------------
x <- rnorm(10000,mean=20,sd=5)
mean(x)

## ----echo=F--------------------------------------------------------------
set.seed(1)

## ------------------------------------------------------------------------
y <- replicate(n=10000,rnorm(1,mean=20,sd=5))
mean(y)

## ----echo=F--------------------------------------------------------------
set.seed(1)

## ------------------------------------------------------------------------
x <- rnorm(10000,mean=20,sd=5)
sd(x)

## ----standard_normal_pdf,out.width="80mm",fig.width=5,fig.height=3.5,echo=F,purl=T----
# Create data for the area to shade
pnorm_plot2 <- function(q_lo,q_hi,mean,sd,...) {
  coord.x <- seq(from=mean-3*sd,to=mean+3*sd,by=sd/100)
  plot(coord.x,dnorm(coord.x,mean,sd),type="l",...)
  shaded.coord.x <- coord.x[coord.x <= q_hi & coord.x >= q_lo]
  polygon.x <- c(min(shaded.coord.x),shaded.coord.x,max(shaded.coord.x))
  polygon.y <- c(0,dnorm(shaded.coord.x,mean,sd),0)  
  polygon(polygon.x,polygon.y,col='skyblue')
}
pnorm_plot2(q_lo=-2,q_hi=2,mean=0,sd=1,main="",xlab="",ylab="probability density")

## ------------------------------------------------------------------------
pnorm(2)-pnorm(-2)

