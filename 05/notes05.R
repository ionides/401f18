## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 

## ----download life expectancy data file,eval=F---------------------------
## download.file(destfile="course_progress.txt",
##  url="https://ionides.github.io/401f18/01/course_progress.txt")

## ----scores_plot,eval=F,purl=T-------------------------------------------
## x <- read.table("course_progress.txt")
## plot(final~midterm,data=x)

## ----mvn_plot,eval=F,echo=F,purl=T---------------------------------------
## library(mvtnorm)
## mvn <- rmvnorm(n=50,
##   mean=c(X=65,Y=65),
##   sigma=matrix(
##     c(200,100,100,150),
##     2,2)
## )
## plot(Y~X,data=mvn)

## ----eval=T--------------------------------------------------------------
var(x$midterm)
var(x$final)
cov(x$midterm,x$final)

## ----eval=T--------------------------------------------------------------
cor(x$midterm,x$final)

## ----mvn_cor_plot,eval=F,echo=F,purl=T,cache=F---------------------------
## library(mvtnorm)
## mvn <- rmvnorm(n=100,
##   mean=c(X=0,Y=0),
##   sigma=matrix(
##     c(1,rho,rho,1),
##     2,2)
## )

## ----eval=T,cache=F------------------------------------------------------
rho <- 0

## ----echo=T--------------------------------------------------------------
library(mvtnorm)
mvn <- rmvnorm(n=100,
  mean=c(X=0,Y=0),
  sigma=matrix(
    c(1,rho,rho,1),
    2,2)
)

## ----eval=T,cache=F------------------------------------------------------
rho <- -0.8

## ----eval=T--------------------------------------------------------------
rho <- 0.95

