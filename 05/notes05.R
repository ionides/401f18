## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 

## ----download life expectancy data file,eval=F---------------------------
## download.file(destfile="course_progress.txt",
##  url="https://ionides.github.io/401f18/05/course_progress.txt")

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

## ----echo=T,eval=T-------------------------------------------------------
var(x)

## ----echo=T,eval=T-------------------------------------------------------
cor(x)

## ----scores_pairs,eval=F,echo=T------------------------------------------
## pairs(x)

## ----out.width="95mm",fig.width=5.5,fig.height=5.5,eval=T,echo=F---------
pairs(x)

## ----mvn_pairs,eval=F,echo=T---------------------------------------------
## mvn <- rmvnorm(50,mean=apply(x,2,mean),sigma=var(x))
## pairs(mvn)

## ----out.width="90mm",fig.width=5.5,fig.height=5.5,eval=T,echo=F---------
set.seed(70)
mvn <- rmvnorm(50,mean=apply(x,2,mean),sigma=var(x))
pairs(mvn)

## ------------------------------------------------------------------------
weights <- c(final=0.4,quiz=0.2,hw=0.2,midterm=0.2)
overall <- as.matrix(x) %*% weights
var(overall)

## ------------------------------------------------------------------------
weights %*% var(x) %*% weights

## ----echo=F--------------------------------------------------------------
set.seed(368) 

## ----normal_uncorrelated,echo=T,eval=T-----------------------------------
x <- rnorm(20)
y <- rnorm(20)

## ------------------------------------------------------------------------
cor(x,y)

## ----plot_normal_uncorrelated,echo=T,eval=F------------------------------
## plot(x,y)

## ----quadratic_uncorrelated,eval=T,echo=T--------------------------------
x <- seq(-2,2,length=20)
y <- x^2

## ------------------------------------------------------------------------
cor(x,y)

## ----plot_quadratic_uncorrelated,eval=F,echo=T---------------------------
## plot(x,y)

## ----reconstruct_variables,echo=F----------------------------------------
L <- read.table(file="life_expectancy.txt",header=TRUE)
L_fit <- lm(Total~Year,data=L)
L_detrended <- L_fit$residuals
U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
U_annual <- apply(U[,2:13],1,mean)
U_detrended <- lm(U_annual~U$Year)$residuals
L_detrended <- subset(L_detrended,L$Year %in% U$Year)

## ----detrended_lm--------------------------------------------------------
lm1 <- lm(L_detrended~U_detrended) ; summary(lm1)

## ----summary-------------------------------------------------------------
names(summary(lm1))
summary(lm1)$coefficients

## ----model_matrix--------------------------------------------------------
X <- model.matrix(lm1)
head(X)

## ----se------------------------------------------------------------------
s <- sqrt(sum(resid(lm1)^2)/(nrow(X)-ncol(X))) ; s
V <- s^2 * solve(t(X)%*%X)
sqrt(diag(V))

## ------------------------------------------------------------------------
summary(lm1)$coefficients

## ----var_from_summary----------------------------------------------------
s <- summary(lm1)$sigma
XX <- summary(lm1)$cov.unscaled
s^2 * XX

## ----var_from_direct_calculation-----------------------------------------
X <- model.matrix(lm1)
sum(resid(lm1)^2)/(nrow(X)-ncol(X)) * solve(t(X)%*%X)

