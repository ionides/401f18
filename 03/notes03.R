## ----setup,echo=F,results=F,cache=F--------------------------------------
library(broman) # used for myround 

## ----reconstruct_variables,echo=F----------------------------------------
L <- read.table(file="life_expectancy.txt",header=TRUE)
L_fit <- lm(Total~Year,data=L)
L_detrended <- L_fit$residuals
U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
U_annual <- apply(U[,2:13],1,mean)
U_detrended <- lm(U_annual~U$Year)$residuals
L_detrended <- subset(L_detrended,L$Year %in% U$Year)

## ----detrended_lm--------------------------------------------------------
lm1 <- lm(L_detrended~U_detrended)
coef(lm1)

## ----build_X-------------------------------------------------------------
X <- cbind(U_detrended,intercept=rep(1,length(U_detrended)))

solve( t(X) %*% X ) %*% t(X) %*% L_detrended

## ------------------------------------------------------------------------
head(X)

## ------------------------------------------------------------------------
length(U_detrended)
dim(X)

## ----echo=F--------------------------------------------------------------
lm1$fitted.values<- unname(lm1$fitted.values)

## ------------------------------------------------------------------------
my_fitted_values<-X %*% solve(t(X)%*%X) %*% t(X) %*% L_detrended 

## ------------------------------------------------------------------------
lm1$fitted.values[1:2]

## ------------------------------------------------------------------------
my_fitted_values[1:2]

## ----plot_fitted_code,echo=T,eval=F--------------------------------------
## plot(L_detrended~U_detrended)
## lines(U_detrended,my_fitted_values,lty="solid",col="blue")
## abline(coef(lm1),lty="dotted",col="red",lwd=2)

## ----plot_fitted_eval,echo=F,fig.width=4,fig.height=3.2,out.width="2.8in",cache=F----
par(mai=c(1.1,1,0.4,1))
plot(L_detrended~U_detrended)  
lines(U_detrended,my_fitted_values,lty="solid",col="blue")
abline(coef(lm1),lty="dotted",col="red",lwd=2)

