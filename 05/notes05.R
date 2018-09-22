## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 

## ----reconstruct_variables,echo=F,eval=F---------------------------------
## L <- read.table(file="life_expectancy.txt",header=TRUE)
## L_fit <- lm(Total~Year,data=L)
## L_detrended <- L_fit$residuals
## U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
## U_annual <- apply(U[,2:13],1,mean)
## U_detrended <- lm(U_annual~U$Year)$residuals
## L_detrended <- subset(L_detrended,L$Year %in% U$Year)

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
summary(lm1)$coefficients

