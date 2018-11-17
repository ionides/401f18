## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 
par(mai=c(0.8,0.8,0.1,0.1))

## ----reconstruct_variables,echo=F,eval=F---------------------------------
## U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
## U_annual <- apply(U[,2:13],1,mean)
## U_detrended <- lm(U_annual~U$Year)$residuals
## L <- read.table(file="life_expectancy.txt",header=TRUE)
## L <- subset(L,L$Year %in% U$Year)
## L_fit <- lm(Total~Year,data=L)
## L_detrended <- L_fit$residuals

## ----lm1-----------------------------------------------------------------
U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
U_annual <- apply(U[,2:13],1,mean)
U_detrended <- lm(U_annual~U$Year)$residuals
L <- read.table(file="life_expectancy.txt",header=TRUE)
L <- subset(L,L$Year %in% U$Year)
L_fit <- lm(Total~Year,data=L)
L_detrended <- L_fit$residuals
lm1 <- lm(L_detrended~U_detrended)

## ----lag_lm--------------------------------------------------------------
n <- length(resid(lm1))
e <- resid(lm1)[2:n]
lag_e <- resid(lm1)[1:(n-1)] # NOTE WE NEED 1:(n-1) NOT 1:n-1
lm2 <- lm(e~lag_e-1)
head(model.matrix(lm2),3)
summary(lm2)$coef

## ----timeplot_code,eval=F,echo=T-----------------------------------------
## plot(U$Year,resid(lm1))

## ----timeplot_plot,fig.width=3,fig.height=3.5,out.width="2in",echo=F-----
plot(U$Year,resid(lm1))

## ----lagplot_code,eval=F,echo=T------------------------------------------
## plot(lag_e,e)

## ----lagplot_plot,fig.width=3,fig.height=3.5,out.width="2in",echo=F------
plot(lag_e,e)

