## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 
par(mai=c(0.8,0.8,0.1,0.1))

## ----gpa_data------------------------------------------------------------
gpa <- read.table("gpa.txt",header=T); head(gpa,3)

## ----lm_a----------------------------------------------------------------
lm1 <- lm(GPA~ACT+High_School*Year,data=gpa) 
coef(summary(lm1))[,1:2]

## ----lm_b----------------------------------------------------------------
head(model.matrix(lm1))

## ------------------------------------------------------------------------
lm2 <- lm(GPA~ACT+High_School+Year+High_School:Year,data=gpa)
head(model.matrix(lm2),4)

## ------------------------------------------------------------------------
lm3 <- lm(GPA~ACT*High_School,data=gpa)

## ------------------------------------------------------------------------
coef(summary(lm3))[,1:2]

## ------------------------------------------------------------------------
ACT_centered <- gpa$ACT-mean(gpa$ACT)
HS_centered <- gpa$Hi - mean(gpa$Hi)
lm3b <- lm(GPA~ACT_centered*HS_centered,data=gpa)
signif(coef(summary(lm3b))[,c(1,2,4)],3)

## ------------------------------------------------------------------------
s3 <- summary(lm3)$sigma
lm4 <- lm(GPA~ACT+High_School,data=gpa)
s4 <- summary(lm4)$sigma
lm5 <- lm(GPA~1,data=gpa)
s5 <- summary(lm5)$sigma
cat("s3 =",s3,"; s4 =",s4,"; s5 =",s5)

## ------------------------------------------------------------------------

## ----data----------------------------------------------------------------
goals <- read.table("FieldGoals2003to2006.csv",header=T,sep=",")
goals[1,c("Name","Teamt","FGt","FGtM1")]
lm6 <- lm(FGt~FGtM1*Name,data=goals)

## ------------------------------------------------------------------------
X<-model.matrix(lm6) ; colnames(X)<-1:38 ; X[1:17,c(1:8,21:26)]

## ------------------------------------------------------------------------
anova(lm6)

## ------------------------------------------------------------------------
X <- cbind(rep(1,6),rep(c(1,0),each=3),rep(c(0,1),each=3)) ; X

## ------------------------------------------------------------------------
mice <- read.table("femaleMiceWeights.csv",header=T,sep=",")
chow=rep(c(1,0),each=12)
hf=rep(c(0,1),each=12)
lm1 <- lm(Bodyweight~chow+hf,data=mice)
coef(summary(lm1))

## ------------------------------------------------------------------------
model.matrix(lm1)

## ------------------------------------------------------------------------
X <- model.matrix(lm1)
t(X)%*%X
det(t(X)%*%X)

## ------------------------------------------------------------------------
X <- model.matrix(lm1)
det(t(X)%*%X)
X2 <- X[,1:2]
det(t(X2)%*%X2)

## ----reconstruct_variables,echo=F----------------------------------------
L <- read.table(file="life_expectancy.txt",header=TRUE)
U <- read.table(file="unemployment.csv",sep=",",header=TRUE)

## ----fit_L_poly3---------------------------------------------------------
L_poly3 <- lm(Total~Year+I(Year^2)+I(Year^3),data=L)

## ----fig_L3,eval=F,echo=T------------------------------------------------
## plot(L$Year,L$Total,
##   type="line",
##   xlab="Year",
##   ylab="Life expectancy")
## 
## lines(L$Year,fitted(L_poly3),
##   lty="dashed")

## ----fig_L3_eval,fig.width=5,fig.height=4,out.width="2in",echo=F---------
par(mai=c(0.8,0.8,0.1,0.1))
plot(L$Year,L$Total,
  type="line",
  xlab="Year",
  ylab="Life expectancy")
  
lines(L$Year,fitted(L_poly3),
  lty="dashed")

## ----lag_lm--------------------------------------------------------------
L_detrended <- L_poly3$residuals
U_annual <- apply(U[,2:13],1,mean)
U_detrended <- lm(U_annual~Year+I(Year^2)+I(Year^3),
  data=U)$residuals
L_detrended <- subset(L_detrended,L$Year %in% U$Year)
lm_poly3 <- lm(L_detrended~U_detrended)
n <- length(resid(lm_poly3))
e <- resid(lm_poly3)[2:n] ; lag_e <- resid(lm_poly3)[1:(n-1)]

## ----timeplot_code,eval=F,echo=T-----------------------------------------
## plot(U$Year,resid(lm_poly3))

## ----timeplot_plot,fig.width=3,fig.height=3.5,out.width="2in",echo=F-----
plot(U$Year,resid(lm_poly3))

## ----lagplot_code,eval=F,echo=T------------------------------------------
## plot(lag_e,e)

## ----lagplot_plot,fig.width=3,fig.height=3.5,out.width="2in",echo=F------
plot(lag_e,e)

## ----fit_L_loess---------------------------------------------------------
L_loess <- loess(Total~Year,data=L,span=0.3)

## ----fig_L_loess,eval=F,echo=T-------------------------------------------
## plot(L$Year,L$Total,
##   type="line",
##   xlab="Year",
##   ylab="Life expectancy")
## 
## lines(L$Year,fitted(L_loess),
##   lty="dashed",col="red")

## ----fig_L_loess_eval,fig.width=5,fig.height=4,out.width="2in",echo=F----
par(mai=c(0.8,0.8,0.1,0.1))
plot(L$Year,L$Total,
  type="line",
  xlab="Year",
  ylab="Life expectancy")
  
lines(L$Year,fitted(L_loess),
  lty="dashed",col="red")

## ----lag_loess-----------------------------------------------------------
L_detrended <- resid(L_loess)
U_annual <- apply(U[,2:13],1,mean)
U_detrended <- resid(loess(U_annual~Year,data=U,span=0.3))
L_detrended <- subset(L_detrended,L$Year %in% U$Year)
lm_loess <- lm(L_detrended~U_detrended)
n <- length(resid(lm_loess))
e <- resid(lm_loess)[2:n] ; lag_e <- resid(lm_loess)[1:(n-1)]

## ----timeplot_code_loess,eval=F,echo=T-----------------------------------
## plot(U$Year,resid(lm_loess))

## ----timeplot_plot_loess,fig.width=3,fig.height=3.5,out.width="2in",echo=F----
plot(U$Year,resid(lm_loess))

## ----lagplot_code_loess,eval=F,echo=T------------------------------------
## plot(lag_e,e)

## ----lagplot_plot_loess,fig.width=3,fig.height=3.5,out.width="2in",echo=F----
plot(lag_e,e)

## ------------------------------------------------------------------------
coef(summary(lm_loess))

## ----gpa_summary,echo=F--------------------------------------------------
summary(lm(GPA~ACT+High_School,data=gpa))

