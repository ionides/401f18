## ----plot_margins,echo=F-------------------------------------------------
par(mai=c(1,0.5,0,0))

## ----read_life_expectancy------------------------------------------------
download.file(destfile="life_expectancy.txt",
  url="https://ionides.github.io/401f18/01/life_expectancy.txt")

## ----read_e0-------------------------------------------------------------
L <- read.table(file="life_expectancy.txt",header=TRUE)

## ----LE_head-------------------------------------------------------------
head(L,3)

## ----alt_read_e0,eval=FALSE----------------------------------------------
## L = read.table(file="life_expectancy.txt",header=TRUE)

## ----LE_indexing---------------------------------------------------------
L[2,3]

## ----LE_rows-------------------------------------------------------------
L[1:3,]

## ----vecExample----------------------------------------------------------
v <- c(3,1,4,1,5,9)
v

## ----extraction----------------------------------------------------------
v[3]

## ----dim-----------------------------------------------------------------
dim(L)

## ----vec-----------------------------------------------------------------
L[1:5,4]

## ----vec_with_names------------------------------------------------------
L[1:5,"Total"]

## ----vec_with_names_weird------------------------------------------------
Total <- "Female"
L[1:5,Total]

## ----length_or_dimA------------------------------------------------------
dim(L)

## ----length_or_dimB------------------------------------------------------
y <- L[,4]
dim(y)

## ----length_or_dimC------------------------------------------------------
length(y)

## ----vec_add-------------------------------------------------------------
u <- c(3,1,4)
v <- c(1,5,9)
u+v

## ----vec_mult------------------------------------------------------------
u*v

## ----vec_scalar_add------------------------------------------------------
u <- c(3,1,4)
a <- 5
u+a

## ----vec_scalar_mult-----------------------------------------------------
u*a

## ------------------------------------------------------------------------
y <- L[,"Total"]

## ----e0_gain-------------------------------------------------------------
g <- y[2:length(y)] - y[1:(length(y)-1)] 

## ------------------------------------------------------------------------
length(y)
length(g)

## ----e0_gain_with_na-----------------------------------------------------
g <- c(NA,g) 
g[1:8]

## ----logic_vec-----------------------------------------------------------
g[1:6]
L_down <- g<0
L_down[1:6]

## ------------------------------------------------------------------------
year <- L[,"Year"]
years_down <- year[L_down]
years_down[1:6]

## ----character_vec-------------------------------------------------------
L_qualitative <- ifelse(g<0,"decreased","increased")
L_qualitative[1:6]

## ------------------------------------------------------------------------
class(g)
class(L_down)
class(L_qualitative)

## ------------------------------------------------------------------------
class(L)
L_matrix <- as.matrix(L)
class(L_matrix)

## ------------------------------------------------------------------------
colnames(L)
rownames(L)[1:8]

## ----subsetting_using_logical--------------------------------------------
L[g<0,"Year"]

## ------------------------------------------------------------------------
A <- matrix(1:6,nrow=2)
A

## ------------------------------------------------------------------------
matrix(c(1,2,3),nrow=2,ncol=3)

## ----matrix_exercise,eval=F----------------------------------------------
## matrix(c(0,1),nrow=3,ncol=2,byrow=TRUE)

## ----echo=FALSE,eval=ANS-------------------------------------------------
matrix(c(0,1),nrow=3,ncol=2,byrow=TRUE)

## ------------------------------------------------------------------------
u <- c(1,2) ; v <- c(3,4) ; c(u,v)

## ------------------------------------------------------------------------
B <- rbind(u,v) ; C <- cbind(u,v)
B
C

## ----download_unemployment-----------------------------------------------
download.file(destfile="unemployment.csv",
  url="https://ionides.github.io/401f18/01/unemployment.csv")

## ----read_u--------------------------------------------------------------
U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
U[1:2,]

## ------------------------------------------------------------------------
u <- apply(U[,2:13],1,mean)
u[1:6]

## ------------------------------------------------------------------------
dim(U)
length(apply(U,1,mean))
length(apply(U,2,mean))

## ----fig_L,eval=F,echo=T-------------------------------------------------
## plot(L$Year,y,type="line",
##   xlab="Year",
##   ylab="Life expectancy")

## ----fig_L_eval,out.width="60mm",fig.width=4,fig.height=3,echo=F---------
plot(L$Year,y,type="line", 
  xlab="Year", 
  ylab="Life expectancy")

## ----fig_U,eval=F,echo=T-------------------------------------------------
## plot(U$Year,u,
##   xlab="Year",
##   ylab="Unemployment")

## ----fig_U_eval,out.width="60mm",fig.width=4,fig.height=3,echo=F---------
plot(U$Year,u,
  xlab="Year", 
  ylab="Unemployment")

## ------------------------------------------------------------------------
my_list <- list(apples=c("red","green"),oranges=c(6,12))
class(my_list)
class(my_list$apples)
class(my_list$oranges)

## ------------------------------------------------------------------------
my_list[[1]] 

## ----lm------------------------------------------------------------------
L_fit <- lm(Total~Year,data=L)

## ----fig_L_code,eval=F,echo=T--------------------------------------------
## plot(Total~Year,data=L,
##   type="l",
##   ylab="Total life expectancy")
## lines(L$Year,L_fit$fitted.values,
##   lty="dotted")

## ----fig_L_plot,echo=F,fig.width=3.5,fig.height=3.5----------------------
par(mai=c(0.9,0.9,0.1,0.1))
plot(Total~Year,data=L,
  type="l",
  ylab="Total life expectancy")
lines(L$Year,L_fit$fitted.values,
  lty="dotted")

## ------------------------------------------------------------------------
class(L_fit)

## ------------------------------------------------------------------------
names(L_fit)

## ----detrended_variables-------------------------------------------------
L_detrended <- L_fit$residuals
U_detrended <- lm(u~U$Year)$residuals
L_detrended <- subset(L_detrended,L$Year %in% U$Year)

## ----detrended_lm--------------------------------------------------------
lm1 <- lm(L_detrended~U_detrended)
coef(lm1)

