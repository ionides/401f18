## ----download life expectancy data file----------------------------------
download.file(destfile="life_expectancy.txt",
  url="https://ionides.github.io/401f18/01/life_expectancy.txt")

## ----read life expectancy into R-----------------------------------------
L <- read.table(file="life_expectancy.txt",header=TRUE)

## ----use head to check the data matrix-----------------------------------
head(L,3)

## ----alternative assignment operator,eval=FALSE--------------------------
## L = read.table(file="life_expectancy.txt",header=TRUE)

## ----indexing L----------------------------------------------------------
L[2,3]

## ----indexing rows of L--------------------------------------------------
L[1:3,]

## ----vector example------------------------------------------------------
v <- c(3,1,4,1,5,9)
v

## ----extracting an element of a vector-----------------------------------
v[3]

## ----using dim to find the dimension-------------------------------------
dim(L)

## ----extracting multiple rows--------------------------------------------
L[1:5,4]

## ----identifying columns by name-----------------------------------------
L[1:5,"Total"]

## ----if the name has no quotes R treats it as a variable-----------------
Total <- "Female"
L[1:5,Total]

## ----finding the dimension of L again------------------------------------
dim(L)

## ----a vector has no dim-------------------------------------------------
y <- L[,4]
dim(y)

## ----but it does have a length-------------------------------------------
length(y)

## ----adding vectors in R-------------------------------------------------
u <- c(3,1,4)
v <- c(1,5,9)
u+v

## ----multiplying vectors in R--------------------------------------------
u*v

## ----vector plus scalar in R---------------------------------------------
u <- c(3,1,4)
a <- 5
u+a

## ----vector times scalar-------------------------------------------------
u*a

## ------------------------------------------------------------------------
y <- L[,"Total"]

## ----gain in life expectancy---------------------------------------------
g <- y[2:length(y)] - y[1:(length(y)-1)] 

## ----comparing length of y and g-----------------------------------------
length(y)
length(g)

## ----adding NA to match lengths------------------------------------------
g <- c(NA,g) 
g[1:8]

## ----a logical vector----------------------------------------------------
g[1:6]
L_down <- g<0
L_down[1:6]

## ----indexing using a logical vector-------------------------------------
year <- L[,"Year"]
years_down <- year[L_down]
years_down[1:6]

## ----a character vector--------------------------------------------------
L_qualitative <- ifelse(g<0,"decreased","increased")
L_qualitative[1:6]

## ----checking the class of R objects-------------------------------------
class(g)
class(L_down)
class(L_qualitative)

## ------------------------------------------------------------------------
class(L)
L_matrix <- as.matrix(L)
class(L_matrix)

## ----using the rownames and colnames functions---------------------------
colnames(L)
rownames(L)[1:8]

## ----subsetting using a logical vector-----------------------------------
L[g<0,"Year"]

## ----using the matrix function-------------------------------------------
A <- matrix(1:6,nrow=2)
A

## ----recycling in the matrix function------------------------------------
matrix(c(1,2,3),nrow=2,ncol=3)

## ----matrix exercise,eval=F----------------------------------------------
## matrix(c(0,1),nrow=3,ncol=2,byrow=TRUE)

## ----using c to concatenate vectors--------------------------------------
u <- c(1,2) ; v <- c(3,4) ; c(u,v)

## ----using rbind and cbind-----------------------------------------------
B <- rbind(u,v) ; C <- cbind(u,v)
B
C

## ----download unemployment data file-------------------------------------
download.file(destfile="unemployment.csv",
  url="https://ionides.github.io/401f18/01/unemployment.csv")

## ----read unemployment data into R---------------------------------------
U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
U[1:2,]

## ----using the apply function to get averages over each row--------------
u <- apply(U[,2:13],1,mean)
u[1:6]

## ----checking the mnemonic-----------------------------------------------
dim(U)
length(apply(U,1,mean))
length(apply(U,2,mean))

## ----fig_L_eval,out.width="60mm",fig.width=4,fig.height=3,echo=F---------
plot(L$Year,y,type="line", 
  xlab="Year", 
  ylab="Life expectancy")

## ----fig_U_eval,out.width="60mm",fig.width=4,fig.height=3,echo=F---------
plot(U$Year,u,
  xlab="Year", 
  ylab="Unemployment")

## ----list example--------------------------------------------------------
my_list <- list(apples=c("red","green"),oranges=c(6,12))
class(my_list)
class(my_list$apples)
class(my_list$oranges)

## ----indexing a list component-------------------------------------------
my_list[[1]] 

## ----lm for a time trend of life expectancy------------------------------
L_fit <- lm(Total~Year,data=L)

## ----fig_L_code,eval=F,echo=T--------------------------------------------
## plot(Total~Year,data=L,
##   type="l",
##   ylab="Total life expectancy")
## lines(L$Year,L_fit$fitted.values,
##   lty="dotted")

## ------------------------------------------------------------------------
class(L_fit)

## ------------------------------------------------------------------------
names(L_fit)

## ----detrend the variables-----------------------------------------------
L_detrended <- L_fit$residuals
U_detrended <- lm(u~U$Year)$residuals
L_detrended <- subset(L_detrended,L$Year %in% U$Year)

## ----fit a linear model to the detrended variables-----------------------
lm1 <- lm(L_detrended~U_detrended)
coef(lm1)

