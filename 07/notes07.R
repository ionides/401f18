## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 

## ------------------------------------------------------------------------
mice <- read.csv(
"https://ionides.github.io/401f18/hw/hw01/femaleMiceWeights.csv"
)
head(mice,3)
lm1 <- lm(Bodyweight~Diet,data=mice)
summary(lm1)$coef

## ------------------------------------------------------------------------
model.matrix(lm1)[c(1:2,12:13,23:24),]

## ----echo=F--------------------------------------------------------------
head(model.matrix(lm1),3)

## ------------------------------------------------------------------------
summary(lm1)$coef

## ------------------------------------------------------------------------
t.test(mice$Bodyweight[1:12],mice$Bodyweight[13:24],
  var.equal=TRUE)

## ----eval=F--------------------------------------------------------------
## download.file(destfile="FieldGoals.csv",
##   url="https://ionides.github.io/401f18/07/FieldGoals.csv")

## ------------------------------------------------------------------------
goals <- read.table("FieldGoals.csv",header=TRUE,sep=",")
head(goals[,1:8]) 

## ------------------------------------------------------------------------
goals[1,1:8]

## ------------------------------------------------------------------------
goals.lm <- lm(FGt~FGt1+Name,data=goals)
X <- model.matrix(goals.lm)

## ------------------------------------------------------------------------
class(goals$Name)
attributes(goals$Name)$levels[1:6]

## ------------------------------------------------------------------------
dim(X)

## ------------------------------------------------------------------------
unname(X[1:15,1:10])

## ------------------------------------------------------------------------
coef(goals.lm)[1:6]

## ----out.width="55mm",fig.width=3.3,fig.height=4,echo=F------------------
plot(FGt~FGt1,data=goals)
intercept <- coef(goals.lm)[1]
slope <- coef(goals.lm)[2]
kicker <- coef(goals.lm)[3:20]
abline(a=intercept,b=slope)
for(i in 1:18) abline(a=intercept+kicker[i],b=slope)

## ----anova---------------------------------------------------------------
anova(goals.lm)

