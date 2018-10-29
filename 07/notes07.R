## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 

## ------------------------------------------------------------------------
mice <- read.csv("https://ionides.github.io/401f18/hw/hw01/femaleMiceWeights.csv")
head(mice,3)
lm1 <- lm(Bodyweight~Diet,data=mice)
summary(lm1)$coef

## ------------------------------------------------------------------------
model.matrix(lm1)[c(1:2,12:13,23:24),]

## ----data----------------------------------------------------------------
goals <- read.table("FieldGoals2003to2006.csv",header=T,sep=",")
goals[1:5,c("Name","Teamt","FGt","FGtM1")]
lm0 <- lm(FGt~FGtM1+Name,data=goals)

## ----factor_class--------------------------------------------------------
class(goals$Name)

## ----design--------------------------------------------------------------
X <- model.matrix(lm0)
dim(X)
unname(X[c(1,5,9,13,17),1:8])

## ----anova---------------------------------------------------------------
anova(lm0)

