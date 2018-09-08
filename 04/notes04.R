## ----setup,echo=F,results=F,cache=F--------------------------------------
library(broman) # used for myround 

## ----dice_sample,echo=T,eval=F-------------------------------------------
## ## Make 20 draws with replacement from {1,2,3,4,5,6}
## ## This models 20 realizations of rolling a fair die
## ## We can plot a histogram of the simulated dice rolls
## my_data <- sample(1:6,size=20,replace=TRUE)
## hist(my_data)

## ----fig.width=4,fig.height=2.5,out.width="2.9in",echo=F,eval=T----------
set.seed(23) 
par(mai=c(0.1,0.8,0.5,0.5))
## Make 20 draws with replacement from {1,2,3,4,5,6}
## This models 20 realizations of rolling a fair die
## We can plot a histogram of the simulated dice rolls
my_data <- sample(1:6,size=20,replace=TRUE)
hist(my_data)

