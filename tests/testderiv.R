library(bbmle)
## source("../R/dists.R")
## source("../R/mle.R")

## an attempt to sketch out by hand
##  how one would derive an analytic
##  gradient function for a formula-specified
##  likelihood and use it ...
set.seed(1001)
x <- rbinom(50,size=10,prob=0.4)
mle2(x~dbinom(prob=p,size=10),start=list(p=0.3),data=data.frame(x))

f <- sbinom(prob=0.1,size=1)$formula
deriv(parse(text=f),"prob")
