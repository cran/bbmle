x <- runif(10)
y <- 1+x+rnorm(10,sd=0.1)

library(bbmle)
m1 = mle2(y~dnorm(a+b*x,sd=exp(s)),start=list(a=1,b=1,s=log(0.1)))

m2 = mle2(y~dnorm(a+b*x,sd=exp(s)),start=list(a=1,b=1,s=log(0.1)),
  method="L-BFGS-B",lower=c(a=0,b=0,s=-Inf))

m2F = mle2(y~dnorm(a+b*x,sd=exp(s)),start=list(a=1,b=1,s=log(0.1)),
  method="L-BFGS-B",lower=c(a=0,b=0,s=-Inf),
  fixed=list(a=1))

