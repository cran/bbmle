library(bbmle)
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
d <- data.frame(x,y)
LL <- function(ymax=15, xhalf=6)
    -sum(stats::dpois(y, lambda=ymax/(1+x/xhalf), log=TRUE))
mfit0 <- mle2(y~dpois(lambda=exp(interc)),
              start=list(interc=log(mean(y))),data=d)

mfit1 <- mle2(y~dpois(lambda=exp(loglambda)),
              start=list(loglambda=log(mean(y))),data=d)
              
gfit0 <- glm(y~1,family=poisson)
coef(mfit0)-coef(gfit0)
logLik(mfit0)-logLik(gfit0)
predict(gfit0,type="response")
predict(mfit0)  ## why only one value??
## FIXME: residuals are backwards
residuals(mfit0)
residuals(gfit0,type="response")

