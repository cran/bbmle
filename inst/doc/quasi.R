
## @knitr opts,echo=FALSE
if (require("knitr")) opts_chunk$set(tidy=FALSE)


## @knitr dfun
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}


## @knitr dobdata
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)


## @knitr fitdob
glmOT.D93 <- glm(counts ~ outcome + treatment, family=poisson)
glmO.D93  <- update(glmOT.D93, . ~ . - treatment)
glmT.D93  <- update(glmOT.D93, . ~ . - outcome)
glmX.D93  <- update(glmT.D93, . ~ . - treatment)
glmQOT.D93 <- update(glmOT.D93, family=quasipoisson)
glmQO.D93 <- update(glmO.D93, family=quasipoisson)
glmQT.D93 <- update(glmT.D93, family=quasipoisson)
glmQX.D93 <- update(glmX.D93, family=quasipoisson)


## @knitr dobll
(sum(dpois(counts,
          lambda=exp(predict(glmOT.D93)),log=TRUE))) ## by hand
(logLik(glmOT.D93))  ## from Poisson fit


## @knitr dobll2
(-2*(logLik(glmT.D93)-logLik(glmOT.D93)))  ## Poisson fit
(deviance(glmT.D93)-deviance(glmOT.D93))   ## Poisson fit
(deviance(glmQT.D93)-deviance(glmQOT.D93)) ## quasi-fit


## @knitr dobdisp
(dfun(glmOT.D93))
(sum(residuals(glmOT.D93,"pearson")^2)/glmOT.D93$df.residual)
(summary(glmOT.D93)$dispersion)
(summary(glmQOT.D93)$dispersion)


## @knitr bbmle
library(bbmle)
(qAIC(glmOT.D93,dispersion=dfun(glmOT.D93)))
(qAICc(glmOT.D93,dispersion=dfun(glmOT.D93),nobs=length(counts)))
ICtab(glmOT.D93,glmT.D93,glmO.D93,glmX.D93,
      dispersion=dfun(glmOT.D93),type="qAIC")
ICtab(glmOT.D93,glmT.D93,glmO.D93,glmX.D93,
      dispersion=dfun(glmOT.D93),
      nobs=length(counts),type="qAICc")
detach("package:bbmle")


## @knitr AICcmodavg
library(AICcmodavg)
aictab(list(glmOT.D93,glmT.D93,glmO.D93,glmX.D93), 
       modnames=c("OT","T","O","X"),
       c.hat=dfun(glmOT.D93))
detach("package:AICcmodavg")


## @knitr MuMin
library(MuMIn)
(gg <-  dredge(glmOT.D93,rank="QAIC", chat=dfun(glmOT.D93)))
(ggc <- dredge(glmOT.D93,rank="QAICc",chat=dfun(glmOT.D93)))
detach("package:MuMIn")


## @knitr eval=FALSE
## (ggqc <- dredge(glmQOT.D93,rank="QAICc",
##                 chat=summary(glmQOT.D93)$dispersion))


