dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glmOT.D93 <- glm(counts ~ outcome + treatment, family=poisson)
library(MuMIn)
sessionInfo()
(gg <-  dredge(glmOT.D93,rank="QAIC", chat=dfun(glmOT.D93)))
(ggc <- dredge(glmOT.D93,rank="QAICc",chat=dfun(glmOT.D93), nobs=length(counts)))
