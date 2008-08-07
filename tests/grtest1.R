## from Eric Weese
library(bbmle)
f <- function(x=2,a=1) x^2 - a
f.g <- function(x,a) 2*x
mle2(f,fixed=list(a=1)) #works
mle2(f,gr=f.g,fixed=list(a=1)) #fails 
