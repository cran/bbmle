\name{profile.mle2-class}
\docType{class}
\alias{profile.mle2-class}
\alias{confint,profile.mle2-method}
\alias{plot,profile.mle2-method}
\alias{plot,profile.mle2,missing-method}
\alias{show,profile.mle2-method}
\title{Class "profile.mle2"; Profiling information for "mle2" object}
\description{Likelihood profiles along each parameter of likelihood function}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("profile.mle2",
  ...)}, but most often by invoking \code{profile} on an "mle2" object.
}
\section{Slots}{
  \describe{
    \item{\code{profile}:}{Object of class \code{"list"}. List of
      profiles, one for each requested parameter. Each profile is a data
      frame with the first column called \code{z} being the signed square
      root of the deviance, and the others being the
      parameters with names prefixed by \code{par.vals.}}
    \item{\code{summary}:}{Object of class \code{"summary.mle2"}. Summary
      of object being profiled.}
  }
}
\section{Methods}{
  \describe{
    \item{confint}{\code{signature(object = "profile.mle2")}: Use profile
      to generate approximate confidence intervals for parameters.}
    \item{plot}{\code{signature(x = "profile.mle2", y = "missing")}: Plot
      profiles for each parameter.}
    \item{summary}{\code{signature(x = "profile.mle2")}: Plot
      profiles for each parameter.}
    \item{show}{\code{signature(object = "profile.mle2")}: Show object.}
  }
}
\seealso{
  \code{\link{mle2}}, \code{\link{mle2-class}}, \code{\link{summary.mle2-class}} 
}
\examples{
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
fit1 <- mle2(y~dpois(lambda=ymax/(1+x/xhalf)),start=list(ymax=1,xhalf=1),
   method="L-BFGS-B",lower=c(ymax=0.001,xhalf=0.001))
p1 <- profile(fit1)
plot(p1,main=c("first","second"),
     xlab=c(~y[max],~x[1/2]),ylab="Signed square root deviance")
}

\keyword{classes}
