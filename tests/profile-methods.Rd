\name{profile-methods}
\docType{methods}
\alias{profile-methods}
\alias{profile,mle2-method}
\alias{plot,profile.mle2-method}
\title{Likelihood profiles }
\description{
  Methods for creating and plotting likelihood profiles
}

\usage{\S4method{profile}{mle2}(fitted, which = 1:p, maxsteps = 100,
                    alpha = 0.01, zmax = sqrt(qchisq(1 - alpha/2, p)),
                    del = zmax/5, trace = FALSE, skiperrs=TRUE,
                    tol.newmin = 0.001, ...)
\arguments{
  \item{fitted}{A fitted maximum likelihood model of class
\dQuote{mle2}}
  \item{which}{a numeric or character vector describing which parameters
to profile (default is to profile all parameters)}
  \item{maxsteps}{maximum number of steps to take looking for an upper
value of the negative log-likelihood}
  \item{alpha}{maximum confidence level}
  \item{zmax}{maximum value of negative log-likelihood (default
  is the likelihood ratio test critical value corresponding to a
  2-tailed chi-squared test at level alpha)}
  \item{del}{step size for profiling}
  \item{trace}{(logical) produce tracing output?}
  \item{skiperrs}{(logical) ignore errors produced during profiling?}
  \item{tol.newmin}{tolerance for finding a new minimum below the
    minimum negative log-likelihood estimated in initial fit}
  \item{\dots}{not used}
}
\details{
   (Describe profiling process further.)
}
\keyword{methods}
