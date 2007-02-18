\name{ICtab}
\alias{ICtab}
\alias{AICtab}
\alias{BICtab}
\alias{AICctab}
\alias{print.ICtab}
\title{Compute table of information criteria and auxiliary info}
\description{
  Computes information criteria for a series of models, optionally
  giving information about weights, differences between ICs, etc.
}
\usage{
ICtab(\dots, type=c("AIC","BIC","AICc"),
    weights = FALSE, delta = FALSE, sort = FALSE,
nobs, dispersion = 1, mnames, k = 2)
AICtab(\dots)
BICtab(\dots)
AICctab(\dots)
\method{print}{ICtab}(x,\dots)
}
\arguments{
  \item{\dots}{a list of (logLik or?) mle objects; in the case of
    \code{AICtab} etc., could also include other arguments to \code{ICtab}}
  \item{type}{specify information criterion to use}
  \item{weights}{(logical) compute IC weights?}
  \item{delta}{(logical) compute differences among ICs?}
  \item{sort}{(logical) sort ICs in increasing order?}
  \item{nobs}{(logical) number of observations: required for
    \code{type="BIC"} or \code{type="AICc"} unless objects have
    an \code{"nobs"} attribute}
  \item{dispersion}{(stub) overdispersion estimate, for computing qAIC}
  \item{mnames}{names for table rows: defaults to names of objects passed}
  \item{k}{penalty term (largely unused)}
  \item{x}{an ICtab object}
}
\value{
  A data frame containing:
  \item{IC}{information criterion}
  \item{df}{degrees of freedom/number of parameters}
  \item{dIC}{difference in IC from minimum-IC model}
  \item{weights}{exp(-dIC/2)/sum(exp(-dIC/2))}
}
\note{The print method uses sensible defaults; all ICs are rounded
  to the nearest 0.1, and IC weights are printed using
  \code{\link{format.pval}} to print an inequality for
  values <0.001}
\references{Burnham and Anderson 2002}
\author{Ben Bolker}
\keyword{misc}
