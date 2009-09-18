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
ICtab(\dots, type=c("AIC","BIC","AICc","qAIC","qAICc"),
    weights = FALSE, delta = TRUE, base = FALSE, sort = TRUE,
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
  \item{base}{(logical) include base IC values?}
  \item{weights}{(logical) compute IC weights?}
  \item{delta}{(logical) compute differences among ICs?}
  \item{sort}{(logical) sort ICs in increasing order?}
  \item{nobs}{(integer) number of observations: required for
    \code{type="BIC"} or \code{type="AICc"} unless objects have
    an \code{"nobs"} attribute}
  \item{dispersion}{overdispersion estimate, for computing qAIC:
    required for \code{type="qAIC"} or \code{type="qAICc"} unless
    objects have a \code{"dispersion"} attribute}
  \item{mnames}{names for table rows: defaults to names of objects passed}
  \item{k}{penalty term (largely unused: left at default of 2)}
  \item{x}{an ICtab object}
}
\value{
  A data frame containing:
  \item{IC}{information criterion}
  \item{df}{degrees of freedom/number of parameters}
  \item{dIC}{difference in IC from minimum-IC model}
  \item{weights}{exp(-dIC/2)/sum(exp(-dIC/2))}
}
\note{(1) The print method uses sensible defaults; all ICs are rounded
  to the nearest 0.1, and IC weights are printed using
  \code{\link{format.pval}} to print an inequality for
  values <0.001. (2) The computation of degrees of freedom/number of
  parameters (e.g., whether
  variance parameters are included in the total) varies enormously
  between packages.  As long as the df computations
  for a given set of models is consistent, differences
  don't matter, but one needs to be careful with log likelihoods
  and models taken from different packages.  If necessary
  one can change the degrees of freedom manually by
  saying \code{attr(obj,"df") <- df.new}, where \code{df.new}
  is the desired number of parameters.
  (3) Defaults have changed to \code{sort=TRUE}, \code{base=FALSE},
  \code{delta=TRUE}, to match my conviction that it rarely makes
  sense to report the overall values of information criteria}
\references{Burnham and Anderson 2002}
\author{Ben Bolker}
\keyword{misc}
