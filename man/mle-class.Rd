\name{mle2-class}
\docType{class}
\alias{mle2-class}
\alias{coef,mle2-method}
\alias{confint,mle2-method}
\alias{show,mle2-method}
\alias{slice,mle2-method}
\alias{summary,mle2-method}
\alias{update,mle2-method}
\alias{vcov,mle2-method}
\title{Class "mle2". Result of Maximum Likelihood Estimation.}
\description{This class encapsulates results of a generic maximum
  likelihood procedure.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("mle2", \dots)}, but
  most often as the result of a call to \code{\link{mle2}}.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{(language) The call to \code{\link{mle2}}.}
    \item{\code{coef}:}{(numeric) Vector of estimated parameters.}
    \item{\code{fullcoef}:}{(numeric) Fixed and estimated parameters.}
    \item{\code{vcov}:}{(numeric matrix) Approximate variance-covariance
      matrix, based on the second derivative matrix at the MLE.}
    \item{\code{min}:}{(numeric) Minimum value of objective function =
      minimum negative log-likelihood.}
    \item{\code{details}:}{(list) Return value from \code{\link{optim}}.}
    \item{\code{minuslogl}:}{(function) The negative log-likelihood function.}
    \item{\code{method}:}{(character) The optimization method used.}
    \item{\code{formula}:}{(character) If a formula was specified, a
      character vector giving the formula and parameter specifications.}
}
}
\section{Methods}{
  \describe{
    \item{confint}{\code{signature(object = "mle2")}: Confidence
      intervals from likelihood profiles.}
    \item{show}{\code{signature(object = "mle2")}: Display object
      briefly.}
    \item{show}{\code{signature(object = "summary.mle2")}: Display object briefly.}
    \item{summary}{\code{signature(object = "mle2")}: Generate object summary.}
    \item{update}{\code{signature(object = "mle2")}:  Update fit.}
    \item{vcov}{\code{signature(object = "mle2")}: Extract
      variance-covariance matrix.}
    \item{plot}{\code{signature(object="profile.mle2,missing")}: Plot profile. }
  }
}
\keyword{classes}
