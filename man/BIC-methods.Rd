\name{BIC-methods}
\docType{methods}
\alias{BIC}
\alias{BIC-methods}
\alias{AIC-methods}
\alias{AICc-methods}
\alias{logLik-methods}
\alias{AICc}
\alias{AIC,mle2-method}
\alias{AICc,mle2-method}
\alias{AICc,logLik-method}
\alias{AICc,ANY-method}
\alias{AICc,ANY,mle2,logLik-method}
\alias{BIC,logLik-method}
\alias{BIC,ANY-method}
\alias{BIC,mle2-method}
\alias{BIC,ANY,mle2,logLik-method}
\alias{logLik,mle2-method}
\alias{anova,mle2-method}
\title{Log likelihoods and model selection for mle2 objects}
\description{
  Various functions for likelihood-based and information-theoretic
  model selection of likelihood models
}
\section{Methods}{
\describe{
    \item{logLik}{\code{signature(object = "mle2")}: Extract maximized
      log-likelihood.}
    \item{AIC}{\code{signature(object = "mle2")}: Calculate
      Akaike Information Criterion}
    \item{AICc}{\code{signature(object = "mle2")}: Calculate
      small-sample corrected Akaike Information Criterion}
    \item{BIC}{\code{signature(object = "mle2")}: Calculate
      Bayesian (Schwarz) Information Criterion}
    \item{BIC}{\code{signature(object = "logLik")}: Calculate
      Bayesian (Schwarz) Information Criterion}
    \item{BIC}{\code{signature(object = "ANY")}: Calculate
      Bayesian (Schwarz) Information Criterion}
    \item{anova}{\code{signature(object="mle2")}: Likelihood Ratio Test
      comparision of different models}
  }
}
\usage{
\S4method{BIC}{ANY,mle2,logLik}(object,...,nobs)
}
\arguments{
  \item{object}{A \code{logLik} or \code{mle2} object}
  \item{...}{An optional list of additional \code{logLik}
    or \code{mle2} objects (fitted to the same data set)}
  \item{nobs}{Number of observations}
}
\details{
  Further arguments to \code{BIC} can be specified
  in the \code{...} list: \code{delta} (logical)
  specifies whether to include a column for delta-BIC
  in the output.
}
\value{
  A table of the BIC values, degrees of freedom,
  and possibly delta-BIC values relative to the
  minimum-BIC model
}
\note{This is implemented in an ugly way and could
  probably be improved!}
\keyword{methods}
