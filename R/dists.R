sbinom <- function(size,prob) {
  list(title="Binomial",
       prob=prob,size=size,
       mean=prob*size,
       median=qbinom(0.5,size,prob),
       mode=NA,
       variance=size*prob*(1-prob),
       sd=sqrt(size*prob*(1-prob)))
}

snbinom <- function(size,prob,mu) {
    if (missing(mu) && !missing(prob)) {
        mupar <- FALSE
        mu = NA ## FIXME
        warning("STUB in snbinom: calc. mu as a function of prob")
    }
    if (!missing(mu) && missing(prob)) {
        mupar <- TRUE
        prob = size/(size+mu)
    }
    v <- if (mupar) mu+mu^2/size else size*(1-prob)/prob^2
    list(title="Negative binomial",
         prob=prob,mu=mu,size=size,
         mean=if (mupar) mu else size*(1-prob)/prob,
         median= if (mupar) qnbinom(0.5,mu=mu,size) else qnbinom(0.5,prob=prob,size),
         mode=NA,
         variance=v,
         sd=sqrt(v))
}

spois <- function(lambda) {
  list(title="Poisson",
       lambda=lambda,
       mean=lambda,
       median=qpois(0.5,lambda),
       mode=NA,
       variance=lambda,
       sd=sqrt(lambda))      
}
