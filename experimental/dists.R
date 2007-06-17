sbinom <- function(prob,size) {
  list(title="Binomial",
       prob=prob,size=size,
       Mean=prob*size,
       Median=qbinom(0.5,prob,size),
       Mode=NA,
       Variance=size*prob*(1-prob),
       SD=sqrt(size*prob*(1-prob)))
}

abinom <- function(prob,size) {
  list(
       Formula=expression(choose(size,x)*prob^x*(1-prob)^(size-x)),
       Formula.log=expression(lgamma(size+1)-lgamma(x+1)-lgamma(size-x+1)+
           x*log(prob) + (size-x)*log(1-prob)))
}

