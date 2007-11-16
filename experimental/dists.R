sbinom <- function(prob,size) {
  list(title="Binomial",
       prob=prob,size=size,
       Mean=prob*size,
       Median=qbinom(0.5,prob,size),
       Mode=NA,
       Variance=size*prob*(1-prob),
       SD=sqrt(size*prob*(1-prob)))
}

abinom <- function() {
  list(params=c("prob","size"),
       fstr = "choose(size,x)*prob^x*(1-prob)^(size-x)",
       lfstr = "lgamma(size+1)-lgamma(x+1)-lgamma(size-x+1)+\
x*log(prob) + (size-x)*log(1-prob)")
}

apois <- function() {
  list(params=c("lambda"),
       fstr = "lambda^x*exp(-lambda)/factorial(x)",
       lfstr = "x*log(lambda)-lambda-lgamma(x+1)")
}

fsubs <- function(lf,pars,x,...) {
  upars = as.list(...)
  for (i in 1:length(pars)) {
    lf = gsub(pars[i],deparse(upars[[i]]),lf)
  }
  gsub("\\bx\\b",x,lf)
}

derivfun = function(formula,params) {
  f6 = formula[[3]] ## extract the RHS
  xvar = as.character(formula[[2]]) ## extract the LHS
  fn = do.call(gsub("^d","a",as.character(f6[[1]])),list()) ## call a-function
  deriv(parse(text=fsubs(fn$lfstr,fn$params,xvar,f6[-1])),
        params,function.arg=TRUE)
}

lf1 = abinom(a^2+b,s)$Formula.log
deriv(lf1,"a")
f2 = z~dbinom(1/(1+exp(-a)),size=N)
f3 = f2[[3]]
f3[[1]] = quote(abinom)
f4 = eval(f3)
deriv(f4$Formula.log,"a")

## for example: start with this formula (from ?mle)

f5 = y~dpois(lambda=exp(lymax)/(1+x/exp(lhalf)))
params = c("lymax","lhalf")
d1 = derivfun(f5,params)
y=c(2:7)
d1(1,1)



## arbitrary parameter ordering?
## still need to wrap function to take negative sum
## using gradient within optim stuff
##  (seems a waste to have a separate function ...)

## could use gradient attribute directly in nlm
## could use hessian=TRUE for nlm, nlminb ... (is it worth it?)
