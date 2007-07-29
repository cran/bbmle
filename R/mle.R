require(methods)  ## for independence from stats4
## require(nlme) ## for fdHess() ## argh.  BIC conflict.

setClass("mle2", representation(call = "language",
                                coef = "numeric",
                                fullcoef = "numeric",
                                vcov = "matrix",
                                min = "numeric",
                                details = "list",
                                minuslogl = "function",
                                method = "character",
                                data="list",
                                formula="character",
                                optimizer="character"))

setClass("summary.mle2", representation(call = "language",
                               coef = "matrix",
                               m2logL = "numeric"))

setClass("profile.mle2", representation(profile="list",
                                       summary="summary.mle2"))


setClass("slice.mle2", representation(profile="list",
                                       summary="summary.mle2"))

setIs("profile.mle2", "slice.mle2")

calc_mle2_function <- function(formula,parameters,
                               start,data=NULL,trace=FALSE) {
  RHS <- formula[[3]]
  ddistn <- as.character(RHS[[1]])
  ## need to check on variable order:
  ## should it go according to function/formula,
  ##   not start?
  parnames <- as.list(names(start))
  names(parnames) <- names(start)
  ## hack
  call.to.char <- function(x) {
    x = as.list(x)
    if (length(x)>1) x <- x[c(2,1,3)]
    paste(sapply(x,as.character),collapse="")
  }
  if (!missing(parameters)) {
    vars <- as.character(sapply(parameters,"[[",2))
    if (length(parameters)>1) {
      models <-  sapply(parameters,function(z) call.to.char(z[[3]]))
       ## as.character(sapply(parameters,"[[",3))
    } else {
      models <- as.character(parameters)
    }
    parameters <- parameters[models!="1"]
    npars <- length(parameters)
    if (npars==0) { ## no non-constant parameters
      parameters <- mmats <- vpos <- NULL
    } else {
      mmats <- list()
      vpos <- list()
      for (i in seq(along=parameters)) {
        vname <- vars[i]
        p <- parameters[[i]]
        p[[2]] <- NULL
        mmat <- model.matrix(p,data=data)     
        pnames <- paste(vname,colnames(mmat),sep=".")
        parnames[[vname]] <- pnames ## insert into parameter names
        vpos0 <- which(names(start)==vname)
        vposvals <- cumsum(sapply(parnames,length))
        ## fill out start vectors with zeros or replicates as appropriate
        if (length(start[[vname]])==1) {
          if (length(grep("-1",models[i])>0)) {
            start[[vname]] <- rep(start[[vname]],length(pnames))
          } else {
            start[[vname]] <- c(start[[vname]],rep(0,length(pnames)-1))
          }
        }
        ## fix: what if parameters are already correctly specified?
        startpos <- if (vpos0==1) 1 else vposvals[vpos0-1]+1
        vpos[[vname]] <- startpos:vposvals[vpos0]
        mmats[[vname]] <- mmat
      }
    }
  } else parameters <- vars <- mmats <- vpos <- NULL
  parnames <- unlist(parnames)
  start <- as.list(unlist(start)) ## collapse/re-expand
  names(start) <- parnames
  arglist <- as.list(RHS[-1]) ## delete function name
  arglist$parameters <- NULL
  arglist1 <- c(list(x=formula[[2]]),arglist,list(log=TRUE))
  arglist1  ## codetools check kluge
  fn <- function() {
    ## is there a better way to do this?
    pars <- unlist(as.list(match.call())[-1])
    if (!is.null(parameters)) {
      ## browser()
      for (i in seq(along=parameters)) {
        assign(vars[i],mmats[[i]] %*% pars[vpos[[i]]])
      }
    }
    arglist1 <- lapply(arglist1,eval,envir=data,enclos=sys.frame(sys.nframe()))
    r <- -sum(do.call(ddistn,arglist1))
    ## doesn't work yet -- need to eval arglist in the right env ...
    ## if (debugfn) cat(unlist(arglist),r,"\n")
    ## browser()
    if (trace) cat(pars,r,"\n")
    r
  }
  npars <- length(parnames)
  flist <-  vector("list",npars)
  names(flist) <- parnames
  formals(fn) <- flist
  list(fn=fn,start=start,parameters=parameters,
       fdata=list(vars=vars,mmats=mmats,vpos=vpos,
         arglist1=arglist1,ddistn=ddistn,parameters=parameters),
       parnames=parnames)
}

## need logic that will identify correctly when
## we need to pass parameters as a vector
mle2 <- function(minuslogl,
                 start,  ## =formals(minuslogl),
                 method,
                 optimizer,
                 fixed=NULL,
                 data=NULL,
                 subset=NULL,
                 default.start=TRUE, 
                 eval.only = FALSE,
                 vecpar = FALSE,
                 parameters=NULL,
                 skip.hessian=FALSE,
                 trace=FALSE,
                 ...) {
  if (missing(method)) method <- mle2.options("optim.method")
  if (missing(optimizer)) optimizer <- mle2.options("optimizer")
  ## if (optimizer != "optim") stop("only optim() is currently supported")
  if (inherits(minuslogl,"formula")) {
    pf <- function(f) {if (is.null(f)) "" else paste(f[2],"~",
                                                     gsub(" ","",as.character(f[3])),sep="")}
    if (missing(parameters)) {
      formula <- pf(minuslogl)
    } else {
      formula <- paste(pf(minuslogl),paste(sapply(parameters,pf),collapse=", "),sep=": ")
    }
    tmp <- calc_mle2_function(minuslogl,parameters,
                              start,data,trace)
    minuslogl <- tmp$fn
    start <- tmp$start
    fdata <- tmp$fdata
    parameters <- tmp$parameters
  } else {
    formula <- ""
    fdata <- NULL

  }
  call <- match.call()
  if(!missing(start))
    if (!is.list(start)) {
      if (is.null(names(start)) || !is.vector(start))
        stop("'start' must be a named vector or named list")
      ## do we want this or not???
      vecpar <- call$vecpar <- TRUE  ## given a vector start: set vecpar=TRUE
      start <- as.list(start)
    }
  ## also check parnames(minuslogl)?
  if (missing(start) && default.start) start <- formals(minuslogl)
  if (!is.null(fixed) && !is.list(fixed)) {
    if (is.null(names(fixed)) || !is.vector(fixed))
      stop("'fixed' must be a named vector or named list")
    fixed <- as.list(fixed)
  }
  if (!is.null(data) && !is.list(data)) ##  && !is.environment(data)) 
    stop("'data' must be a list")
  nfix <- names(unlist(namedrop(fixed)))
  if (!is.null(parnames(minuslogl))) {
    nfull <- parnames(minuslogl)
    fullcoef <- vector("list",length(nfull))
    names(fullcoef) <- nfull
  } else {
    fullcoef <- formals(minuslogl)
    nfull <- names(fullcoef)
  }
  if(any(! nfix %in% nfull))
    stop("some named arguments in 'fixed' are not arguments to the specified log-likelihood function")
  if (length(nfix)>0) start[nfix] <- NULL
  fullcoef[nfix] <- fixed
  ## switched namedrop() from outside to inside sapply ?
  nstart <- names(unlist(sapply(namedrop(start),eval.parent)))
  fullcoef[! nfull %in% nfix & ! nfull %in% nstart ] <- NULL  ## delete unnecessary names
  nfull <- names(fullcoef)
  template <- lapply(start, eval.parent)  ## preserve list structure!
  if (vecpar) template <- unlist(template)
  start <- sapply(namedrop(start), eval.parent) # expressions are allowed; added namedrop
  nstart <- names(unlist(namedrop(start)))
  ## named <- length(names(fullcoef))
  oo <- match(nstart, names(fullcoef))
  if (any(is.na(oo)))
    stop("some named arguments in 'start' are not arguments to the specified log-likelihood function")
  ## if (named)
  start <- start[order(oo)]
  ## attach(data,warn.conflicts=FALSE)
  ## on.exit(detach(data))
  denv <- local(environment(),c(as.list(data),fdata,list(mleenvset=TRUE)))
  argnames.in.data <- names(data)[names(data) %in% names(formals(minuslogl))]
  args.in.data <- lapply(argnames.in.data,get,env=denv)
  names(args.in.data) <- argnames.in.data
  args.in.data  ## codetools kluge
  objectivefunction <- function(p){
    l <- relist(p,template) ## redo list structure
    ## if (named)
    names(p) <- nstart[order(oo)] ## make sure to reorder
    l[nfix] <- fixed
    ##    cat("p\n"); print(p)
    ## cat("l\n"); print(l)
    ##    cat("data\n"); print(data)
    if (vecpar) {
      ## if (named)
      l <- namedrop(l[nfull])
      l <- unlist(l)
      args <- list(l)
      args <- c(list(l),args.in.data)
    } else { args <- c(l,args.in.data)
           }
    ## eval in environment of minuslogl???
    ## doesn't help, environment(minuslogl) is empty by this time
    ## cat("e3:",length(ls(envir=environment(minuslogl))),"\n")
    do.call("minuslogl",args)
  } ## end of objective function
  ## only want to do this if environment has not been previously
  ##   set!
  if (!("mleenvset" %in% ls(envir=environment(minuslogl))))
    environment(minuslogl) <- denv
  if (length(start)==0 || eval.only) {
    if (length(start)==0) start <- numeric(0)
    optimizer <- "none"
    skip.hessian <- TRUE
    oout <- list(par=start, value=objectivefunction(start),
                 hessian = matrix(NA,nrow=length(start),ncol=length(start)))
    ## browser()

  } else {
    oout <- switch(optimizer,
                   optim = optim(par=start,
                     fn=objectivefunction, method=method, hessian=!skip.hessian, ...),
                   nlm = nlm(f=objectivefunction, hessian=!skip.hessian, ...),
                   nlminb = nlminb(start=start,
                     objective=objectivefunction, hessian=NULL, ...),
                   constrOptim = constrOptim(theta=start,
                     f=objectivefunction, method=method, ...),
                   stop("unknown optimizer (choices are 'optim', 'nlm', 'nlminb', and 'constrOptim')")
                 )
  }
  optimval <- switch(optimizer,
                     optim= , constrOptim=, none="value",
                     nlm="minimum",
                     nlminb="objective")
  if (optimizer=="nlm") oout$par <- oout$estimate
  if (optimizer=="nlminb") {
    names(oout$par) <- names(start)
  }
  ## HACK: constrOptim doesn't handle optim() arguments very well
  ## so use default hessian=FALSE and compute them later
  ## n.b. this is not using parscale information
  if (optimizer %in% c("nlminb","constrOptim") && !skip.hessian) {
    oout$hessian <- nlme::fdHess(oout$par,objectivefunction)$Hessian
    oout$hessian[lower.tri(oout$hessian)] <- t(oout$hessian)[lower.tri(oout$hessian)]
    ## print(oout$hessian)
  }
  ##  } else {
  ## oout <- optim(start, objectivefunction, method=method, hessian=!skip.hessian, ...)
  if (skip.hessian) {
    oout$hessian = matrix(NA,nrow=length(start),ncol=length(start))
  }
  ## skip hessian calculation if 0 varying parameters
  ##if (length(oout$par)) oout$hessian <- fdHess(pars=oout$par,fun=f)$Hessian
  coef <- oout$par
  nc <- names(coef)
  if (skip.hessian) {
    vcov <- matrix(NA,length(coef),length(coef))
  } else {
    if (length(coef)) {
      tmphess <- try(solve(oout$hessian))
      if (class(tmphess)=="try-error") {
        vcov <- matrix(NA,length(coef),length(coef))
        warning("couldn't invert Hessian")
      } else vcov <- tmphess
    } else {
      vcov <- matrix(numeric(0),0,0)
    }
  }
  dimnames(vcov) <- list(nc,nc)
  min <-  oout[[optimval]]
  ##  if (named)
  fullcoef[nstart[order(oo)]] <- coef
  ## else fullcoef <- coef
  m = new("mle2", call=call, coef=coef, fullcoef=unlist(fullcoef), vcov=vcov,
      min=min, details=oout, minuslogl=minuslogl, method=method,
    optimizer=optimizer,
      data=as.list(data),formula=formula)
  attr(m,"df") = length(m@coef)
  if (!missing(data)) attr(m,"nobs") = length(data[[1]])
  ## to work with BIC as well
  m
}

## should this be object@fullcoef or object@coef??? or should
## it have an additional argument --- is that possible?
setMethod("coef", "mle2", function(object) object@fullcoef )
## fullcoef <- function(object) object@fullcoef  ## this should be a method
setMethod("coef", "summary.mle2", function(object) object@coef )

setMethod("show", "mle2", function(object){
    cat("\nCall:\n")
    print(object@call)
    cat("\nCoefficients:\n")
    print(coef(object))
    cat("\nLog-likelihood: ")
    cat(round(as.numeric(logLik(object)),2),"\n")
    if (object@details$convergence>0)
      cat("\nWarning: optimization did not converge (code ",
          object@details$convergence,")\n",sep="")
  })

setMethod("show", "summary.mle2", function(object){
    cat("Maximum likelihood estimation\n\nCall:\n")
    print(object@call)
    cat("\nCoefficients:\n")
    printCoefmat(coef(object))
    cat("\n-2 log L:", object@m2logL, "\n")
})

setMethod("show", "profile.mle2", function(object){
    cat("Likelihood profile:\n\n")
    print(object@profile)
  })

setMethod("summary", "mle2", function(object, waldtest=TRUE, ...){
    cmat <- cbind(Estimate = object@coef,
                  `Std. Error` = sqrt(diag(object@vcov)))
    zval <- cmat[,"Estimate"]/cmat[,"Std. Error"]
    pval <- 2*pnorm(-abs(zval))
    coefmat <- cbind(cmat,"z value"=zval,"Pr(z)"=pval)
    m2logL <- 2*object@min
    new("summary.mle2", call=object@call, coef=coefmat, m2logL= m2logL)
})

setMethod("profile", "mle2",
          function (fitted, which = 1:p, maxsteps = 100,
                    alpha = 0.01, zmax = sqrt(qchisq(1 - alpha/2, p)),
                    del = zmax/5, trace = FALSE, skiperrs=TRUE, ...) {
            ## fitted: mle2 object
            ## which: which parameters to profile
            ## maxsteps: steps to take looking for zmax
            ## alpha: max alpha level
            ## zmax: log-likelihood difference
            ## del: stepsize
            if (fitted@optimizer=="constrOptim")
              stop("profiling not yet working for constrOptim -- sorry")
            Pnames <- names(fitted@coef)
            p <- length(Pnames)
            if (is.character(which)) which <- match(which,Pnames)
            if (any(is.na(which)))
              stop("parameters not found in model coefficients")
            onestep <- function(step,bi) {
              if (missing(bi)) bi <- B0[i] + sgn * step * del * std.err[i]
              fix <- list(bi)
              names(fix) <- p.i
              if (is.null(call$fixed)) call$fixed <- fix
              else call$fixed <- c(eval(call$fixed),fix)
              if (skiperrs) {
                pfit <- try(eval.parent(call, 2), silent=TRUE)
              } else {
                pfit <- eval.parent(call, 2)
              }
              if(skiperrs && inherits(pfit, "try-error")) {
                warning(paste("Error encountered in profile:",pfit))
                return(NA)
              }
              else {
                zz <- 2*(pfit@min - fitted@min)
                ri <- pv0
                ri[, names(pfit@coef)] <- pfit@coef
                ri[, p.i] <- bi
                if (zz > -0.001)
                  zz <- max(zz, 0)
                else {
                  cat("Profiling has found a better solution, so original fit had not converged:\n")
                  cat("New minimum=",pfit@min,"\n")
                  cat("Returning new parameters ...\n")
                  return(pfit@fullcoef)
                  ## cat("Parameter values:\n")
                  ## print(pfit@fullcoef)
                  ## stop("try restarting fit from values above")
                }
                z <- sgn * sqrt(zz)
                pvi <<- rbind(pvi, ri)
                zi <<- c(zi, z)
              }
              if (trace) cat(bi, z, "\n")
              z
            } ## end onestep
            ## Profile the likelihood around its maximum
            ## Based on profile.glm in MASS
            summ <- summary(fitted)
            std.err <- summ@coef[, "Std. Error"]
            Pnames <- names(B0 <- fitted@coef)
            pv0 <- t(as.matrix(B0))
            p <- length(Pnames)
            prof <- vector("list", length = length(which))
            names(prof) <- Pnames[which]
            call <- fitted@call
            call$minuslogl <- fitted@minuslogl
            ndeps <- eval.parent(call$control$ndeps)
            parscale <- eval.parent(call$control$parscale)
            upper <- eval.parent(call$upper)
            lower <- eval.parent(call$lower)
            ## cat("upper\n")
            ## print(upper)
            for (i in which) {
              zi <- 0
              pvi <- pv0
              p.i <- Pnames[i]
              ## omit values from control vectors:
              ##   is this necessary/correct?
              ## FIXME: if lower, upper are set then the profile should
              ##    respect that!
              if (!is.null(ndeps)) call$control$ndeps <- ndeps[-i]
              if (!is.null(parscale)) call$control$parscale <- parscale[-i]
              if (!is.null(upper)) call$upper <- upper[-i]
              if (!is.null(lower)) call$lower <- lower[-i]
              for (sgn in c(-1, 1)) {
                if (trace)
                  cat("\nParameter:", p.i, c("down", "up")[(sgn + 1)/2 + 1], "\n")
                step <- 0
                z <- 0
                ## This logic was a bit frail in some cases with
                ## high parameter curvature. We should probably at least
                ## do something about cases where the mle2 call fails
                ## because the parameter gets stepped outside the domain.
                ## (We now have.)
                call$start <- as.list(B0)
                lastz <- 0
                lbound <- if (!is.null(lower)) lower[i] else -Inf
                ubound <- if (!is.null(upper)) upper[i] else Inf
                while ((step <- step + 1) < maxsteps && abs(z) < zmax) {
                  curval <- B0[i] + sgn * step * del * std.err[i]
                  if ((sgn==-1 & curval<lbound) ||
                      (sgn==1 && curval>ubound)) break
                  z <- onestep(step)
                  if(is.na(z)) break
                  lastz <- z
                }
                if(abs(lastz) < zmax) {
                  ## now let's try a bit harder if we came up short
                  for(dstep in c(0.2, 0.4, 0.6, 0.8, 0.9)) {
                    curval <- B0[i] + sgn * (step-1+dstep) * del * std.err[i]
                    if ((sgn==-1 & curval<lbound) ||
                      (sgn==1 && curval>ubound)) break
                    z <- onestep(step - 1 + dstep)
                    if(is.na(z) || abs(z) > zmax) break
                    lastz <- z
                  }
                  if ((abs(lastz) < zmax) &&
                      ((sgn==-1 && lbound>-Inf) || (sgn==1 && ubound<Inf))) {
                    ## bounded and didn't make it, try at boundary
                    if (sgn==-1 && B0[i]>lbound) onestep(bi=lbound)
                    if (sgn==1  && B0[i]<ubound) onestep(bi=ubound)
                  }
                } else if(length(zi) < 5) { # try smaller steps
                  mxstep <- step - 1
                  step <- 0.5
                  while ((step <- step + 1) < mxstep) {
                    onestep(step)
                  }
                }
              }
              si <- order(pvi[, i])
              prof[[p.i]] <- data.frame(z = zi[si])
              prof[[p.i]]$par.vals <- pvi[si,, drop=FALSE]
            }
            new("profile.mle2", profile = prof, summary = summ)
          })


ICtab <- function(...,type=c("AIC","BIC","AICc"),
                  weights=FALSE,delta=FALSE,
                   sort=FALSE,nobs,dispersion=1,mnames,k=2) {
  L <- list(...)
  type <- match.arg(type)
  if (type=="AICc" || type=="BIC") {
    if (missing(nobs)) {
      if(is.null(attr(L[[1]],"nobs")))
        stop("must specify number of observations if corr=TRUE")
      nobs <- sapply(L,attr,"nobs")
      if (length(unique(nobs))>1)
        stop("nobs different: must have identical data for all objects")
    }
  }
  ICs <- switch(type,
                AIC=sapply(L,AIC),
                BIC=sapply(L,BIC,nobs=nobs),
                AICc=sapply(L,AICc,nobs=nobs))
  ## hack: protect against aod method
  if (is.matrix(ICs)) ICs <- ICs["AIC",]  
  getdf <- function(x) {
    if (!is.null(df <- attr(x,"df"))) return(df)
    else if (!is.null(df <- attr(logLik(x),"df"))) return(df)
  }
  df <- sapply(L,getdf)
  tab <- data.frame(IC=ICs,df=df)
  names(tab)[1] <- type
  dIC <- ICs-min(ICs)
  if (delta) {
    tab <- data.frame(tab,"dIC"=dIC)
    names(tab)[3] <- paste("d",type,sep="")
  }
  if (weights) {
      wts <- exp(-dIC/2)/sum(exp(-dIC/2))
      tab <- data.frame(tab,weight=wts)
    }
  if (missing(mnames)) {
    Call <- match.call()
    if (!is.null(names(Call))) {
      xargs <- which(names(Call) %in% names(formals())[-1])
    } else xargs <- numeric(0)
    mnames <- as.character(Call)[c(-1,-xargs)]
  }
  row.names(tab) <- mnames
  if (sort) {
    tab <- tab[order(ICs),]
  }
  class(tab) <- "ICtab"
  tab
}

print.ICtab <- function(x,...) {
  chtab <- format(do.call("cbind",lapply(x,round,1)))
  rownames(chtab) <- attr(x,"row.names")
  chtab[,"df"] <- as.character(x$df)
  if (!is.null(x$weight))
    chtab[,"weight"] <- format.pval(x$weight,eps=0.001,
                                    digits=3)
  print(chtab,quote=FALSE)
}

AICtab <- function(...) {
  ## fancy footwork to preserve model names
  Call <- match.call()
  xargs <- which(names(Call) %in% names(formals(ICtab))[-1])
  mnames <- as.character(Call)[c(-1,-xargs)]
  ICtab(...,mnames=mnames,type="AIC")
}
BICtab <- function(...) {
  Call <- match.call()
  xargs <- which(names(Call) %in% names(formals(ICtab))[-1])
  mnames <- as.character(Call)[c(-1,-xargs)]
  ICtab(...,mnames=mnames,type="BIC")
}

AICctab <- function(...) {
  Call <- match.call()
  xargs <- which(names(Call) %in% names(formals(ICtab))[-1])
  mnames <- as.character(Call)[c(-1,-xargs)]
  ICtab(...,mnames=mnames,type="AICc")
}

setGeneric("AICc", function(object, ..., nobs) standardGeneric("AICc"))

setMethod("AICc", "mle2",
          function (object, ..., nobs)  {
            L <- list(...)
            if (length(L)) {
              L <- c(list(object),L)
              if (missing(nobs) && is.null(attr(object,"nobs")))
                stop("must specify number of observations")
              nobs <- sapply(L,attr,"nobs")
              if (length(unique(nobs))>1)
                stop("nobs different: must have identical data for all objects")
              logLiks <- sapply(L, logLik)
              df <- sapply(L,attr,"df")
              val <- logLiks+k*df*(df+1)/(nobs-df-1)
              data.frame(AICc=val,df=df)
            } else {
              df <- attr(object,"df")
              c(-2*logLik(object)+2*df+2*df*(df+1)/(nobs-df-1))
            }
          })

setMethod("AICc", signature(object="logLik"),
function(object, ..., nobs){
  if (missing(nobs)) {
    if (is.null(attr(object,"nobs")))
      stop("number of observations not specified")
    nobs <- attr(object,"nobs")
  }
  df <- attr(object,"df")
  -2 * c(object) + 2*df+2*df*(df+1)/(nobs-df-1)
})

setMethod("AICc", signature(object="ANY"),
function(object, ..., nobs){
  AICc(object=logLik(object, ..., nobs=nobs))
})

setMethod("AIC", "mle2",
          function (object, ..., k = 2) {
            L <- list(...)
            if (length(L)) {
              L <- c(list(object),L)
              if (!all(sapply(L,class)=="mle2")) stop("all objects in list must be class mle2")
              logLiks <- lapply(L, logLik)
              AICs <- sapply(logLiks,AIC,k=k)
              df <- sapply(L,attr,"df")
              data.frame(AIC=AICs,df=df)
            } else AIC(logLik(object), k = k)
          })

##trace

## copied from stats4
setGeneric("BIC", function(object, ...) standardGeneric("BIC"))

setMethod("BIC", signature(object="logLik"),
          function(object, ...){
            args = list(...)
            nobs = args[["nobs"]]
            args[["nobs"]] <- NULL
            if (is.null(attr(object,"nobs"))) attr(object,"nobs") <- nobs
            nobs <- attr(object,"nobs")
            if (is.null(nobs)) {
              stop("can't determine number of observations")
            }
            -2 * c(object) + attr(object, "df") * log(nobs)
          })

setMethod("BIC", signature(object="ANY"),
          function(object, ...){
            BIC(object=logLik(object, ...))
          })

setMethod("BIC", "mle2",
          function (object, ...) {
            L <- list(...)
            if ("nobs" %in% names(L)) {
              nobs = L$nobs
              L[["nobs"]] <- NULL
            } else nobs <- NULL
            if (length(L)) {
              L <- c(list(object),L)
              logLiks <- lapply(L, logLik)
              BICs <- sapply(logLiks,BIC,nobs=nobs)
              df <- sapply(L,attr,"df")
              data.frame(BIC=BICs,df=df)
            }
            else BIC(logLik(object), nobs = nobs)
          })

setMethod("anova","mle2",
          function(object,...,width=getOption("width"),
                   exdent=10) {
            mlist <- c(list(object),list(...))
            ## get names from previous call
            mnames <- sapply(sys.call(sys.parent())[-1],deparse)
            ltab <- as.matrix(do.call("rbind",
                                      lapply(mlist,
                                             function(x) {
                                               c("Tot Df"=length(x@coef),
                                                 Deviance=-2*logLik(x))
                                             })))
            terms=sapply(mlist,
              function(obj) {
                if (is.null(obj@formula) || obj@formula=="") {
                  mfun <- obj@call$minuslogl
                  mfun <- paste("[",if (is.name(mfun)) {
                    as.character(mfun)
                  } else { "..." },
                                "]",sep="")
                  paste(mfun,": ",paste(names(obj@coef),
                                        collapse="+"),sep="")
                } else {
                  as.character(obj@formula)
                }
              })
            mterms <- paste("Model ",
                            1:length(mnames),": ",mnames,", ",terms,sep="")
            mterms <- strwrapx(mterms,width=width,exdent=exdent,
                               wordsplit="[ \n\t]")
  ## trunc.term <- function(s,len) {
  ##     ## cat("***",nchar(s),length(grep("\\+",s)),"\n",sep=" ")    
  ##     if ((nchar(s)<len) || (length(grep("\\+",s))==0)) return(s)
  ##     ## cat("abc\n")
  ##     lens <- cumsum(sapply(strsplit(s,"\\+")[[1]],nchar)+1)
  ##     paste(substr(s,1,max(lens[lens<len])-1),"+...",sep="")
  ##   }
  ## WRAP here
  heading <- paste("Likelihood Ratio Tests",
                   paste(mterms,
                         collapse="\n"),
                   sep="\n")
  ltab <- cbind(ltab,Chisq=abs(c(NA,diff(ltab[,"Deviance"]))),
                Df=abs(c(NA,diff(ltab[,"Tot Df"]))))
  ltab <- cbind(ltab,"Pr(>Chisq)"=c(NA,pchisq(ltab[,"Chisq"][-1],
                       ltab[,"Df"][-1],lower.tail=FALSE)))
  rownames(ltab) <- 1:nrow(ltab)
  attr(ltab,"heading") <- heading
  class(ltab) <- "anova"
  ltab
})

setMethod("plot", signature(x="profile.mle2", y="missing"),
function (x, levels, which=1:p, conf = c(99, 95, 90, 80, 50)/100, nseg = 50,
          plot.confstr = FALSE, confstr = NULL, absVal = TRUE, add = FALSE,
          col.minval="green", lty.minval=2,
          col.conf="magenta", lty.conf=2,
          col.prof="blue", lty.prof=1,
          xlabs=nm, ylab="z",
          show.points=FALSE,
          main, xlim, ylim, ...)
{
    ## Plot profiled likelihood
    ## Based on profile.nls (package stats)
    obj <- x@profile
    nm <- names(obj)
    p <- length(nm)
    ## need to save these for i>1 below
    no.xlim <- missing(xlim)
    no.ylim <- missing(ylim)    
    if (is.character(which)) which <- match(which,nm)
    confstr <- NULL
    if (missing(levels)) {
        levels <- sqrt(qchisq(pmax(0, pmin(1, conf)), 1))
        confstr <- paste(format(100 * conf), "%", sep = "")
    }
    if (any(levels <= 0)) {
        levels <- levels[levels > 0]
        warning("levels truncated to positive values only")
    }
    if (is.null(confstr)) {
        confstr <- paste(format(100 * pchisq(levels^2, 1)), "%", sep = "")
    }
    mlev <- max(levels) * 1.05
    ##    opar <- par(mar = c(5, 4, 1, 1) + 0.1)
    if (!missing(xlabs) && length(which)<length(nm)) {
      xl2 = nm
      xl2[which] <- xlabs
      xlabs <- xl2
    }
    for (i in seq(along = nm)[which]) {
      ## <FIXME> This does not need to be monotonic
      ## cat("**",i,obj[[i]]$par.vals[,i],obj[[i]]$z,"\n")
      if (missing(main)) setmain <- TRUE
      if (setmain) {
        main <- paste("Likelihood profile:",nm[i])
      }
      yvals <- obj[[i]]$par.vals[,nm[i],drop=FALSE]
      sp <- splines::interpSpline(yvals, obj[[i]]$z,
                                  na.action=na.omit)
      bsp <- try(splines::backSpline(sp),silent=TRUE)
      bsp.OK <- (class(bsp)[1]!="try-error")
      if (bsp.OK) {
        predfun <- function(y) { predict(bsp,y)$y }
      } else { ## backspline failed
        warning("non-monotonic profile: confidence limits may be unreliable")
        ## what do we do?
        ## attempt to use uniroot
        predfun <- function(y) {
          pfun0 = function(z1) {
            t1 = try(uniroot(function(z) {
              predict(sp,z)$y-z1
            }, range(obj[[i]]$par.vals[,i])),silent=TRUE)
            if (class(t1)[1]=="try-error") NA else t1$root
          }
          sapply(y,pfun0)
        }
      }
      ## </FIXME>
      if (no.xlim) xlim <- predfun(c(-mlev, mlev))
      if (is.na(xlim[1]))
        xlim[1] <- min(obj[[i]]$par.vals[, i])
      if (is.na(xlim[2]))
        xlim[2] <- max(obj[[i]]$par.vals[, i])
      xvals <- obj[[i]]$par.vals[,nm[i]]
      if (absVal) {
        if (!add) {
          if (no.ylim) ylim <- c(0,mlev)
          plot(abs(z) ~ xvals, data = obj[[i]],
               xlab = nm[i],
               ylab = if (missing(ylab)) expression(abs(z)) else ylab,
               xlim = xlim, ylim = ylim,
               type = "n", main=main, ...)
        }
        avals <- rbind(as.data.frame(predict(sp)),
                       data.frame(x = drop(yvals), y = obj[[i]]$z))
        avals$y <- abs(avals$y)
        lines(avals[order(avals$x), ], col = col.prof, lty=lty.prof)
        if (show.points) points(yvals,abs(obj[[i]]$z))
      } else { ## not absVal
          if (!add) {
            if (no.ylim) ylim <- c(-mlev,mlev)
            plot(z ~ xvals, data = obj[[i]], xlab = nm[i],
                 ylim = ylim, xlim = xlim,
                 ylab = if (missing(ylab)) expression(z) else ylab,
                 type = "n", main=main, ...)
          }
          lines(predict(sp), col = col.prof, lty=lty.prof)
          if (show.points) points(yvals,obj[[i]]$z)
        }
      x0 <- predfun(0)
      abline(v = x0, h=0, col = col.minval, lty = lty.minval)
      for (j in 1:length(levels)) {
        lev <- levels[j]
        confstr.lev <- confstr[j]
        ## Note: predict may return NA if we didn't profile
          ## far enough in either direction. That's OK for the
          ## "h" part of the plot, but the horizontal line made
          ## with "l" disappears.
          pred <- predfun(c(-lev, lev))
          ## horizontal
          if (absVal) levs=rep(lev,2) else levs=c(-lev,lev)
          lines(pred, levs, type = "h", col = col.conf, lty = 2)
          ## vertical
          pred <- ifelse(is.na(pred), xlim, pred)
          if (absVal) {
            lines(pred, rep(lev, 2), type = "l", col = col.conf, lty = lty.conf)
          } else {
            lines(c(x0,pred[2]), rep(lev, 2), type = "l", col = col.conf, lty = lty.conf)
            lines(c(pred[1],x0), rep(-lev, 2), type = "l", col = col.conf, lty = lty.conf)
          }
        if (plot.confstr) {
          text(labels=confstr.lev,x=x0,y=lev,col=col.conf)
        }
      } ## loop over levels
    } ## loop over variables
    ## par(opar)
  })

setMethod("confint", "profile.mle2",
function (object, parm, level = 0.95, trace=FALSE, ...)
{
  Pnames <- names(object@profile)
  if (missing(parm)) parm <- Pnames
  if (is.character(parm)) parm <- match(parm,Pnames)
  if (any(is.na(parm))) stop("parameters not found in model coefficients")
  ## Calculate confidence intervals based on likelihood
  ## profiles
  of <- object@summary
  pnames <- rownames(of@coef)
  if (missing(parm))
    parm <- seq(along=pnames)
  if (is.character(parm))
        parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(round(100 * a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2),
                dimnames = list(pnames[parm], pct))
    cutoff <- qnorm(a)
    std.err <- object@summary@coef[, "Std. Error"]
    call <- object@summary@call
    B0 <- object@summary@coef[,"Estimate"]
    for (pm in parm) {
      pro <- object@profile[[pnames[pm]]]
      sp <- if (length(pnames) > 1)
        spline(x = pro[, "par.vals"][, pm], y = pro[, 1])
      else spline(x = pro[, "par.vals"], y = pro[, 1])
      ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
    }
    drop(ci)
})

setMethod("confint", "mle2",
function (object, parm, level = 0.95, method,
          trace=FALSE,quietly=!interactive(),...)
{
  if (missing(method)) method <- mle2.options("confint")
  ## changed coef() calls to object@coef -- really *don't* want fullcoef!
  Pnames <- names(object@coef)
  if (missing(parm))
    parm <- seq(along=Pnames)
  if (is.character(parm)) parm <- match(parm,Pnames)
  if (any(is.na(parm))) stop("parameters not found in model coefficients")
  if (method=="spline") {
    if (!quietly) cat("Profiling...\n")
    prof = try(profile(object,which=parm))
    if (inherits(prof,"try-error")) stop(paste("Problem with profiling:",prof))
    return(confint(prof, parm, level, ...))
  } else {
    B0 <- object@coef
    pnames <- names(B0)
    if (missing(parm))
      parm <- seq(along=pnames)
    if (is.character(parm))
      parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(round(100 * a, 1), "%")
    pct <- paste(round(100 * a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2),
                dimnames = list(pnames[parm], pct))
    std.err <- summary(object)@coef[, "Std. Error"]
    if (method=="uniroot") {
      chisqcutoff <- qchisq(level,1)
      call <- object@call
      call$start <- as.list(B0) ## added
      for (pm in parm) {
        critfun <- function(step)
          {
            bi <- B0[pm] + sgn * step * std.err[pm]
            fix <- list(bi)
            names(fix) <- pnames[pm]
            call$fixed <- c(fix,eval(call$fixed))
            pfit <- try(eval(call), silent=TRUE)
            if(inherits(pfit, "try-error")) {
              warning(paste("Error encountered in profile (uniroot):",pfit))
              return(NA)
            }
            else {
              zz <- 2*pfit@min - 2*(-logLik(object))
              if (zz > -0.001)
                zz <- max(zz, 0)
              else
                stop("profiling has found a better solution, so original fit had not converged")
              z <- zz - chisqcutoff
            }
            if (trace) cat(bi, z, "\n")
            z
          }
        sgnvec=c(-1,1)
        for (i in 1:2) {
          sgn = sgnvec[i]
          c0 <- critfun(0)
          ctry <- 5
          cdel <- -0.25
          c5 <- NA
          while (is.na(c5) && ctry>0) {
            c5 <- critfun(ctry)
            if (is.na(c5)) {
              if (trace) cat("encountered NA, reducing ctry to",ctry+cdel,"\n")
              ctry <- ctry+cdel
            }
          }
          if (trace) cat(c0,c5,"\n")
          if (is.na(c0*c5) || c0*c5>0) {
            warning(paste("can't find confidence limits in",
                          c("negative","positive")[i],"direction"))
            curci <- NA
            ## FIXME: could try harder!
          } else {
            curci <- B0[pm]+sgn*std.err[pm]*uniroot(critfun,c(0,ctry))$root
          }
          ci[pnames[pm],i] <- curci
        }
      }
    } else if (method=="quad") {
      for (pm in parm) {
        ci[pnames[pm],] <- qnorm(a,B0[pm],std.err[pm])
      }
    } else stop("unknown method")
    return(drop(ci))
  }
})

setMethod("logLik", "mle2",
function (object, ...)
{
    if(length(list(...)))
        warning("extra arguments discarded")
    val <- -object@min
    attr(val, "df") <- length(object@coef)
    attr(val, "nobs") <- attr(object,"nobs")
    class(val) <- "logLik"
    val
  })

setMethod("vcov", "mle2", function (object, ...) { object@vcov } )

setMethod("update", "mle2",
function (object, ..., evaluate = TRUE)
{
  call <- object@call
  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras) > 0) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  if (evaluate) eval(call, parent.frame()) else call
})


mle2.options <- function(...) {
single <- FALSE
args <- list(...)
  setvals <- !is.null(names(args))
  if (!length(args)) args <- names(.Mle2.options)
if (all(unlist(lapply(args, is.character)))) 
     args <- as.list(unlist(args))
  if (length(args) == 1) {
    if (is.list(args[[1]]) | is.null(args[[1]])) 
      args <- args[[1]]
    else if (!setvals)
      single <- TRUE
  }
  if (setvals) {
    .Mle2.options[names(args)] <<- args
    value <- .Mle2.options[names(args)]
  } else value <- .Mle2.options[unlist(args)]
   if (single) value <- value[[1]]
if (setvals) invisible(value) else value
}


.Mle2.options = list(optim.method="BFGS",confint = "spline",optimizer="optim")


.onLoad <- function(lib, pkg) require(methods)

## should probably roll this in as an option to profile
## include attribute, warning? draw differently (leave off
## conf. limit lines)
slice <- function(fitted, ...) UseMethod("slice")

setMethod("slice", "mle2",    
function (fitted, which = 1:p, maxsteps = 100,
                          alpha = 0.01, zmax = sqrt(qchisq(1 - alpha/2, p)),
                          del = zmax/5, trace = FALSE, ...)
{
    onestep <- function(step)
    {
        bi <- B0[i] + sgn * step * del * std.err[i]
        fix <- list(bi)
        names(fix) <- p.i
        call$fixed <- c(fix,eval(call$fixed))
        call$eval.only = TRUE
        pfit <- try(eval(call), silent=TRUE) ##
        if(inherits(pfit, "try-error")) return(NA)
        else {
            zz <- 2*(pfit@min - fitted@min)
            ri <- pv0
            ri[, names(pfit@coef)] <- pfit@coef
            ri[, p.i] <- bi
            if (zz > -0.001)
                zz <- max(zz, 0)
            else stop("profiling has found a better solution, so original fit had not converged")
            z <- sgn * sqrt(zz)
            pvi <<- rbind(pvi, ri)
            zi <<- c(zi, z)
        }
        if (trace) cat(bi, z, "\n")
        z
      }
    ## Profile the likelihood around its maximum
    ## Based on profile.glm in MASS
    summ <- summary(fitted)
    std.err <- summ@coef[, "Std. Error"]
    Pnames <- names(B0 <- fitted@coef)
    pv0 <- t(as.matrix(B0))
    p <- length(Pnames)
    prof <- vector("list", length = length(which))
    names(prof) <- Pnames[which]
    call <- fitted@call
    call$minuslogl <- fitted@minuslogl
    for (i in which) {
        zi <- 0
        pvi <- pv0
        p.i <- Pnames[i]
        for (sgn in c(-1, 1)) {
          if (trace)
            cat("\nParameter:", p.i, c("down", "up")[(sgn + 1)/2 + 1], "\n")
          step <- 0
          z <- 0
          ## This logic was a bit frail in some cases with
          ## high parameter curvature. We should probably at least
          ## do something about cases where the mle2 call fails
          ## because the parameter gets stepped outside the domain.
          ## (We now have.)
          call$start <- as.list(B0)
          lastz <- 0
          while ((step <- step + 1) < maxsteps && abs(z) < zmax) {
            z <- onestep(step)
            if(is.na(z)) break
            lastz <- z
          }
          if(abs(lastz) < zmax) {
            ## now let's try a bit harder if we came up short
            for(dstep in c(0.2, 0.4, 0.6, 0.8, 0.9)) {
              z <- onestep(step - 1 + dstep)
              if(is.na(z) || abs(z) > zmax) break
            }
          } else if(length(zi) < 5) { # try smaller steps
            mxstep <- step - 1
            step <- 0.5
            while ((step <- step + 1) < mxstep) onestep(step)
          }
        }
        si <- order(pvi[, i])
        prof[[p.i]] <- data.frame(z = zi[si])
        prof[[p.i]]$par.vals <- pvi[si,, drop=FALSE]
    }
    new("slice.mle2", profile = prof, summary = summ)
  })


## reconstruct list structure:
##   v is a vector, l is the original list
##      to use as a template
relist <- function(v,l) {
if (is.list(v)) v <- unlist(v)
if (!all(sapply(l,mode)=="numeric")) {
    stop("can't relist non-numeric values")
  }
  lens = sapply(l,length)
  if (all(lens==1))
    return(as.list(v))
  l2 <- split(v,rep(1:length(l),lens))
  names(l2) <- names(l)
  l3 <- mapply(function(x,y) {
    if (!is.null(dim(y))) {
      z=array(x,dim(y)); dimnames(z)=dimnames(y); z
    } else {
      z=x; names(z)=names(y); z
    }
  },l2,l,SIMPLIFY=FALSE)
names(l3) <- names(l)
  l3
}

namedrop <- function(x) {
if (!is.list(x)) x
  for (i in seq(along=x)) {
    ## cat(i,length(x),"\n")
    n = names(x[[i]])
    lx = length(x[[i]])
    if (!is.null(n)) {
      if (lx==1) {
        names(x[[i]]) <- NULL
      } else if (length(unique(n))<lx) {
        names(x[[i]]) <- 1:lx
      }
    } ## !is.null(names(x[[i]]))
  } ## loop over elements
  x
}

"parnames<-" <- function(obj,value) {
attr(obj,"parnames") <- value
  obj
}

parnames <- function(obj) {
attr(obj,"parnames")
}


### TEST OF NLMINB
if (FALSE) {
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
mle2(y~dpois(lambda=ymax/(1+x/xhalf)),start=list(ymax=25,xhalf=3.06),
     optimizer="nlminb",fixed=list(ymax=38.76),lower=c(0,0),trace=TRUE)

f = calc_mle2_function(y~dpois(lambda=ymax/(1+x/xhalf)),
  start=list(ymax=25,xhalf=3.06))
f2 = function(xhalf) {
  -sum(dpois(y,38.76/(1+x/xhalf),log=TRUE))
}
optim(f2,par=3.06,method="BFGS")
## optim(f2,par=3.06,method="L-BFGS-B",lower=0) ## error
nlminb(objective=f2,start=3.06)
nlminb(objective=f2,start=3.06,lower=0)
nlminb(objective=f2,start=3.06,lower=1e-8)
}

strwrapx <- function(x, width = 0.9 * getOption("width"),
indent = 0, exdent = 0, 
prefix = "", simplify = TRUE,
    parsplit= "\n[ \t\n]*\n", wordsplit = "[ \t\n]") 
{
    if (!is.character(x)) 
      x <- as.character(x)
    indentString <- paste(rep.int(" ", indent), collapse = "")
    exdentString <- paste(rep.int(" ", exdent), collapse = "")
    y <- list()
    ## split at "+" locations
    plussplit = function(w) {
      lapply(w,
             function(z) {
               plusloc = which(strsplit(z,"")[[1]]=="+")
               plussplit =
                 apply(cbind(c(1,plusloc+1),
                             c(plusloc,nchar(z,type="width"))),
                       1,
                       function(b) substr(z,b[1],b[2]))
               plussplit
             })}
    ## ugh!
    z <- lapply(strsplit(x, parsplit),
                function(z) { lapply(strsplit(z,wordsplit),
                                   function(x) unlist(plussplit(x)))
                })
    ## print(str(lapply(strsplit(x,parsplit),strsplit,wordsplit)))
    ## print(str(z))
    for (i in seq_along(z)) {
        yi <- character(0)
        for (j in seq_along(z[[i]])) {
            words <- z[[i]][[j]]
            nc <- nchar(words, type = "w")
            if (any(is.na(nc))) {
                nc0 <- nchar(words)
                nc[is.na(nc)] <- nc0[is.na(nc)]
            }
            if (any(nc == 0)) {
                zLenInd <- which(nc == 0)
                zLenInd <- zLenInd[!(zLenInd %in% (grep("\\.$", 
                  words) + 1))]
                if (length(zLenInd) > 0) {
                  words <- words[-zLenInd]
                  nc <- nc[-zLenInd]
                }
            }
            if (length(words) == 0) {
                yi <- c(yi, "", prefix)
                next
            }
            currentIndex <- 0
            lowerBlockIndex <- 1
            upperBlockIndex <- integer(0)
            lens <- cumsum(nc + 1)
            first <- TRUE
            maxLength <- width - nchar(prefix, type = "w") - 
                indent
            while (length(lens) > 0) {
                k <- max(sum(lens <= maxLength), 1)
                if (first) {
                  first <- FALSE
                  maxLength <- maxLength + indent - exdent
                }
                currentIndex <- currentIndex + k
                if (nc[currentIndex] == 0) 
                  upperBlockIndex <- c(upperBlockIndex, currentIndex - 
                    1)
                else upperBlockIndex <- c(upperBlockIndex, currentIndex)
                if (length(lens) > k) {
                  if (nc[currentIndex + 1] == 0) {
                    currentIndex <- currentIndex + 1
                    k <- k + 1
                  }
                  lowerBlockIndex <- c(lowerBlockIndex, currentIndex + 
                    1)
                }
                if (length(lens) > k) 
                  lens <- lens[-(1:k)] - lens[k]
                else lens <- NULL
            }
            nBlocks <- length(upperBlockIndex)
            s <- paste(prefix, c(indentString, rep.int(exdentString, 
                nBlocks - 1)), sep = "")
            for (k in (1:nBlocks)) {
              s[k] <- paste(s[k],
                            paste(words[lowerBlockIndex[k]:upperBlockIndex[k]], 
                                  collapse = " "), sep = "")
            }
            s = gsub("\\+ ","+",s)  ## kluge
            yi <- c(yi, s, prefix)
        }
        y <- if (length(yi)) 
            c(y, list(yi[-length(yi)]))
        else c(y, "")
    }
    if (simplify) 
        y <- unlist(y)
    y
  }
