ICtab <- function(...,type=c("AIC","BIC","AICc","qAIC","qAICc"),
                  weights=FALSE,delta=TRUE,base=FALSE,
                  sort=TRUE,nobs,dispersion=1,mnames,k=2) {
  L <- list(...)
  if (is.list(L[[1]]) && length(L)==1) L <- L[[1]]
  type <- match.arg(type)
  if (dispersion !=1) {
    if (type=="BIC") stop("cannot specify dispersion with BIC")
    if (substr(type,1,1)!="q") {
      type = paste("q",type,sep="")
      warning("dispersion!=1, type changed to ",type)
    }
  }
  if (type=="AICc" || type=="BIC" || type=="qAICc") {
    if (missing(nobs)) {
      if(is.null(attr(L[[1]],"nobs")))
        stop("must specify number of observations if corr=TRUE")
      nobs <- sapply(L,attr,"nobs")
      if (length(unique(nobs))>1)
        stop("nobs different: must have identical data for all objects")
      nobs <- nobs[1]
    }
  }
  ICs <- switch(type,
                AIC=sapply(L,AIC),
                BIC=sapply(L,BIC),
                AICc=sapply(L,AICc,nobs=nobs),
                qAIC=sapply(L,qAIC,dispersion=dispersion),
                qAICc=sapply(L,qAICc,nobs=nobs,dispersion=dispersion))
  ## hack: protect against aod method
  if (is.matrix(ICs)) ICs <- ICs["AIC",]  
  getdf <- function(x) {
    if (!is.null(df <- attr(x,"df"))) return(df)
    else if (!is.null(df <- attr(logLik(x),"df"))) return(df)
  }
  dIC <- ICs-min(ICs)
  df <- sapply(L,getdf)
  if (base) {
    tab <- data.frame(IC=ICs,df=df)
    names(tab)[1] <- type
  } else if (delta) {
    tab <- data.frame(dIC=dIC,df=df)
    names(tab)[1] <- paste("d",type,sep="")
  }
  if (delta && base) {
    tab <- data.frame(tab,dIC=dIC)
    names(tab)[3] <- paste("d",type,sep="")
  }
  if (!delta && !base) stop("either 'base' or 'delta' must be TRUE")
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

print.ICtab <- function(x,...,min.weight=0.001) {
    chtab <- format(do.call("cbind",lapply(x,round,1)))
    rownames(chtab) <- attr(x,"row.names")
    chtab[,"df"] <- as.character(x$df)
    if (!is.null(x$weight))
        chtab[,"weight"] <- format.pval(x$weight,eps=min.weight,
                                        digits=2)
    print(chtab,quote=FALSE)
}

AICtab <- function(...) {
  ## fancy footwork to preserve model names
  ICtab(...,mnames=get.mnames(match.call()),type="AIC")
}
BICtab <- function(...) {
  ICtab(...,mnames=get.mnames(match.call()),type="BIC")
}

AICctab <- function(...) {
  ICtab(...,mnames=get.mnames(match.call()),type="AICc")
}

setGeneric("AICc", function(object, ..., nobs, k=2) standardGeneric("AICc"))

setMethod("AICc", "mle2",
          function (object, ..., nobs, k)  {
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
              val <- -2*logLiks+k*df*(df+1)/(nobs-df-1)
              data.frame(AICc=val,df=df)
            } else {
              df <- attr(object,"df")
              c(-2*logLik(object)+k*df+k*df*(df+1)/(nobs-df-1))
            }
          })

setMethod("AICc", signature(object="logLik"),
function(object, ..., nobs, k){
  if (missing(nobs)) {
    if (is.null(attr(object,"nobs")))
      stop("number of observations not specified")
    nobs <- attr(object,"nobs")
  }
  df <- attr(object,"df")
  ## FIXME: should second "2" also be k?
  -2 * c(object) + k*df+2*df*(df+1)/(nobs-df-1)
})

setMethod("AICc", signature(object="ANY"),
function(object, ..., nobs, k){
  AICc(object=logLik(object, ...), nobs=nobs, k=k)
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

### quasi- methods

setGeneric("qAICc", function(object, ..., nobs, dispersion, k=2)
           standardGeneric("qAICc"))

setMethod("qAICc", signature(object="ANY"),
function(object, ..., nobs, dispersion, k=2){
  qAICc(object=logLik(object), nobs=nobs, dispersion=dispersion, k=k)
})

setMethod("qAICc", "mle2",
          function (object, ..., nobs, dispersion, k)  {
            L <- list(...)
            if (length(L)) {
              L <- c(list(object),L)
              if (missing(nobs) && is.null(attr(object,"nobs")))
                stop("must specify number of observations")
              if (missing(dispersion) && is.null(attr(object,"dispersion")))
                stop("must specify (over)dispersion coefficient")
              nobs <- sapply(L,attr,"nobs")
              if (length(unique(nobs))>1)
                stop("nobs different: must have identical data for all objects")
              logLiks <- sapply(L, logLik)/dispersion
              df <- sapply(L,attr,"df")+1 ## add one for scale parameter
              val <- logLiks+k*df*(df+1)/(nobs-df-1)
              data.frame(AICc=val,df=df)
            } else {
              df <- attr(object,"df")
              c(-2*logLik(object)/dispersion+2*df+2*df*(df+1)/(nobs-df-1))
            }
          })

setMethod("qAICc", signature(object="logLik"),
          function(object, ..., nobs, dispersion, k){
            if (missing(nobs)) {
              if (is.null(attr(object,"nobs")))
                stop("number of observations not specified")
              nobs <- attr(object,"nobs")
            }
            if (missing(dispersion)) {
              if (is.null(attr(object,"dispersion")))
                stop("dispersion not specified")
              dispersion <- attr(object,"dispersion")
            }
            df <- attr(object,"df")+1 ## add one for scale parameter
            -2 * c(object)/dispersion + k*df+2*df*(df+1)/(nobs-df-1)
          })

setGeneric("qAIC", function(object, ..., dispersion, k=2)
           standardGeneric("qAIC"))

setMethod("qAIC", signature(object="ANY"),
function(object, ..., dispersion, k=2){
  qAIC(object=logLik(object), dispersion=dispersion, k)
})

setMethod("qAIC", signature(object="logLik"),
          function(object, ..., dispersion, k){
            if (missing(dispersion)) {
              if (is.null(attr(object,"dispersion")))
                stop("dispersion not specified")
              dispersion <- attr(object,"dispersion")
            }
            df <- attr(object,"df")
            -2 * c(object)/dispersion + k*df
          })

setMethod("qAIC", "mle2",
          function (object, ..., dispersion, k=2) {
            L <- list(...)
            if (length(L)) {
              L <- c(list(object),L)
              if (!all(sapply(L,class)=="mle2"))
                stop("all objects in list must be class mle2")
              logLiks <- lapply(L, logLik)
              AICs <- sapply(logLiks,qAIC, k=k, dispersion=dispersion)
              df <- sapply(L,attr,"df")
              data.frame(AIC=AICs,df=df)
            } else {
              qAIC(logLik(object), k=k, dispersion=dispersion)
            }
          })

## copied from stats4
## setGeneric("BIC", function(object, ...) standardGeneric("BIC"))

## setMethod("BIC", signature(object="logLik"),
##           function(object, ...){
##             args = list(...)
##             nobs = args[["nobs"]]
##             args[["nobs"]] <- NULL
##             if (is.null(attr(object,"nobs"))) attr(object,"nobs") <- nobs
##             nobs <- attr(object,"nobs")
##             if (is.null(nobs)) {
##               stop("can't determine number of observations")
##             }
##             -2 * c(object) + attr(object, "df") * log(nobs)
##           })

## setMethod("BIC", signature(object="ANY"),
##           function(object, ...){
##             BIC(object=logLik(object, ...))
##           })

## setMethod("BIC", "mle2",
##           function (object, ...) {
##             L <- list(...)
##             if ("nobs" %in% names(L)) {
##               nobs = L$nobs
##               L[["nobs"]] <- NULL
##             } else nobs <- NULL
##             if (length(L)) {
##               L <- c(list(object),L)
##               logLiks <- lapply(L, logLik)
##               BICs <- sapply(logLiks,BIC,nobs=nobs)
##               df <- sapply(L,attr,"df")
##               data.frame(BIC=BICs,df=df)
##             }
##             else BIC(logLik(object), nobs = nobs)
##           })
