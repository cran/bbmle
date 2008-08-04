setMethod("predict", "mle2",
 function(object,newdata=NULL,
                         location=c("mean","median"), ...) {
    ## notes: should operate on formula
    ## pull out call$formula (not character)
    location <- match.arg(location)
    if (class(try(form <- as.formula(object@call$minuslogl)))!="formula")
      stop("can only use predict() if formula specified")
    LHS <- form[[3]]
    ddist = as.character(LHS[[1]])
    sdist = gsub("^d","s",ddist)
    arglist = as.list(LHS)[-1]
    if (!exists(sdist) || !is.function(get(sdist)))
      stop("function ",sdist," does not exist")
    ## evaluate parameters
    ## evaluate sdist [ newdata > coef > data ]
    parameters <- eval(object@call$parameters)
    if (!is.null(parameters)) {
        vars <- as.character(sapply(parameters,"[[",2))
        models <-  sapply(parameters,function(z) call.to.char(z[[3]]))
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
                mmat <- model.matrix(p,data=c(newdata,object@data))
                pnames <- paste(vname,colnames(mmat),sep=".")
                assign(vname,mmat %*% coef(object)[pnames])
            }
        }
    }
    arglist1 <- lapply(arglist,eval,envir=c(newdata,object@data,as.list(coef(object))),
                       enclos=sys.frame(sys.nframe()))
    vals <- with(as.list(coef(object)),do.call(sdist,arglist1))
    return(vals[[location]])
})
