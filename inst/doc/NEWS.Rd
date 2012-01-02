\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}

\name{NEWS}
\title{bbmle News}
\encoding{UTF-8}

	\section{CHANGES IN VERSION 0.9.8 (29/03/2011)}{
	  \itemize{
	    \item change NEWS format to NEWS.Rd
	    \item gradient functions work better with fixed parameters, hence with profiling
	    \item profile plot reverts to linear for non-monotonic profile
	    \item added warning in confint for non-monotonic profile, or for non-monotonic spline fit to monotonic profile; revert from spline+linear to linear approximation in this case
	    \item various documentation improvements, including adding
	  details of profile computation
	  \item optimx improvements
	  \item require \code{data} argument when using formula interface
	  \item turn off hessian computation in profile
	  \item allow use of \code{MASS::ginv}
 }
}

 \section{CHANGES IN VERSION 0.9.7}{
   \itemize{
     \item bug fix in calc_mle2_function for no-intercept models
     (thanks to Colin Kremer)
     \item fixed optimx, added 'user' option
   }
   }

 \section{CHANGES IN VERSION 0.9.6}{
   \itemize{
     \item changed hessian calculation to use numDeriv code (causes tiny
     changes to hessian results that could matter in edge cases)
     too lazy to provide a backward compatibility mode ...
     \item documented optimizer= choices in ?mle2
   }
   }

 \section{CHANGES IN VERSION 0.9.5.1}{
   \itemize{
     \item fixed bug in AICc (David Harris)
   }
   }


 \section{CHANGES IN VERSION 0.9.5}{
   \itemize{
     \item  too many changes!
 \item added NAMESPACE, various fixes to go with that
 \item beginnings of an RUnit testing framework
 \item tweaked vignette
 \item added prof.lower, prof.upper to profile()
 \item added "optimize" to list of allowed optimizers, some
 bug fixes
 }
}

   \section{CHANGES IN VERSION 0.9.4.1}{
     \itemize{
       \item tweaked par() resetting in profile plots
     }
     }

 \section{CHANGES IN VERSION 0.9.4}{
   \itemize{
     \item more qAICc fixing
   }
   }

\section{CHANGES IN VERSION 0.9.3 (version bump from previous:
  18/09/2009)}{
  \itemize{
    \item tweaked handling of bounds: profile now succeeds
    on some 1D problems where it didn't before
  }
  }

\section{CHANGES IN VERSION 0.9.2 (15/08/2009)}{
\itemize{
\item added deviance, residuals methods
\item added newparams argument to predict, simulate;
   newdata argument to simulate
\item added vignette (stub)
\item added explicit params argument, to help sort out full parameter specifications when parameters is 
  non-NULL
\item fixed predict() for case with parameters
\item added snorm
\item changed ICtab defaults to weight=TRUE, base=FALSE, sort=TRUE
}}

\section{CHANGES IN VERSION 0.9.1}{
  \itemize{

    \item added simulate method (formula interface only)
    \item fix AICctab bug
    \item remove spurious cat/print in profile
    \item fix qAIC bug
  }
  }

  \section{CHANGES IN VERSION 0.9.0 (26/08/2008)}{
    \itemize{
      \item  fix Tom Hobbs bug: named lower/upper/parscale/ndeps
      get rearranged properly, otherwise rearrange in order
      of "start" and issue a warning
      \item documentation tweak for S4 as.data.frame
      \item added sbeta to list of known distributions
      \item removed nlme requirement & auto-loading
    }
    }

\section{CHANGES IN VERSION 0.8.9 (04/08/2008)}{
  \itemize{
    \item version bump, submit to CRAN
    \item added predict method
  }
  }

\section{CHANGES IN VERSION 0.8.8  (10/07/2008)}{
  \itemize{
  \item added flexibility for profile plotting (main, x labels etc.);
    added examples
\item added an instance of "namedrop" to fix naming problem
\item added tol.newmin to slice etc.
\item added check for numeric return from profile within confint
\item fixed bugs in profile plotting when profile is restricted
   to a subset of variables
\item added tests for par() to reset to original on exit
\item improved profile documentation
\item replicate std.err if specified in profile
\item add as.data.frame 
\item tweak tol.newmin (better fit found during profile) code
}
}
\section{CHANGES IN VERSION 0.8.7 (12/05/2008)}{
  \itemize{
    \item version bump, moved to R-forge.
    \item reordered NEWS file (most recent first)
  }
  }
     
\section{CHANGES IN VERSION 0.8.6.1 (26/03/2008)}{
  \itemize{
    \item tweaked stop-on-better-fit code
    \item fixed (?) qAIC(c) methods
    \item tweak/fix to ICtab documentation (thanks to Tom Hobbs)
  }
}




   \section{CHANGES IN VERSION 0.8.6}{
     \itemize{
       \item added qAIC(c) methods (not working yet!)
     }
     }

  \section{CHANGES IN VERSION 0.8.5.1}{
    \itemize{
      \item oops. Fixed infelicity (bug?) in new   environment manipulation
    }
    }

  \section{CHANGES IN VERSION 0.8.5}{
  \itemize{
    \item tweaked environment/data assignment to preserve
    original minuslogl environment better
  }
  }

    \section{CHANGES IN VERSION 0.8.4}{
      \itemize{
      \item changed  plot.profile.mle2 options (added onepage etc.,
      \made plot.confstr=TRUE by default)
    }
    }

   \section{CHANGES IN VERSION 0.8.3}{
   \itemize{
     \item added warning about too-short lower/upper 
     \item added documentation
   }
   }

      \section{CHANGES IN VERSION 0.8.2}{
	\itemize{
	  \item fixed bug in AICctab
	  \item cosmetic change to printing -- save call.orig
	  \item moved ChangeLog to NEWS
	}
	}

      \section{CHANGES IN VERSION 0.8.1}{
	\itemize{
	  \item fixed (?) environment bug
	  tried to use built-in relist, but failed: renamed relist
	  to "relist2" (try again later)
	  \item documented get.mnames (auxiliary function for ICtabs)
	  \item started to add gr (gradient) capability -- NOT TESTED
	}
	}
	  

      \section{CHANGES IN VERSION 0.8}{
	\itemize{
	\item changed ICtab to allow either ICtab(x,y,z) or ICtab(list(x,y,z))
	(L <- list(...); if is.list(L[[1]]) && length(L)==1)
      }
      }

	  \section{CHANGES IN VERSION 0.7.7}{
	    \itemize{
	      \item  fix bug in profiling: all optim() methods EXCEPT L-BFGS-B
	      return the value of the objective function if given a function
	      with no arguments/zero-length starting parameter vector
	      (this is the situation with "profiling" a 1-D function).
	      \item L-BFGS-B gives funky answers.  added a check for this case.
	      (may need to check behavior for alternate optimizers (nlm etc))
	      [this behavior triggered a "found better fit" error when profiling
	      1D functions with L-BFGS-B]
	      \item changed behavior when finding better fit during profiling
	      to return new parameters
	    }
	    }


     \section{CHANGES IN VERSION 0.7.6}{
       \itemize{
	 \item tweak vignette
	 \item fixed second major AICc bug (was fixed in mle2 method,
	 but not in logLikmethod)
	 }}

	 \section{CHANGES IN VERSION 0.7.5}{
	   \itemize
	   change "ll" to "LL" in examples for clarity
  \item tweaked anova reporting of models (wrap instead of truncating)
    added (undocumented) show.points option to profile plot
	to display actual locations of profile evaluation
    tweaked profile to behave better when profiling variables
	with constraints (upper, lower)
    moved vignette to inst/doc where it belongs
    ICtab hack to protect against package:aod definition of AIC(logLik)
    added submit stub
    tweaked slice.mle2-class docs for consistency
    fiddled with vignette
    preliminary code to allow non-monotonic profiles
    preliminary add nlm to list of optimizers (untested)
    add aod, Hmisc, emdbook to VignetteDepends and Suggests:

    \section{CHANGES IN VERSION 0.7}{
      \itemize{
      \item better df extraction in ICtab
      \item minor bug fix for AICc (allows AICc of nls objects)
      \item     handle models with -1 in formula better: starting values set "all equal"
      \item made ANOVA formula line-length accessible
      \item added skip.hessian and trace arguments to mle2
      \item messed around with BIC definition -- attempt at consistency with nlme
      \item added rudimentary support for nlminb, constrOptim
      \item nlme now required for fdHess (which is required for
	nlminb since it doesn't compute a finite-diff
	Hessian)
      }
      }

	\section{CHANGES IN VERSION 0.6}{
	  \itemize{
	  \item add experimental formula interface
	  \item change all names from mle to mle2 to avoid confusion/conflicts
       with stats4 version of mle
       \item change internal structure of data evaluation
       \item worked on vignette
       \item added optimizer slot (stub)
     }
     }

    \section{CHANGES IN VERSION 0.5}{
    \itemize{
      fix AICc bug!  (was deviance+2*k*(k+1)/(n-k-1), not
      AIC+2*k*(k+1)/(n-k-1))
    }
    }


  \section{CHANGES IN VERSION 0.4}{
    \itemize{
    \item change AIC to AICc for corrections
    \item add AICtab for weights, delta, sort ... options
    \item expose error messages occuring within profile()
    \item uniroot tries harder to find a valid endpoint
    \item truncate terms in anova.mle at 80 characters
  }
  }

    \section{CHANGES IN VERSION 0.3}{
      \itemize{
      \item enhanced anova method, works with print.anova
      \item tweaked namedrop() code -- ??
    }
    }
    
\section{CHANGES IN VERSION 0.2}{
  \itemize{
  \item added parnames, parnames<-
  \item minor fix to allow "profiles" of 1-parameter models (skip fdHess call)
  \item minor change to print method for mle results
  \item tweaking "vecpar" (to allow parameter vectors in objective function)
  \item removed fdHess/nlme dependency
  }
