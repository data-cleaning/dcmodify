#' Data Modification By Modifying Rules
#' 
#' @section Introduction:
#' Data often contain errors and missing data. Experts can often correct
#' commonly occuring errors based on simple conditional rules. This package
#' facilitates the expression, management, and application of such rules on data
#' sets.
#' 
#' 
#' The general workflow in \code{validate.modify} follows the following pattern.
#' \itemize{
#'   \item Define or read a set of rules with \code{\link{modifier}}. 
#'   \item \code{\link{modify}} data with the modification rules.
#'   \item Examine the results either graphically or by summary.
#' }
#'
#' There are several convenience functions that allow one to define modification
#' rules from the commandline, through a (freeform or yaml) file and to
#' investigate and maintain the rules themselves. Please have a look at the
#' introductory vignette
#'
#' \code{vignette("intro",package="validate.modify")}
#'
#'
#'    
#' @docType package
#' @name validate.modify
#' @aliases package-validate.modify validate.modify
#' @import methods
#' @import yaml
#' @import validate
#' @importFrom settings clone_and_merge
#' @export origin 
#' @export description
#' @export label
#' @export variables
NULL


#' @include modifier.R
NULL


get_rule_guard <- function(r,dat, na.condition){
  g <- guard(r)
  I <- eval(g,dat)
  if ( is.null(I) ){ 
    rep(TRUE,nrow(dat)) 
  } else {
    I[is.na(I)] <- na.condition
    I
  }
}

#' @rdname modify
#' @export 
setMethod("modify",c("data.frame","modifier"), function(dat, x, ...){
  opts <- settings::clone_and_merge(x$._options,...)
  sequential <- opts("sequential")
  odat <- if (sequential) NULL else dat
  na.condition <- opts("na.condition")
  
  modifiers <- x$exprs(vectorize=FALSE
    , expand_assignments=TRUE
# temp hack to prevent expansion of linear (in)equalities.
# needs to be fixed when options are exported.
    , lin_eq_eps=0
    , lin_ineq_eps=0)
  for ( m in modifiers ){
    m <- set_guards(m)
    for (n in m){ # loop over nested conditionals
      I <- if (sequential) get_rule_guard(n, dat,na.condition) else get_rule_guard(n,odat,na.condition)
      if (any(I)) dat[I,] <- within(dat[I,,drop=FALSE], eval(n))
    }
  }
  
  dat
})

#' Shortcut to modify data
#' 
#' @param dat A \code{data.frame}
#' @param ... A comma-separated list of modifying rules.
#' 
#' @export 
modify_so <- function(dat, ...){
  modify(dat, modifier(...))
}




