#' Data Modification By Modifying Rules
#'
#' @description
#' Data often contain errors and missing data. Experts can often correct
#' commonly occuring errors based on simple conditional rules. This package
#' facilitates the expression, management, and application of such rules on data
#' sets.
#'
#'
#' The general workflow in \code{dcmodify} follows the following pattern.
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
#' \code{vignette("introduction",package="dcmodify")}
#'
#'
#'
#' @name dcmodify
#' @aliases dcmodify-package dcmodify
#' @import methods
#' @import yaml
#' @import validate
#' @importFrom settings clone_and_merge
#' @importFrom utils capture.output
#' @importFrom lumberjack no_log
#' @export origin 
#' @export description
#' @export label
#' @export variables
"_PACKAGE"


#' @include modifier.R
NULL


get_rule_guard <- function(r, dat, na.condition, ref, alt_guard=rep(TRUE,nrow(dat))){
  g <- guard(r)
  # NULL is the case where the guard should be copied from
  # alt_guard: this occurs when r comes after the first assignment in
  # a block.
  if (is.null(g)){
    return(alt_guard)
  }
  I <- eval(g,dat, enclos = ref)
  if ( is.null(I) ){
    rep(FALSE,nrow(dat))
  } else {
    I[is.na(I)] <- na.condition
    I
  }
}

namecheck <- function(x){
  n1 <- ls(x)
  n2 <- ls(parent.env(x))
  i <- n1 %in% n2
  if (any(i)){
    n <- paste(paste0("'",n1[i],"'"),collapse=", ") 
    w <- sprintf("Possible reference ambiguity: both current data set and reference data have variables named %s.",n)
    warning(w)
  }
  x
}


#' @rdname modify
#'
#' @param dat A \code{data.frame}
#' @param x A \code{modifier} object containing modifying rules.
#' @param ref A \code{environment}
#' @param ... Options
#' @param logger Optional. A \code{lumberjack}-compatible logger object.
#' @param ... Extra arguments.
#' 
#' @examples 
#' m <- modifier( if (height < mean(height)) height <- 2*height
#' , if ( weight > mean(weight) ) weight <- weight/2  )
#' modify(women,m)
#' @export
setMethod("modify",signature("data.frame","modifier","environment"), function(dat, x, ref, logger=NULL, ...){
  data_env <- namecheck(list2env(dat,parent=ref))
  data_env$. <- dat
  modify_work(data_env, x, logger,...)
})

#' @rdname modify
setMethod("modify",signature("data.frame","modifier"), function(dat, x, logger=NULL, ...){
  data_env <- list2env(dat)
  data_env$. <- dat
  modify_work(data_env, x, logger,...)
})

#' @rdname modify
setMethod("modify",signature("data.frame","modifier","data.frame"),function(dat, x,ref, logger=NULL,...){
  env <- new.env()
  env$ref <- ref
  data_env <- namecheck(list2env(dat, parent=env))
  data_env$. <- dat
  modify_work(data_env, x, logger, ...)
})

#' @rdname modify
setMethod("modify",signature("data.frame","modifier","list"),function(dat, x, ref, logger=NULL,...){
  env <- list2env(ref)  
  data_env <- namecheck(list2env(dat,parent=env))
  data_env$. <- dat
  modify_work(data_env, x, logger,...)  
})


modify_work <- function(dat, x, logger=NULL, ...){
  opts <- settings::clone_and_merge(x$._options,...)
  sequential <- opts("sequential")
  frame <- dat$.
  odat <- if (sequential) NULL else frame
  logger <- if(is.null(logger)) no_log$new() else logger
  
  na.condition <- opts("na.condition")
  asgnmts <- x$assignments()
  i <- 0
  I <- rep(TRUE, nrow(frame))
  for (n in asgnmts){
    i <- i+1
    dat$n <- n
    pre_dat <- frame
    
    I <- if (sequential) {
      get_rule_guard(n, frame, na.condition,ref = dat , alt_guard=I)
    } else {
      get_rule_guard(n,odat, na.condition, ref =dat, alt_guard=I)
    }
    if (all(I)){
      frame <- within_env(frame, eval(n), dat)
    } else {
      if (any(I)) frame[I,] <- within_env(frame, eval(n), dat)[I,,drop=FALSE]
    }

    meta     <- list(  expr = n
                     , src = paste(deparse(n),collapse="\n")
                     , file = "<modifier>"
                     , line = c(i,i)
                    )

    logger$add(meta, pre_dat, frame)
    
  }

  frame
}

#' Shortcut to modify data
#'
#' @param dat A \code{data.frame}
#' @param ... A comma-separated list of modifying rules.
#'
#' @export
modify_so <- function(dat, ...){
  modify(dat, modifier(...))
}

within_env <- function (data, expr, enclos, ...) 
{
  parent <- enclos
  e <- evalq(environment(), data, parent)
  eval(substitute(expr), e)
  l <- as.list(e, all.names = TRUE)
  l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
  nl <- names(l)
  del <- setdiff(names(data), nl)
  data[nl] <- l
  data[del] <- NULL
  data
}

