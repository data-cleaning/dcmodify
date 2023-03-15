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
#' @docType package
#' @name dcmodify
#' @aliases dcmodify-package dcmodify
#' @import methods
#' @import yaml
#' @import validate
#' @importFrom settings clone_and_merge
#' @importFrom utils capture.output
#' @importFrom R6 R6Class
#' @export origin 
#' @export description
#' @export label
#' @export variables
NULL


#' @include modifier.R
NULL


get_rule_guard <- function(r,dat, na.condition){
  g <- guard(r)
  if (is.null(g)){
    return(rep(TRUE,nrow(dat)))
  }
  I <- eval(g,dat)
  if ( is.null(I) ){
    rep(FALSE,nrow(dat))
  } else {
    I[is.na(I)] <- na.condition
    I
  }
}

#TODO: Overzetten naar lumberjack.
no_log <- R6::R6Class("no_log"
                  , private = list(
                    verbose = NULL
                  )
                  , public = list(
                    label = NULL
                    , initialize = function( verbose = TRUE){
                      private$verbose <- verbose
                    }
                    , add = function(meta, input, output){
                      # NOP! we don't store anything!
                    }
                    , dump = function(file=NULL,...){
                      if (is.character(file) && private$verbose ){
                        msgf("NO log dumped at %s", normalizePath(file))
                      }
                    }
                    , logdata = function(){
                      data.frame()
                    }
                  )
)


#' @rdname modify
#'
#' @param dat A \code{data.frame}
#' @param x A \code{modifier} object containing modifying rules.
#' @param ... Options
#' @param logger Optional. A \code{lumberjack}-compatible logger object.
#' @param ... Extra arguments.
#' 
#' @examples 
#' m <- modifier( if (height < mean(height)) height <- 2*height
#' , if ( weight > mean(weight) ) weight <- weight/2  )
#' modify(women,m)
#' @export 
setMethod("modify",c("data.frame","modifier"), function(dat, x, logger=NULL, ...){
  opts <- settings::clone_and_merge(x$._options,...)
  sequential <- opts("sequential")
  odat <- if (sequential) NULL else dat
  
  logger <- if(is.null(logger)) no_log$new() else logger
  
  na.condition <- opts("na.condition")
  asgnmts <- x$assignments()
  for (n in asgnmts){
    pre.dat <- dat
    
    I <- if (sequential) {
      get_rule_guard(n, dat,na.condition)
    } else {
      get_rule_guard(n,odat,na.condition)
    }
    if (all(I)){
      dat <- within(dat, eval(n))
    } else {
      if (any(I)) dat[I,] <- within(dat, eval(n))[I,,drop=FALSE]
    }

    meta     <- list(  expr = n
                     , src = deparse(n)
                     , file = x$source
#                     , line = lines # regelnummers in file. 
                    )

    logger$add(meta, pre.dat, dat)
    
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
