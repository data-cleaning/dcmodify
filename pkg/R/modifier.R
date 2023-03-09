

#' Store modification rules
#'
#' @keywords internal
setRefClass("modifier"
    , contains = "expressionset"
    , fields = c(
        "source"
    )
    , methods = list(
      show = function() show_modifier(.self)
      , initialize = function(..., .file, .data){
          obj <- ini_modifier(.self, ..., .file=.file, .data=.data)
          obj$source <- if(!missing(.file)) .file else NULL
          obj
      }
      , assignments = 
        function(flatten = TRUE, dplyr_verbs = FALSE){
          guarded_assignments( .self
                             , flatten = flatten
                             , dplyr_verbs = dplyr_verbs
                             )
        }
    ) 
)


show_modifier <- function(obj){
  cat(sprintf("Object of class %s with %d elements:\n",class(obj)[1],length(obj)))
  nm <- names(obj)
  lb <- label(obj)
  calls <- lapply(obj$rules,expr)
  
  for ( i in seq_along(calls)){
    cat(sprintf("%s: %s\n  %s\n\n",nm[i],lb[i],gsub("\n","\n  ",as.character(calls[i]))  ))
  }
}

# TODO add flatten argument
guarded_assignments <- function(obj, flatten = TRUE, dplyr_verbs = FALSE){
  
  expr <- obj$exprs(  vectorize=FALSE
                   ,  expand_assignments=TRUE
                   )
  m <- list()
  for (n in names(expr)){
    guards <- set_guards(expr[[n]], dplyr_verbs = dplyr_verbs)
    # flat list of all assignments
    if (isTRUE(flatten)){
      names(guards) <- rep(n, length(guards))
      m <- c(m, guards)
    } else {
    # nested list of assignment per expression
      m[[n]] <- guards
    }
  }
  names(m) <- make.unique(names(m))
  m
}

call2text <- function(cl){
  capture.output(print(cl))
}


#' Create or read a set of data modification rules
#'
#'
#' @param ... A comma-separated list of modification rules.
#' @param .file (optional) A character vector of file locations.
#' @param .data (optional) A \code{data.frame} with at least a column \code{"rule"}
#'        of type \code{character}. Optionally, the following columns of metadata
#'        can be provided (all \code{character}, except \code{"created"} which 
#'        should be \code{POSIXct}): \code{"name"}, 
#'        \code{"label"}, \code{"description"}, \code{"origin"}, \code{"created"}.
#'
#' @return An object of class \code{modifier}.
#'
#' @examples 
#' m <- modifier( if (height < mean(height)) height <- 2*height
#' , if ( weight > mean(weight) ) weight <- weight/2  )
#' modify(women,m)
#' 
#' 
#' @export
modifier <- function(..., .file, .data) new("modifier", ... , .file=.file, .data=.data)




ini_modifier <- function(obj ,..., .file, .data){
  if ( missing(.file) && missing(.data)){
    validate::.ini_expressionset_cli(obj, ..., .prefix="M")
  } else if (!missing(.file)) {
    validate::.ini_expressionset_yml(obj, file=.file, .prefix="M")
  }else if (!missing(.data)) {
    validate:::.ini_expressionset_df(obj, dat=.data, .prefix="M")
  }
  
  i <- sapply(expr(obj),is_modifying)
  
  if ( !all(i) ){
    err <- paste(sprintf("\n[%03d] %s", which(!i), sapply(expr(obj)[!i], call2text )))
    warning(paste0(
      "Invalid syntax detected. The following expressions have been ignored:",
      paste(err,collapse="") 
    ))
  }
  if ( length(i) > 0 ){
    obj$rules <- obj$rules[i]
    obj$._options <- validate::.PKGOPT
    obj$._options(lin.eq.eps=0, lin.ineq.eps=0)
  }
  obj
}

# cl: a call.
is_modifying <- function(cl){
  
  if (is_assignment(cl)) return(TRUE)
  
  # if statement.
  if (is_if(cl)){
    return(all(sapply(cl[-c(1,2)],is_modifying)))
  }
  
  # block
  if (is_block(cl)){
    return(all(sapply(cl[-1],is_modifying)))
  }

  FALSE
}

#### S4 GENERICS --------------------------------------------------------------


#' Modify a data set
#'
#' @param dat An R object carrying data
#' @param x An R object carrying modififying rules
#' @param ... Options.
#'
#' @export
setGeneric("modify", function(dat, x, ...) standardGeneric("modify"))


setGeneric("expr", function(x,...) standardGeneric("expr"))

#### S4 IMPLEMENTATIONS
setMethod("expr","modifier",function(x,...){
  lapply(x$rules, function(r) r@expr)
})





