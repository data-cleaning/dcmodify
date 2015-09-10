
#' Store modification rules
#'
#' @keywords internal
setRefClass("modifier"
    , contains = "expressionset"
    , methods = list(
      initialize = function(..., .file) ini_modifier(.self, ..., .file=.file)
    ) 
)



#' Create or read a set of data modification rules
#'
#'
#' @param ... A comma-separated list of modification rules.
#' @param .file A character vector of file locations.
#'
#' @return An
#' @export
modifier <- function(..., .file) new("modifier", ... , .file=.file)

# Unfortunately we need to import this stuff directly from 'validate'
# because cross-package inheritance don't work very well for reference
# classes. Ref classes are needed because we want multiple dispatch.
show_expressionset <- getFromNamespace("show_expressionset","validate")
call2text <- getFromNamespace("call2text","validate")
get_exprs <- getFromNamespace("get_exprs","validate")

ini_modifier <- function(obj ,..., .file){
  ini_expressionset_cli(obj, ..., .prefix="M")
}


#### S4 GENERICS --------------------------------------------------------------


#' Modify a data set
#'
#' @param dat An R object carrying data
#' @param x An R object carrying modifications
#' @param ... Options.
#'
#' @export
setGeneric("modify", function(dat, x, ...) standardGeneric("modify"))


