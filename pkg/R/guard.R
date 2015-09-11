

# conjucate two calls
`%&%` <- function(e,f){
  if (is.null(e)) return(f)
  if (is.null(f)) return(e)
  cl <- expression((x) & (y))[[1]]
  cl[[2]][[2]] <- e
  cl[[3]][[2]] <- f
  cl
}

# negate a call
not <- function(e){
  cl <- expression(!(x))[[1]]
  cl[[2]][[2]] <- e
  cl
}

# get guard attribute
guard <- function(x){
  attr(x,"guard")
}

condition <- function(x){
  x[[2]]
}

# calls of the form 
# if (<condition>){
#   <calls1>
# } else {
#   <calls2>
# }
# are translated to a list where each element is a call from {<calls1>, <calls2>}. 
# The elements of the list have a 'guard' attribute, which is a logical expression 
# indicating under which condition a call should be evaluated.
#
set_guards <- function(x){
  cl <- x
  if ( cl[[1]] == "{" ){
    v <- lapply(cl[min(2,length(cl)):length(cl)], function(ex){
      attr(ex,'guard') <- guard(x)
      ex
    })
    return(unlist(lapply(v,set_guards)))
  }
  
  if(cl[[1]] == 'if'){
    v <- cl[[3]] # expression
    attr(v,'guard') <- guard(x) %&% condition(cl)
    v <- list(v)
    if (length(cl)==4){ # there is an 'else'
      w <- cl[[4]]
      attr(w,'guard') <- guard(x) %&% not(condition(cl))
      v <- list(v[[1]],w)
    }
    return(unlist(lapply(v,set_guards)))
  }
  return(list(x)) 
}
  








