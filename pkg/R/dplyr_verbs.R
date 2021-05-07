# dplyr has introduced some useful verbs for replacing values
# these can be rewritten into if then else statements
# this code, does not create a dependency on dplyr, but rewrites (whenever
# dplyr is not loaded)
# the following dplyr verbs into if then else statements.

is_case_when <- function(e){
  is.call(e) && e[[1]] == "case_when"
}

# case_when( x > 1 ~  1
#          , x < -1 ~ 2
#          , TRUE ~ 3
#          )
# is written into
# if(x > 1){
#    1
#  } else if (x < -1){
#    2
#  } else {
#    3
#  }
case_when_into_if <- function(e){
  if (!is_case_when(e)){
    return(e)
  }
  fs <- e[-1]

  ifexpr <- Reduce(function(f1,f2){
    cond <- f1[[2]]
    value <- f1[[3]]
    #print(list(f1=f1, f2=f2))

    if (is.null(f2)){
      if (isTRUE(cond)){
        value
      } else {
        bquote(if (.(cond)) .(value))
      }
    } else {
      bquote(if (.(cond)) .(value) else {.(f2)})
    }
  }, fs
  , right = T # build expression from right to left
  , init = NULL # needed to check for "dangling else".
  )
  ifexpr
}

is_na_if <- function(e){
  is.call(e) && e[[1]] == "na_if"
}

na_if_into_if <- function(e){
  if (!is_na_if(e)){
    return(e)
  }
  test_value <- e[[3]]
  value <- e[[2]]
  bquote(if (.(value) == .(test_value)) NA)
}
