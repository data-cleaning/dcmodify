
## vectorize

## guarding expressions
expect_equal(
    dcmodify:::set_guards( quote(x <- 3) )
  , list( quote(x<-3)) 
)

L <-  dcmodify:::set_guards(quote(if(x>0) y <- 0))
expect_equal(length(L),1)
expect_equivalent(L[[1]], quote(y<-0))  
expect_equal(attr(L[[1]],"guard"), quote(x>0))

L <- dcmodify:::set_guards(quote(
 if ( x > 0 ){
   y <- 1
 } else {
   z <- 1
 }
))  
expect_equal(length(L),2)
expect_equivalent(L[[1]], quote(y<-1))
expect_equivalent(L[[2]], quote(z<-1))  
expect_equal(attr(L[[1]],"guard"), quote(x>0))
expect_equal(attr(L[[2]],"guard"), quote(!x>0))

L <- dcmodify:::set_guards(quote(
  if (x > 0 ){
    x <- 2
    y <- 3
  }
))
expect_equal(length(L),2)
expect_equivalent(L[[1]], quote(x <- 2))
expect_equivalent(L[[2]], quote(y <- 3))
expect_equal(attr(L[[1]],"guard"), quote(x>0))
expect_equal(attr(L[[2]],"guard"), quote(x>0))

L <- dcmodify:::set_guards(quote(
  if ( x > 0 ){
    if (y > 0){
      z <- 1
    }
  }
))

expect_equivalent(L[[1]], quote(z <- 1))
expect_equal(attr(L[[1]],"guard"), quote(x>0 & y > 0))

L <- dcmodify:::set_guards(quote(
  if ( x > 1 ){
    z <- 1
  } else if (x < -1){
    z <- 2
  } else {
    z <- 3
  }
))

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))
expect_equivalent(L[[3]], quote(z <- 3))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1 & x < -1))

### assigment with if

L <- dcmodify:::set_guards(quote(
  z <- if ( x > 1 ){
    1
  } else if (x < -1){
    2
  } else {
    3
  }
))

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))
expect_equivalent(L[[3]], quote(z <- 3))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1 & x < -1))

### assignment with case_when

L <- dcmodify:::set_guards(quote(
  z <- case_when( 
    x > 1  ~ 1,
    x < -1 ~ 2,
    TRUE   ~ 3
    )
))

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))
expect_equivalent(L[[3]], quote(z <- 3))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1 & x < -1))
expect_equal(attr(L[[3]],"guard"), quote(!x>1 & !x < -1))

# ifelse

L <- dcmodify:::set_guards(quote(
  z <- ifelse(x > 1, 1, 2)
))

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1))

# ifelse nested (baaadddd...)

L <- dcmodify:::set_guards(quote(
  z <- ifelse(x > 1, 1, ifelse(x < -1, 2, 3))
))

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))
expect_equivalent(L[[3]], quote(z <- 3))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1 & x < -1))
expect_equal(attr(L[[3]],"guard"), quote(!x>1 & !x < -1))

