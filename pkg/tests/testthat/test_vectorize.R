
context("vectorize")

test_that("guarding expressions",{
  expect_equal(
      set_guards( expression(x <- 3)[[1]] )
    , list( expression(x<-3)[[1]] ) 
  )
  
  L <-  set_guards(expression(if(x>0) y <- 0)[[1]])
  expect_equal(length(L),1)
  expect_equivalent(L[[1]], expression(y<-0)[[1]])  
  expect_equal(attr(L[[1]],"guard"), expression(x>0)[[1]])
  
  L <- set_guards(expression(
   if ( x > 0 ){
     y <- 1
   } else {
     z <- 1
   }
  )[[1]])  
  expect_equal(length(L),2)
  expect_equivalent(L[[1]],expression(y<-1)[[1]])
  expect_equivalent(L[[2]],expression(z<-1)[[1]])  
  expect_equal(attr(L[[1]],"guard"),expression(x>0)[[1]])
  expect_equal(attr(L[[2]],"guard"),expression(!(x>0))[[1]])
  
  L <- set_guards(expression(
    if (x > 0 ){
      x <- 2
      y <- 3
    }
  )[[1]])
  expect_equal(length(L),2)
  expect_equivalent(L[[1]],expression(x <- 2)[[1]])
  expect_equivalent(L[[2]],expression(y <- 3)[[1]])
  expect_equal(attr(L[[1]],"guard"),expression(x>0)[[1]])
  expect_equal(attr(L[[1]],"guard"),expression(x>0)[[1]])

  L <- set_guards(expression(
    if ( x > 0 ){
      if (y > 0){
        z <- 1
      }
    }
  )[[1]])
  expect_equivalent(L[[1]],expression(z <- 1)[[1]])
  expect_equal(attr(L[[1]],"guard"),expression((x>0) & (y > 0))[[1]])
})


