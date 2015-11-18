

context("modifier")

test_that("syntax check",{
  m <- expression(
    x <- 1
    , if ( x > 0) y <- 1
    , if ( x > 0 ) y <- 1 else z <- 1
    , if( x > 0 ){
      x <- 0
    }
  )

  for ( e in m){
    expect_true(is_modifying(e))
  }  

  m <- expression(
    x = 1
    , if (x > 0) print("hello")
    , if (x > 0) {
      x <- NA
      print("hello")
    }
  )
  for ( e in m){
    expect_false(is_modifying(e))
  }
   
})


test_that("no-crash test",{
  capture.output(modifier(if(x>0)x<-1))
})


test_that("macros work",{
  m <- modifier(lim := 10, if (y > lim) y <- 0)
  dat <- data.frame(y=c(9,11))
  expect_equal(modify(dat,m),data.frame(y=c(9,0)))
})

test_that("non/sequential execution is supported",{
  dat <- data.frame(x = 0,y=0)
  m <- modifier(
    if ( x == 0) x <- 1
    ,if ( x == 0) y <- 1
  )
  
  expect_equal(modify(dat,m,sequential=TRUE), data.frame(x=1,y=0))
  expect_equal(modify(dat,m,sequential=FALSE) , data.frame(x=1,y=1))
})


