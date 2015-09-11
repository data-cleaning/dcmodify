

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

