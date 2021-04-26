library(dcmodify)

## modifier

## syntax check
m <- expression(
  x <- 1
  , if ( x > 0) y <- 1
  , if( x > 0 ){
    x <- 0
  } # these are of course "just" assignments...
  , y <- if (x > 0) 1
  , y <- if (x > 0) {1}
  , y <- ifelse(x > 0, 1, 2)
  , y <- case_when(x > 0 ~ 1, TRUE ~ 2)
  , if (x > 0) x <- 10 else y <- 1
  , if (x > 0) x <- 10 else if (x < -1) y <- 1 else z <- 2
)

for ( e in m){
  expect_true(dcmodify:::is_modifying(e))
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
  expect_false(dcmodify:::is_modifying(e))
}
#expect_warning(modifier( if (x > 0) y<- 1 else y<-2  ))
   


## no-crash test
capture.output(modifier(if(x>0)x<-1))


## macros work
m <- modifier(lim := 10, if (y > lim) y <- 0)
dat <- data.frame(y=c(9,11))
expect_equal(modify(dat,m),data.frame(y=c(9,0)))


## selection works
m <- modifier( if (x > 0) x <- 1)
# crash test
z <- m[1]
expect_equal(length(z), 1)



## missing values are handled
m <- modifier(if(x==0) x <- 1)
dat <- data.frame(x=NA)
expect_equal(modify(dat, m), dat)
expect_equal(modify(dat, m, na.condition=TRUE), data.frame(x=1))



## non/sequential execution is supported
dat <- data.frame(x = 0,y=0)
m <- modifier(
  if ( x == 0) x <- 1
  ,if ( x == 0) y <- 1
)

expect_equal(modify(dat,m,sequential=TRUE), data.frame(x=1,y=0))
expect_equal(modify(dat,m,sequential=FALSE) , data.frame(x=1,y=1))



