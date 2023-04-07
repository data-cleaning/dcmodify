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

expect_silent(m <- modifier(.file="rulefile.R"))
expect_equal(length(m), 1)



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

## read rules from data frame works
df <- data.frame(rule= "if(x==0)x<-1", name="", description = "")
m <- modifier(.data = df)
dat <- data.frame(x=0)
mod_dat <- data.frame(x=1)
expect_equal(modify(dat, m), mod_dat)


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


## assignments

m <- modifier(
  if ( x == 0) x <- 1
  ,if ( x > 0){
    y <- 1
    z <- 1
  }
)

asgnmnts <- m$assignments()
guard <- dcmodify:::guard
expect_equal(asgnmnts[[1]], quote(x <- 1))
expect_equal(asgnmnts[[2]], quote(y <- 1))
expect_equal(asgnmnts[[3]], quote(z <- 1))
expect_equal(guard(asgnmnts[[1]]), quote(x == 0))
expect_equal(guard(asgnmnts[[2]]), quote(x > 0))
expect_equal(guard(asgnmnts[[3]]), quote(x > 0))

asgnmnts <- m$assignments(flatten=FALSE)


## aggregates / assignments
m <- modifier(if (is.na(turnover)){turnover <- mean(turnover, na.rm=TRUE)})
d <- data.frame(turnover = c(NA, 1, 3))
d_m <- modify(d, m)
expect_equal(d_m$turnover[1], 2)

m <- modifier(
  if (is.na(turnover)){
    turnover <- mean_by(turnover, by=nace, na.rm=TRUE)}
  )
d <- data.frame(turnover = c(1, NA, 3, 4, NA, 6), nace=c("A", "A", "A", "B", "B", "B"))
d_m <- modify(d, m)
expect_equal(d_m$turnover, c(1,2,3,4,5,6))

