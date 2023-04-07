library(dcmodify)
library(lumberjack)

logfile <- tempfile()

m = modifier(if(Sepal.Length<5.0) Sepal.Length <- 42, if(Petal.Width<0.3) Petal.Width <- 42)

logger = simple$new(verbose=FALSE)

i2 <- start_log(iris, logger) 
i3 <- modify(i2, m, logger=logger)     
i4 <- dump_log(i3, file=logfile, stop=TRUE) 
expect_equal(nrow(read.csv(logfile)),2)


# Logging with rules coming from a file

d <- data.frame(id=letters[1:3], x=c(0,NA,2))
m <- modifier(.file="rulefile.R")
logger <- cellwise$new(key="id", verbose=FALSE)
expect_silent(modify(d,m,logger=logger))
logger$dump(file=logfile)

m <- modifier(.data=data.frame(rule="if(is.na(x)) x<-0"))
logger <- cellwise$new(key="id",verbose=FALSE)
expect_silent(modify(d,m,logger=logger))
logger$dump(file=logfile)





