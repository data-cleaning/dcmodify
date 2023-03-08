library(dcmodify)
library(lumberjack)

logfile <- tempfile()

m = modifier(if(Sepal.Length<5.0) Sepal.Length <- 42, if(Petal.Width<0.3) Petal.Width <- 42)

logger = simple$new(verbose=TRUE)

i2 <- start_log(iris, logger) 
i3 <- modify(i2, m, logger=logger)     
i4 <- dump_log(i3, file=logfile, stop=TRUE) 
expect_equal(nrow(read.csv(logfile)),2)


## missing values are handled
