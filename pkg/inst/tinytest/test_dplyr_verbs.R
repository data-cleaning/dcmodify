### assignment with case_when

L <- dcmodify:::set_guards(quote(
  z <- case_when( 
    x > 1  ~ 1,
    x < -1 ~ 2,
    TRUE   ~ 3
  )
), dplyr_verbs = TRUE)

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))
expect_equivalent(L[[3]], quote(z <- 3))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1 & x < -1))
expect_equal(attr(L[[3]],"guard"), quote(!x>1 & !x < -1))

# ifelse

L <- dcmodify:::set_guards(quote(
  z <- ifelse(x > 1, 1, 2)
), dplyr_verbs = TRUE)

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1))

# ifelse nested (baaadddd...)

L <- dcmodify:::set_guards(quote(
  z <- ifelse(x > 1, 1, ifelse(x < -1, 2, 3))
), dplyr_verbs = TRUE)

expect_equivalent(L[[1]], quote(z <- 1))
expect_equivalent(L[[2]], quote(z <- 2))
expect_equivalent(L[[3]], quote(z <- 3))

expect_equal(attr(L[[1]],"guard"), quote(x>1))
expect_equal(attr(L[[2]],"guard"), quote(!x>1 & x < -1))
expect_equal(attr(L[[3]],"guard"), quote(!x>1 & !x < -1))

