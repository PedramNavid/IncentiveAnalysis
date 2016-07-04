test_that("levels pass basic tests", {
  x <- seq(1, 100)
  t <- seq(2, 100, by = 10)
  expect_equal(max(findLevel(x,t)), length(t) + 1)
  expect_equal(min(findLevel(x,t)), 1)
  expect_equal(length(findLevel(x,t)), length(x))

  x <- c(1,2,3,4)
  t <- 3
  e <- c(1,1,2,2)
  expect_equal(findLevel(x, t), e)
})

test_that("NAs are preserved", {
  expect_equal(findLevel(c(1,NA,3), 2), c(1, NA_integer_, 2))
})


