test_that("levels pass basic tests") {
  x <- seq(1, 100)
  n <- seq(2, 100, by = 10)
  expect_equal(max(findLevel(x,n)), length(n) + 1)
  expect_equal(min(findLevel(x,n)), length(n) - 1)
  expect_equal(length(findLevel(x,n), length(x)))
}
