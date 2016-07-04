test_that("payouts pass basic tests", {
  x <- rep(c(1,2,3,4), times = 2)
  p <- c(0, 100, 200, 500)
  expect_equal(findPayout(x, p), rep(p, 2))
  expect_equal(length(findPayout(x, p)), length(x))
  # More payout levels than actual levels is okay
  p2 <- c(0, 100, 200, 500, 1000)
  expect_equal(findPayout(x, p2), rep(p, 2))
})

test_that("payout fails if levels greater than payouts", {
  x <- c(1,2,3,4,5)
  p <- c(0, 100, 200, 500)
  expect_error(findPayout(x, p), "More levels than there are payout grids.")
})

test_that("payout fails if levels less than 1", {
  x <- c(0,1,2,3)
  p <- c(100, 500, 1000, 5000)
  expect_error(findPayout(x,p), "Lowest level must not be less than 1 for payouts.")
})
