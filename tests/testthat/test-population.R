
test_that("pop objects are working as expected", {
  id <- round(rnorm(100, 1000, sd = 500),0)
  group <- rep(c('a', 'b', 'c'), length.out = 100)
  result <- rnorm(n = 100, 500000, 100000)
  mypop <- pop(id, group, result)

  expect_s3_class(mypop, "pop")
  expect_true(is.pop(mypop))
  expect_false(is.tenure_pop(mypop))
  expect_equal(pop_n(mypop), 100)

})
