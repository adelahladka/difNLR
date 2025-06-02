test_that("genNLR - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")

  # seed
  set.seed(123)
  # generating parameters for dichotomous data with DIF, 5 items
  a <- matrix(runif(10, 0.8, 2), ncol = 2)
  b <- matrix(runif(10, -2, 2), ncol = 2)
  c <- matrix(runif(10, 0, 0.25), ncol = 2)
  d <- matrix(runif(10, 0.8, 1), ncol = 2)
  # generating dichotomous data set with 300 observations (150 each group)
  expect_snapshot(genNLR(N = 300, a = a, b = b, c = c, d = d))
  # generating dichotomous data set with 300 observations (150 each group)
  # and different mean and standard deviation for underlying distribution
  expect_snapshot(genNLR(N = 300, a = a, b = b, c = c, d = d, mu = c(1, 0), sigma = c(1, 2)))
  # generating dichotomous data set with 300 observations (250 reference group, 50 focal)
  expect_snapshot(genNLR(N = 300, ratio = 5, a = a, b = b, c = c, d = d))

  # generating parameters for nominal data with DDF, 5 items,
  # each item 3 choices
  a <- matrix(runif(20, 0.8, 2), ncol = 4)
  b <- matrix(runif(20, -2, 2), ncol = 4)
  # generating nominal data set with 300 observations (150 each group)
  expect_snapshot(genNLR(N = 300, itemtype = "nominal", a = a, b = b))
  # generating nominal data set with 300 observations (250 reference group, 50 focal)
  expect_snapshot(genNLR(N = 300, itemtype = "nominal", ratio = 5, a = a, b = b))

  # generating parameters for nominal data with DDF, 5 items,
  # items 1 and 2 have 2 choices, items 3, 4 and 5 have 3 choices
  a <- matrix(runif(20, 0.8, 2), ncol = 4)
  a[1:2, c(2, 4)] <- NA
  b <- matrix(runif(20, -2, 2), ncol = 4)
  b[1:2, c(2, 4)] <- NA
  # generating nominal data set with 300 observations (150 each group)
  expect_snapshot(genNLR(N = 300, itemtype = "nominal", a = a, b = b))
  # generating nominal data set with 300 observations (250 reference group, 50 focal)
  expect_snapshot(genNLR(N = 300, itemtype = "nominal", ratio = 5, a = a, b = b))
})
