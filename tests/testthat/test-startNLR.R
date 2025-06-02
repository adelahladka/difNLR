test_that("startNLR on GMAT data - checking inputs", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  group <- GMAT[, "group"] # group membership variable

  # formula for 3PL model with the same guessing for both groups,
  # IRT parameterization
  M <- formulaNLR(model = "3PLcg", type = "both", parameterization = "irt")

  # checking inputs
  expect_error(startNLR(Data = GMAT[, 1:20], group = group))
  expect_error(startNLR(Data = GMAT[, 1:20], group = group, model = "6PL"))
  expect_error(startNLR(Data = GMAT[, 1:20], group = group, model = c("2PL", "3PL")))
  expect_error(startNLR(Data = GMAT[, 1:20], group = group, model = "2PL", parameterization = c("irt", "is")))
  expect_error(startNLR(Data = GMAT[, 1:20], group = group, model = "2PL", constraints = c("a", "b")))
  expect_error(startNLR(Data = GMAT[, 1:20], group = group, model = "2PL", match = "dscore"))
})
