test_that("startNLR - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # 3PL model with the same guessing for both groups
  expect_snapshot(startNLR(Data, group, model = "3PLcg"))
  expect_snapshot(startNLR(Data, group, model = "3PLcg", parameterization = "is"))
  # simplified into a single table
  expect_snapshot(startNLR(Data, group, model = "3PLcg", simplify = TRUE))
  expect_snapshot(startNLR(Data, group, model = "3PLcg", parameterization = "is", simplify = TRUE))

  # 2PL model
  expect_snapshot(startNLR(Data, group, model = "2PL"))
  expect_snapshot(startNLR(Data, group, model = "2PL", parameterization = "is"))
  expect_snapshot(startNLR(Data, group, model = "2PL", parameterization = "logistic"))

  # 4PL model with a total score as the matching criterion
  expect_snapshot(startNLR(Data, group, model = "4PL", match = "score"))
  expect_snapshot(startNLR(Data, group, model = "4PL", match = "score", parameterization = "is"))

  # starting values for model specified for each item
  expect_snapshot(startNLR(Data, group,
    model = c(
      rep("1PL", 5), rep("2PL", 5),
      rep("3PL", 5), rep("4PL", 5)
    )
  ))

  # 4PL model with fixed a and c parameters
  expect_snapshot(startNLR(Data, group, model = "4PL", constraints = "ac", simplify = TRUE))
})

test_that("startNLR - checking inputs", {
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
