test_that("fit 4PL model on generated data", {
  # skip_on_cran()
  # skip_on_os("linux")

  # setting parameters
  # discrimination
  a <- matrix(rep(c(
    1.00, 1.12, 1.45, 1.25, 1.32, 1.38, 1.44, 0.89, 1.15,
    1.30, 1.29, 1.46, 1.16, 1.26, 0.98
  ), 2), ncol = 2)
  # difficulty
  b <- matrix(rep(c(
    1.34, 0.06, 1.62, 0.24, -1.45, -0.10, 1.76, 1.96, -1.53,
    -0.44, -1.67, 1.91, 1.62, 1.79, -0.21
  ), 2), ncol = 2)
  # guessing
  c <- matrix(rep(c(
    0.00, 0.00, 0.00, 0.00, 0.00, 0.17, 0.18, 0.05, 0.10,
    0.11, 0.15, 0.20, 0.21, 0.23, 0.24
  ), 2), ncol = 2)
  # inattention
  d <- matrix(rep(c(
    1.00, 1.00, 1.00, 0.92, 0.87, 1.00, 1.00, 0.88, 0.93,
    0.94, 0.81, 0.98, 0.87, 0.96, 0.85
  ), 2), ncol = 2)

  # introducing DIF in items 5, 8, 11 and 15
  b[5, 2] <- b[5, 2] + 1
  a[8, 2] <- a[8, 2] + 1
  d[11, 2] <- 1
  c[15, 2] <- 0

  # generating data with parameters a, b, c, d
  set.seed(42)
  df <- genNLR(N = 1000, a = a, b = b, c = c, d = d)
  Data <- df[, 1:15]
  group <- df[, 16]

  fit1 <- difNLR(Data, group, focal.name = 1, model = "4PL", type = "all")
  # saveRDS(fit1, file = "tests/testthat/fixtures/fit1_gen.rds")
  fit1_gen <- readRDS(test_path("fixtures", "fit1_gen.rds"))

  expect_s3_class(fit1, "difNLR")
  expect_equal(fit1, fit1_gen)

  set.seed(42)
  sam <- sample(1:1000, 400)
  expect_message(expect_message(fit8a <- difNLR(Data[sam, ], group[sam],
    focal.name = 1, model = "4PL",
    type = "all", initboot = TRUE
  )))
  # saveRDS(fit8a, file = "tests/testthat/fixtures/fit8a_gen.rds")
  fit8a_gen <- readRDS(test_path("fixtures", "fit8a_gen.rds"))
  expect_s3_class(fit8a, "difNLR")
  expect_equal(fit8a, fit8a_gen)

  expect_warning(fit8b <- difNLR(Data[sam, ], group[sam],
    focal.name = 1, model = "4PL",
    type = "all", initboot = FALSE
  ))
  # saveRDS(fit8b, file = "tests/testthat/fixtures/fit8b_gen.rds")
  fit8b_gen <- readRDS(test_path("fixtures", "fit8b_gen.rds"))
  expect_s3_class(fit8b, "difNLR")
  expect_equal(fit8b, fit8b_gen)
})

test_that("fit different models on generated data", {
  # skip_on_cran()
  # skip_on_os("linux")

  # setting parameters
  # discrimination
  a <- matrix(rep(c(
    1.00, 1.12, 1.45, 1.25, 1.32, 1.38, 1.44, 0.89, 1.15,
    1.30, 1.29, 1.46, 1.16, 1.26, 0.98
  ), 2), ncol = 2)
  # difficulty
  b <- matrix(rep(c(
    1.34, 0.06, 1.62, 0.24, -1.45, -0.10, 1.76, 1.96, -1.53,
    -0.44, -1.67, 1.91, 1.62, 1.79, -0.21
  ), 2), ncol = 2)
  # guessing
  c <- matrix(rep(c(
    0.00, 0.00, 0.00, 0.00, 0.00, 0.17, 0.18, 0.05, 0.10,
    0.11, 0.15, 0.20, 0.21, 0.23, 0.24
  ), 2), ncol = 2)
  # inattention
  d <- matrix(rep(c(
    1.00, 1.00, 1.00, 0.92, 0.87, 1.00, 1.00, 0.88, 0.93,
    0.94, 0.81, 0.98, 0.87, 0.96, 0.85
  ), 2), ncol = 2)

  # introducing DIF in items 5, 8, 11 and 15
  b[5, 2] <- b[5, 2] + 1
  a[8, 2] <- a[8, 2] + 1
  d[11, 2] <- 1
  c[15, 2] <- 0

  # generating data with parameters a, b, c, d
  set.seed(42)
  df <- genNLR(N = 1000, a = a, b = b, c = c, d = d)
  Data <- df[, 1:15]
  group <- df[, 16]

  # Rasch model
  fit1 <- difNLR(Data, group, focal.name = 1, model = "Rasch")
  expect_s3_class(fit1, "difNLR")
  expect_equal(fit1$DIFitems, c(5, 8, 11, 12, 15))

  # item-specific model
  model <- c("1PL", rep("2PL", 2), rep("3PL", 2), rep("3PLd", 2), rep("4PL", 8))
  fit2 <- difNLR(Data, group, focal.name = 1, model = model, type = "all")
  expect_s3_class(fit2, "difNLR")
  expect_equal(fit2$DIFitems, c(5, 8, 11, 15))

  # item-specific type
  type <- rep("all", 15)
  type[5] <- "b"
  type[8] <- "a"
  type[11] <- "c"
  type[15] <- "d"
  fit3 <- difNLR(Data, group, focal.name = 1, model = model, type = type)
  expect_s3_class(fit3, "difNLR")
  expect_equal(fit3$DIFitems, 5)

  # item-specific constraints
  constraints <- rep(NA, 15)
  constraints[5] <- "ac"
  constraints[8] <- "bcd"
  constraints[11] <- "abd"
  constraints[15] <- "abc"
  fit4 <- difNLR(Data, group,
    focal.name = 1, model = model,
    constraints = constraints, type = type
  )
  expect_s3_class(fit4, "difNLR")
  expect_equal(fit4$DIFitems, c(5, 8, 11, 15))

  # item purification
  fit9 <- difNLR(Data[, 1:6], group,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", purify = TRUE
  )
  expect_s3_class(fit9, "difNLR")
  expect_equal(dim(fit9$difPur), c(3, 6))
  expect_equal(fit9$DIFitems, 5)
  expect_equal(round(fit9$pval, 3), c(0.144, 0.974, 0.244, 0.507, 0.000, 0.126))

  # anchor items
  fit9b <- difNLR(Data[, 1:6], group,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", anchor = c(1:4, 6)
  )
  fit9c <- difNLR(Data[, 1:6], group,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", anchor = paste0("Item", c(1:4, 6))
  )
  expect_s3_class(fit9b, "difNLR")
  expect_equal(fit9b$DIFitems, 5)
  expect_equal(round(fit9b$pval, 3), c(0.144, 0.974, 0.244, 0.507, 0.000, 0.126))
  expect_equal(fit9b, fit9c)

  # item purification with no DIF items
  fit9d <- difNLR(Data[, -c(5, 8, 11, 15)], group,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", purify = TRUE
  )
  expect_equal(fit9d$DIFitems, "No DIF item detected")

  # multiple comparison adjustments
  fit10 <- difNLR(Data[, 1:6], group,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", p.adjust.method = "holm"
  )
  expect_s3_class(fit10, "difNLR")
  expect_equal(fit10$DIFitems, 5)
  expect_equal(round(fit10$adj.pval, 3), c(1.000, 1.000, 1.000, 0.747, 0.000, 0.137))

  # combining item purification and multiple comparison adjustment
  fit11 <- difNLR(Data[, 1:6], group,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", p.adjust.method = "holm",
    purify = TRUE
  )
  expect_s3_class(fit11, "difNLR")
  expect_equal(fit11$DIFitems, 5)
  expect_equal(round(fit11$adj.pval, 3), c(0.629, 1.000, 0.733, 1.000, 0.000, 0.629))
})

test_that("use different estimation methods on generated data and 4PL model", {
  # skip_on_cran()
  # skip_on_os("linux")

  # setting parameters
  # discrimination
  a <- matrix(rep(c(
    1.00, 1.12, 1.45, 1.25, 1.32, 1.38, 1.44, 0.89, 1.15,
    1.30, 1.29, 1.46, 1.16, 1.26, 0.98
  ), 2), ncol = 2)
  # difficulty
  b <- matrix(rep(c(
    1.34, 0.06, 1.62, 0.24, -1.45, -0.10, 1.76, 1.96, -1.53,
    -0.44, -1.67, 1.91, 1.62, 1.79, -0.21
  ), 2), ncol = 2)
  # guessing
  c <- matrix(rep(c(
    0.00, 0.00, 0.00, 0.00, 0.00, 0.17, 0.18, 0.05, 0.10,
    0.11, 0.15, 0.20, 0.21, 0.23, 0.24
  ), 2), ncol = 2)
  # inattention
  d <- matrix(rep(c(
    1.00, 1.00, 1.00, 0.92, 0.87, 1.00, 1.00, 0.88, 0.93,
    0.94, 0.81, 0.98, 0.87, 0.96, 0.85
  ), 2), ncol = 2)

  # introducing DIF in items 5, 8, 11 and 15
  b[5, 2] <- b[5, 2] + 1
  a[8, 2] <- a[8, 2] + 1
  d[11, 2] <- 1
  c[15, 2] <- 0

  # generating data with parameters a, b, c, d
  set.seed(42)
  df <- genNLR(N = 1000, a = a, b = b, c = c, d = d)
  Data <- df[, 1:15]
  group <- df[, 16]
  match <- as.vector(scale(apply(Data, 1, sum)))

  fit_nls <- difNLR(Data[, 4:5], group, focal.name = 1, model = "4PL", method = "nls", match = match)
  # saveRDS(fit_nls, file = "tests/testthat/fixtures/fit_nls.rds")
  fit_nls_gen <- readRDS(test_path("fixtures", "fit_nls.rds"))
  expect_equal(fit_nls, fit_nls_gen)

  fit_nls_sandwich <- difNLR(Data[, 4:5], group, focal.name = 1, model = "4PL", method = "nls", match = match, sandwich = TRUE)
  # saveRDS(fit_nls_sandwich, file = "tests/testthat/fixtures/fit_nls_sandwich.rds")
  fit_nls_sandwich_gen <- readRDS(test_path("fixtures", "fit_nls_sandwich.rds"))
  expect_equal(fit_nls_sandwich, fit_nls_sandwich_gen)

  fit_mle <- difNLR(Data[, 4:5], group, focal.name = 1, model = "4PL", method = "mle", match = match)
  fit_likelihood <- difNLR(Data[, 4:5], group, focal.name = 1, model = "4PL", method = "likelihood", match = match)
  # saveRDS(fit_mle, file = "tests/testthat/fixtures/fit_mle.rds")
  fit_mle_gen <- readRDS(test_path("fixtures", "fit_mle.rds"))
  expect_equal(fit_mle, fit_mle_gen)
  expect_equal(fit_likelihood, fit_mle_gen)

  fit_plf <- difNLR(Data[, 4:5], group, focal.name = 1, model = "4PL", method = "plf", match = match)
  # saveRDS(fit_plf, file = "tests/testthat/fixtures/fit_plf.rds")
  fit_plf_gen <- readRDS(test_path("fixtures", "fit_plf.rds"))
  expect_equal(fit_plf, fit_plf_gen)

  fit_em <- difNLR(Data[, 4:5], group, focal.name = 1, model = "4PL", method = "em", match = match)
  # saveRDS(fit_em, file = "tests/testthat/fixtures/fit_em.rds")
  fit_em_gen <- readRDS(test_path("fixtures", "fit_em.rds"))
  expect_equal(fit_em, fit_em_gen)

  fit_irls <- difNLR(Data[, 4:5], group, focal.name = 1, model = "2PL", method = "irls", match = match)
  # saveRDS(fit_irls, file = "tests/testthat/fixtures/fit_irls.rds")
  fit_irls_gen <- readRDS(test_path("fixtures", "fit_irls.rds"))
  expect_equal(fit_irls, fit_irls_gen)
})

test_that("fit 3PL model on GMAT data", {
  # skip_on_cran()
  # skip_on_os("linux")

  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # testing both DIF effects using likelihood-ratio test and
  # 3PL model with fixed guessing for groups
  fit1 <- difNLR(Data, group, focal.name = 1, model = "3PLcg")
  # saveRDS(fit1, file = "tests/testthat/fixtures/fit1_GMAT.rds")
  fit1_GMAT <- readRDS(test_path("fixtures", "fit1_GMAT.rds"))

  expect_s3_class(fit1, "difNLR")
  expect_equal(fit1, fit1_GMAT)
})

test_that("fit 3PL model on GMAT data with different tests", {
  # skip_on_cran()
  # skip_on_os("linux")

  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # likelihood-ratio test
  fit_LR <- difNLR(Data, group, focal.name = 1, model = "3PLc", test = "LR")
  expect_s3_class(fit_LR, "difNLR")
  expect_equal(fit_LR$DIFitems, c(1, 2, 7, 13))

  # Wald test - odd results, TODO: check one more time
  fit_W <- difNLR(Data, group, focal.name = 1, model = "3PLc", test = "W")
  expect_s3_class(fit_W, "difNLR")
  expect_equal(fit_W$DIFitems, c(1, 2, 5, 12, 13, 14, 20))

  # F-test
  fit_F <- difNLR(Data, group, focal.name = 1, model = "3PLc", test = "F")
  expect_s3_class(fit_F, "difNLR")
  expect_equal(fit_F$DIFitems, c(1, 2, 7, 13))
})

test_that("coef.difNLR works on GMAT and 3PLcg model", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # testing both DIF effects using likelihood-ratio test and
  # 3PL model with fixed guessing for groups
  fit1 <- difNLR(Data, group, focal.name = 1, model = "3PLcg")

  # estimated parameters
  # coef_fit1_GMAT <- coef(fit1)
  # saveRDS(coef_fit1_GMAT, file = "tests/testthat/fixtures/coef_fit1_GMAT.rds")
  coef_fit1_GMAT <- readRDS(test_path("fixtures", "coef_fit1_GMAT.rds"))
  expect_equal(coef(fit1), coef_fit1_GMAT)

  # includes standard errors
  # coef_fit1_GMAT_SE <- coef(fit1, SE = TRUE)
  # saveRDS(coef_fit1_GMAT_SE, file = "tests/testthat/fixtures/coef_fit1_GMAT_SE.rds")
  coef_fit1_GMAT_SE <- readRDS(test_path("fixtures", "coef_fit1_GMAT_SE.rds"))
  expect_equal(coef(fit1, SE = TRUE), coef_fit1_GMAT_SE)

  # includes standard errors and simplifies to matrix
  # coef_fit1_GMAT_SE_simplify <- coef(fit1, SE = TRUE, simplify = TRUE)
  # saveRDS(coef_fit1_GMAT_SE_simplify, file = "tests/testthat/fixtures/coef_fit1_GMAT_SE_simplify.rds")
  coef_fit1_GMAT_SE_simplify <- readRDS(test_path("fixtures", "coef_fit1_GMAT_SE_simplify.rds"))
  expect_equal(coef(fit1, SE = TRUE, simplify = TRUE), coef_fit1_GMAT_SE_simplify)

  # intercept-slope parameterization
  # coef_fit1_GMAT_is <- coef(fit1, IRTpars = FALSE)
  # saveRDS(coef_fit1_GMAT_is, file = "tests/testthat/fixtures/coef_fit1_GMAT_is.rds")
  coef_fit1_GMAT_is <- readRDS(test_path("fixtures", "coef_fit1_GMAT_is.rds"))
  expect_equal(coef(fit1, IRTpars = FALSE), coef_fit1_GMAT_is)

  # intercept-slope parameterization, simplifies to matrix, turn off confidence intervals
  # coef_fit1_GMAT_is_simplify_noCI <- coef(fit1, IRTpars = FALSE, simplify = TRUE, CI = 0)
  # saveRDS(coef_fit1_GMAT_is_simplify_noCI, file = "tests/testthat/fixtures/coef_fit1_GMAT_is_simplify_noCI.rds")
  coef_fit1_GMAT_is_simplify_noCI <- readRDS(test_path("fixtures", "coef_fit1_GMAT_is_simplify_noCI.rds"))
  expect_equal(coef(fit1, IRTpars = FALSE, simplify = TRUE, CI = 0), coef_fit1_GMAT_is_simplify_noCI)

  # for DIF items only
  # coef_fit1_GMAT_DIF_is_simplify_noCI <- coef(fit1, item = fit1$DIFitems, IRTpars = FALSE, simplify = TRUE, CI = 0)
  # saveRDS(coef_fit1_GMAT_DIF_is_simplify_noCI, file = "tests/testthat/fixtures/coef_fit1_GMAT_DIF_is_simplify_noCI.rds")
  coef_fit1_GMAT_DIF_is_simplify_noCI <- readRDS(test_path("fixtures", "coef_fit1_GMAT_DIF_is_simplify_noCI.rds"))
  expect_equal(coef(fit1, item = fit1$DIFitems, IRTpars = FALSE, simplify = TRUE, CI = 0), coef_fit1_GMAT_DIF_is_simplify_noCI)
})

test_that("coef.difNLR works on GMAT and 2PL model", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  fit1 <- difNLR(Data, group, focal.name = 1, model = "2PL", method = "irls")

  # estimated parameters
  # coef_fit1_GMAT_2PL <- coef(fit1)
  # saveRDS(coef_fit1_GMAT_2PL, file = "tests/testthat/fixtures/coef_fit1_GMAT_2PL.rds")
  coef_fit1_GMAT_2PL <- readRDS(test_path("fixtures", "coef_fit1_GMAT_2PL.rds"))
  expect_equal(coef(fit1), coef_fit1_GMAT_2PL)

  # includes standard errors
  # coef_fit1_GMAT_SE_2PL <- coef(fit1, SE = TRUE)
  # saveRDS(coef_fit1_GMAT_SE_2PL, file = "tests/testthat/fixtures/coef_fit1_GMAT_SE_2PL.rds")
  coef_fit1_GMAT_SE_2PL <- readRDS(test_path("fixtures", "coef_fit1_GMAT_SE_2PL.rds"))
  expect_equal(coef(fit1, SE = TRUE), coef_fit1_GMAT_SE_2PL)

  # includes standard errors and simplifies to matrix
  # coef_fit1_GMAT_SE_simplify_2PL <- coef(fit1, SE = TRUE, simplify = TRUE)
  # saveRDS(coef_fit1_GMAT_SE_simplify_2PL, file = "tests/testthat/fixtures/coef_fit1_GMAT_SE_simplify_2PL.rds")
  coef_fit1_GMAT_SE_simplify_2PL <- readRDS(test_path("fixtures", "coef_fit1_GMAT_SE_simplify_2PL.rds"))
  expect_equal(coef(fit1, SE = TRUE, simplify = TRUE), coef_fit1_GMAT_SE_simplify_2PL)

  # intercept-slope parameterization
  # coef_fit1_GMAT_is_2PL <- coef(fit1, IRTpars = FALSE)
  # saveRDS(coef_fit1_GMAT_is_2PL, file = "tests/testthat/fixtures/coef_fit1_GMAT_is_2PL.rds")
  coef_fit1_GMAT_is_2PL <- readRDS(test_path("fixtures", "coef_fit1_GMAT_is_2PL.rds"))
  expect_equal(coef(fit1, IRTpars = FALSE), coef_fit1_GMAT_is_2PL)

  # intercept-slope parameterization, simplifies to matrix, turn off confidence intervals
  # coef_fit1_GMAT_is_simplify_noCI_2PL <- coef(fit1, IRTpars = FALSE, simplify = TRUE, CI = 0)
  # saveRDS(coef_fit1_GMAT_is_simplify_noCI_2PL, file = "tests/testthat/fixtures/coef_fit1_GMAT_is_simplify_noCI_2PL.rds")
  coef_fit1_GMAT_is_simplify_noCI_2PL <- readRDS(test_path("fixtures", "coef_fit1_GMAT_is_simplify_noCI_2PL.rds"))
  expect_equal(coef(fit1, IRTpars = FALSE, simplify = TRUE, CI = 0), coef_fit1_GMAT_is_simplify_noCI_2PL)

  # for DIF items only
  # coef_fit1_GMAT_DIF_is_simplify_noCI_2PL <- coef(fit1, item = fit1$DIFitems, IRTpars = FALSE, simplify = TRUE, CI = 0)
  # saveRDS(coef_fit1_GMAT_DIF_is_simplify_noCI_2PL, file = "tests/testthat/fixtures/coef_fit1_GMAT_DIF_is_simplify_noCI_2PL.rds")
  coef_fit1_GMAT_DIF_is_simplify_noCI_2PL <- readRDS(test_path("fixtures", "coef_fit1_GMAT_DIF_is_simplify_noCI_2PL.rds"))
  expect_equal(coef(fit1, item = fit1$DIFitems, IRTpars = FALSE, simplify = TRUE, CI = 0), coef_fit1_GMAT_DIF_is_simplify_noCI_2PL)
})

test_that("fit different models on LtL data", {
  # skip_on_cran()
  # skip_on_os("linux")

  data(LearningToLearn, package = "ShinyItemAnalysis")
  Data <- LearningToLearn[60:100]
  group <- LearningToLearn[, "track_01"]
  match <- scale(LearningToLearn[, "score_6"])

  expect_message(expect_message(expect_warning(expect_warning(fit1 <- difNLR(Data, group,
    focal.name = 1, model = "Rasch", match = match,
    type = "b"
  )))))

  df <- data.frame(do.call(rbind, fit1$nlrPAR), row.names = colnames(Data))
  expect_identical(coef(fit1, simplify = TRUE, CI = 0, IRTpars = FALSE), df)
})

test_that("checking inputs GMAT and 3PL model", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # different dimensions
  expect_error(difNLR(Data, group[-c(1:3)], focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data, group, match = group[-c(1:3)], focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data[1:1999, 1], group, focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data = rep(NA, 2000), group, focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data = c(Data[1:1000, 1], rep(NA, 1000)), group = c(rep(NA, 1000), group[1:1000]), focal.name = 1, model = "3PL"))
  # invalid model
  expect_error(difNLR(Data, group, focal.name = 1, model = "5PL"))
  expect_error(difNLR(Data, group, focal.name = 1))
  # invalid combination of DIF type and model
  expect_error(difNLR(Data, group, focal.name = 1, model = "1PL", type = "nudif"))
  expect_error(difNLR(Data, group, focal.name = 1, model = "Rasch", type = "nudif"))
  # invalid combination of purification and external matching
  expect_error(difNLR(Data, group, match = group, focal.name = 1, model = "3PL", purify = TRUE))
  # invalid test
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", test = "XXX"))
  # invalid significance level
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", alpha = 30))
  # invalid number of nrBo with initboot
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", initboot = TRUE, nrBo = -4))
  # invalid method
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", method = "ffff"))
  # invalid length of type
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", type = c("udif", "nudif")))
  # invalid type
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", type = c("abx")))
  # invalid length of constraints
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", constraints = c("a", "b")))
  # invalid constraints
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", constraints = c("abx")))
  # invalid combination of type and constraints
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", constraints = "a", type = "a"))

  # # invalid combination of method and sandwich
  # expect_warning(difNLR(Data, group, focal.name = 1, model = "3PL", method = "plf", sandwich = TRUE))

  # different ways to input group
  fit1 <- difNLR(GMAT[, 1:20], GMAT$group, focal.name = 1, model = "3PL")
  fit2 <- difNLR(GMAT[, 1:21], "group", focal.name = 1, model = "3PL")
  fit3 <- difNLR(GMAT[, 1:21], 21, focal.name = 1, model = "3PL")
  expect_equal(fit1, fit2)
  expect_equal(fit1, fit3)

  # invalid group
  set.seed(42)
  expect_error(difNLR(Data, rbinom(nrow(Data), 4, prob = runif(nrow(Data))), focal.name = 1, model = "3PL"))
  # invalid dimensions
  expect_error(difNLR(Data[-c(1:4), 1], group, match = GMAT$criterion, focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data[-c(1:4), ], group, focal.name = 1, model = "3PL"))

  # invalid length of model
  expect_error(difNLR(Data, group, focal.name = 1, model = c("3PL", "2PL")))

  # invalid length of start, specifying starting values
  start <- startNLR(Data, group, model = "3PL", match = scale(apply(Data, 1, sum)), parameterization = "is")
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", start = start[1:19]))
  fit4 <- difNLR(Data, group, focal.name = 1, model = "3PL", start = start)
  fit5 <- difNLR(Data, group, focal.name = 1, model = "3PL")
  expect_equal(fit4, fit5)

  start <- startNLR(Data, group, model = "3PL", match = scale(apply(Data, 1, sum)), parameterization = "irt")
  fit6 <- difNLR(Data, group, focal.name = 1, model = "3PL", start = start)
  expect_equal(fit5$DIFitems, fit6$DIFitems)
  names(start[[1]]) <- letters[1:6]
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", start = start))
})

test_that("predict, fitted and residuals works on GMAT and 3PL model", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # testing both DIF effects using likelihood-ratio test and
  # 3PL model with fixed guessing for groups
  fit1 <- difNLR(Data, group, focal.name = 1, model = "3PL")

  # fitted values
  # fitted_fit1_GMAT <- fitted(fit1)
  # saveRDS(fitted_fit1_GMAT, file = "tests/testthat/fixtures/fitted_fit1_GMAT.rds")
  fitted_fit1_GMAT <- readRDS(test_path("fixtures", "fitted_fit1_GMAT.rds"))
  fitted_fit1 <- fitted(fit1)
  expect_equal(fitted_fit1_GMAT, fitted_fit1)

  # residuals
  # residuals_fit1_GMAT <- residuals(fit1)
  # saveRDS(residuals_fit1_GMAT, file = "tests/testthat/fixtures/residuals_fit1_GMAT.rds")
  residuals_fit1_GMAT <- readRDS(test_path("fixtures", "residuals_fit1_GMAT.rds"))
  residuals_fit1 <- residuals(fit1)
  expect_equal(residuals_fit1_GMAT, residuals_fit1)

  # predicted values
  # predict_fit1_GMAT <- predict(fit1)
  # saveRDS(predict_fit1_GMAT, file = "tests/testthat/fixtures/predict_fit1_GMAT.rds")
  predict_fit1_GMAT <- readRDS(test_path("fixtures", "predict_fit1_GMAT.rds"))
  predict_fit1 <- predict(fit1)
  expect_equal(predict_fit1_GMAT, predict_fit1)

  # predicted - new values
  # predict_fit1_new_GMAT <- predict(fit1, match = rep(c(-1, 0, 1), 2), group = rep(c(0, 1), each = 3), item = 1)
  # saveRDS(predict_fit1_new_GMAT, file = "tests/testthat/fixtures/predict_fit1_new_GMAT.rds")
  predict_fit1_new_GMAT <- readRDS(test_path("fixtures", "predict_fit1_new_GMAT.rds"))
  predict_fit1_new <- predict(fit1, match = rep(c(-1, 0, 1), 2), group = rep(c(0, 1), each = 3), item = 1)
  expect_equal(predict_fit1_new_GMAT, predict_fit1_new)

  predict_fit1_new2_GMAT <- predict(fit1, match = -3, group = c(0, 1), item = 2)
  coefs_fit1 <- coef(fit1, item = 2, CI = 0)[[1]]
  expect_equal(
    .gNLR(
      x = -3, g = c(0, 1), a = coefs_fit1[["a"]], b = coefs_fit1[["b"]], c = coefs_fit1[["c"]], d = 1,
      aDif = coefs_fit1[["aDif"]], bDif = coefs_fit1[["bDif"]], cDif = coefs_fit1[["cDif"]], dDif = 0
    ),
    predict_fit1_new2_GMAT[, "prob"]
  )

  frm <- formulaNLR(model = "3PL", parameterization = "is")
  fitestim <- estimNLR(
    y = Data[, 2], match = scale(apply(Data, 1, sum)), group = group,
    formula = frm$M1$formula, method = "nls",
    lower = frm$M1$lower, upper = frm$M1$upper,
    start = startNLR(Data[, 2], group, model = "3PL", match = scale(apply(Data, 1, sum)), parameterization = "is")[[1]]
  )
  coefs_estim <- coef(fitestim)[c("b0", "b1", "b2", "b3")]
  coefs_estim["c"] <- coef(fitestim)["cR"]
  coefs_estim["cDif"] <- coef(fitestim)["cF"] - coef(fitestim)["cR"]
  expect_equal(round(coefs_estim, 4), round(coef(fit1, item = 2, CI = 0, IRTpars = FALSE)[[1]], 4))
})

test_that("S3 methods on generated data", {
  # skip_on_cran()
  # skip_on_os("linux")

  # setting parameters
  # discrimination
  a <- matrix(rep(c(
    1.00, 1.12, 1.45, 1.25, 1.32, 1.38, 1.44, 0.89, 1.15,
    1.30, 1.29, 1.46, 1.16, 1.26, 0.98
  ), 2), ncol = 2)
  # difficulty
  b <- matrix(rep(c(
    1.34, 0.06, 1.62, 0.24, -1.45, -0.10, 1.76, 1.96, -1.53,
    -0.44, -1.67, 1.91, 1.62, 1.79, -0.21
  ), 2), ncol = 2)
  # guessing
  c <- matrix(rep(c(
    0.00, 0.00, 0.00, 0.00, 0.00, 0.17, 0.18, 0.05, 0.10,
    0.11, 0.15, 0.20, 0.21, 0.23, 0.24
  ), 2), ncol = 2)
  # inattention
  d <- matrix(rep(c(
    1.00, 1.00, 1.00, 0.92, 0.87, 1.00, 1.00, 0.88, 0.93,
    0.94, 0.81, 0.98, 0.87, 0.96, 0.85
  ), 2), ncol = 2)

  # introducing DIF in items 5, 8, 11 and 15
  b[5, 2] <- b[5, 2] + 1
  a[8, 2] <- a[8, 2] + 1
  d[11, 2] <- 1
  c[15, 2] <- 0

  # generating data with parameters a, b, c, d
  set.seed(42)
  df <- genNLR(N = 1000, a = a, b = b, c = c, d = d)
  Data <- df[, 1:15]
  group <- df[, 16]

  # testing both DIF effects using likelihood-ratio test and
  # 3PL model with fixed guessing for groups
  fit1 <- difNLR(Data, group, focal.name = 1, model = "3PL")
  fit2 <- difNLR(Data, group, focal.name = 1, model = "2PL", p.adjust.method = "holm")
  fit3 <- difNLR(Data[, -c(5, 8, 11, 15)], group, focal.name = 1, model = "2PL", p.adjust.method = "holm")
  expect_warning(fit4 <- difNLR(Data, group, focal.name = 1, model = c(rep("2PL", 7), rep("3PL", 8)), test = "F", purify = TRUE, nrIter = 1))

  # print
  expect_snapshot(print(fit1))
  expect_snapshot(print(fit2))
  expect_snapshot(print(fit3))
  expect_snapshot(print(fit4))

  # fitted values
  # fitted_fit1_gen <- fitted(fit1)
  # saveRDS(fitted_fit1_gen, file = "tests/testthat/fixtures/fitted_fit1_gen.rds")
  fitted_fit1_gen <- readRDS(test_path("fixtures", "fitted_fit1_gen.rds"))
  fitted_fit1 <- fitted(fit1)
  expect_equal(fitted_fit1_gen, fitted_fit1)
  expect_error(fitted(fit1, item = 16))
  expect_error(fitted(fit1, item = "xxx"))
  expect_error(fitted(fit1, item = FALSE))

  # residuals
  # residuals_fit1_gen <- residuals(fit1)
  # saveRDS(residuals_fit1_gen, file = "tests/testthat/fixtures/residuals_fit1_gen.rds")
  residuals_fit1_gen <- readRDS(test_path("fixtures", "residuals_fit1_gen.rds"))
  residuals_fit1 <- residuals(fit1)
  expect_equal(residuals_fit1_gen, residuals_fit1)
  expect_error(residuals(fit1, item = c(2, 77)))
  expect_error(residuals(fit1, item = "xxx"))
  expect_error(residuals(fit1, item = FALSE))

  # predicted values
  # predict_fit1_gen <- predict(fit1)
  # saveRDS(predict_fit1_gen, file = "tests/testthat/fixtures/predict_fit1_gen.rds")
  predict_fit1_gen <- readRDS(test_path("fixtures", "predict_fit1_gen.rds"))
  predict_fit1 <- predict(fit1)
  expect_equal(predict_fit1_gen, predict_fit1)
  expect_error(predict(fit1, item = c(2, 77)))
  expect_error(predict(fit1, item = "xxx"))
  expect_error(predict(fit1, item = TRUE))

  # predicted - new values
  # predict_fit1_new_gen <- predict(fit1, match = rep(c(-1, 0, 1), 2), group = rep(c(0, 1), each = 3), item = 5)
  # saveRDS(predict_fit1_new_gen, file = "tests/testthat/fixtures/predict_fit1_new_gen.rds")
  predict_fit1_new_gen <- readRDS(test_path("fixtures", "predict_fit1_new_gen.rds"))
  predict_fit1_new <- predict(fit1, match = rep(c(-1, 0, 1), 2), group = rep(c(0, 1), each = 3), item = 5)
  expect_equal(predict_fit1_new_gen, predict_fit1_new)

  # AIC
  # AIC_fit1_gen <- AIC(fit1)
  # saveRDS(AIC_fit1_gen, file = "tests/testthat/fixtures/AIC_fit1_gen.rds")
  AIC_fit1_gen <- readRDS(test_path("fixtures", "AIC_fit1_gen.rds"))
  AIC_fit1 <- AIC(fit1)
  expect_equal(AIC_fit1_gen, AIC_fit1)
  expect_error(AIC(fit1, item = c(2, 77)))
  expect_error(AIC(fit1, item = "xxx"))
  expect_error(AIC(fit1, item = list()))

  # BIC
  # BIC_fit1_gen <- BIC(fit1)
  # saveRDS(BIC_fit1_gen, file = "tests/testthat/fixtures/BIC_fit1_gen.rds")
  BIC_fit1_gen <- readRDS(test_path("fixtures", "BIC_fit1_gen.rds"))
  BIC_fit1 <- BIC(fit1)
  expect_equal(BIC_fit1_gen, BIC_fit1)
  expect_error(BIC(fit1, item = c(5, 2, 88)))
  expect_error(BIC(fit1, item = "xxx"))
  expect_equal(BIC(fit1, item = c(5, 2)), BIC(fit1, item = c(2, 5)[2:1]))
  expect_error(BIC(fit1, item = list("Item1")))

  # logLik
  # logLik_fit1_gen <- logLik(fit1)
  # saveRDS(logLik_fit1_gen, file = "tests/testthat/fixtures/logLik_fit1_gen.rds")
  logLik_fit1_gen <- readRDS(test_path("fixtures", "logLik_fit1_gen.rds"))
  logLik_fit1 <- logLik(fit1)
  expect_equal(logLik_fit1_gen, logLik_fit1)
  expect_error(logLik(fit1, item = "Item55"))
  expect_error(logLik(fit1, item = c(5, 2, 88)))
  expect_error(logLik(fit1, item = list("Item1")))

  # plot(fit1, plot.type = "stat")
  # plot
  plot_fit1 <- plot(fit1, item = 5, group.names = c("A", "B"))[[1]]
  vdiffr::expect_doppelganger("plot_fit1_gen", plot_fit1)
  expect_warning(plot(fit1, item = 5, group.names = c("A", "B", "C"))[[1]])
  expect_warning(plot(fit1, item = 5, group.names = c("A"))[[1]])
  expect_error(plot(fit1, item = 25)[[1]])
  expect_error(plot(fit1, item = list("Item1")))

  plot_fit2 <- plot(fit1, item = 5, group.names = c("A", "B"), draw.CI = TRUE)[[1]]
  vdiffr::expect_doppelganger("plot_fit2_gen", plot_fit2)

  plot_stat <- plot(fit1, plot.type = "stat")
  vdiffr::expect_doppelganger("plot_stat_gen", plot_stat)

  expect_error(plot(fit4, plot.type = "stat"))
  expect_error(plot(fit4, plot.type = "XXX"))
  # vdiffr::manage_cases()
})
