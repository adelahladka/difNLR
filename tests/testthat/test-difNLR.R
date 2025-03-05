test_that("fit 3PL model on generated data", {
  skip_on_cran()
  skip_on_os("linux")

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
  expect_message(fit8a <- difNLR(Data[sam, ], group[sam], focal.name = 1, model = "4PL", type = "all", initboot = TRUE))
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
  skip_on_cran()
  skip_on_os("linux")

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
  expect_s3_class(fit9b, "difNLR")
  expect_equal(fit9$DIFitems, 5)
  expect_equal(round(fit9$pval, 3), c(0.144, 0.974, 0.244, 0.507, 0.000, 0.126))

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

test_that("use different estimation methods on generated data", {
  skip_on_cran()
  skip_on_os("linux")

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
  # saveRDS(fit_mle, file = "tests/testthat/fixtures/fit_mle.rds")
  fit_mle_gen <- readRDS(test_path("fixtures", "fit_mle.rds"))
  expect_equal(fit_mle, fit_mle_gen)

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
  skip_on_cran()
  skip_on_os("linux")

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
  skip_on_cran()
  skip_on_os("linux")

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
  skip_on_cran()
  skip_on_os("linux")

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
  skip_on_cran()
  skip_on_os("linux")

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
