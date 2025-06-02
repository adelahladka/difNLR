test_that("S3 methods of estimNLR on GMAT data", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  y <- GMAT[, 1] # item 1
  match <- scale(rowSums(GMAT[, 1:20])) # standardized total score
  group <- GMAT[, "group"] # group membership variable

  # formula for 3PL model with the same guessing for both groups,
  # IRT parameterization
  M <- formulaNLR(model = "3PLcg", type = "both", parameterization = "irt")

  # starting values for 3PL model with the same guessing for item 1
  start <- startNLR(GMAT[, 1:20], group, model = "3PLcg", parameterization = "irt")
  start <- start[[1]][M$M1$parameters]

  # nonlinear least squares
  fit_nls <- estimNLR(
    y = y, match = match, group = group,
    formula = M$M1$formula, method = "nls",
    lower = M$M1$lower, upper = M$M1$upper, start = start
  )
  # saveRDS(fit_nls, file = "tests/testthat/fixtures/estimNLR_fit_nls.rds")
  fit_nls_expected <- readRDS(test_path("fixtures", "estimNLR_fit_nls.rds"))
  expect_equal(fit_nls, fit_nls_expected)

  # print
  expect_snapshot(print(fit_nls))

  # coefs
  # saveRDS(coef(fit_nls), file = "tests/testthat/fixtures/estimNLR_fit_nls_coef.rds")
  fit_nls_coef_expected <- readRDS(test_path("fixtures", "estimNLR_fit_nls_coef.rds"))
  expect_equal(coef(fit_nls), fit_nls_coef_expected)

  # logLik
  # saveRDS(logLik(fit_nls), file = "tests/testthat/fixtures/estimNLR_fit_nls_logLik.rds")
  fit_nls_logLik <- readRDS(test_path("fixtures", "estimNLR_fit_nls_logLik.rds"))
  expect_equal(logLik(fit_nls), fit_nls_logLik)

  # vcov
  # saveRDS(vcov(fit_nls), file = "tests/testthat/fixtures/estimNLR_fit_nls_vcov.rds")
  fit_nls_vcov <- readRDS(test_path("fixtures", "estimNLR_fit_nls_vcov.rds"))
  expect_equal(vcov(fit_nls), fit_nls_vcov)

  # vcov - sandwich
  # saveRDS(vcov(fit_nls, sandwich = TRUE), file = "tests/testthat/fixtures/estimNLR_fit_nls_vcov_sandwich.rds")
  fit_nls_vcov_sandwich <- readRDS(test_path("fixtures", "estimNLR_fit_nls_vcov_sandwich.rds"))
  expect_equal(vcov(fit_nls, sandwich = TRUE), fit_nls_vcov_sandwich)

  # fitted
  # saveRDS(fitted(fit_nls), file = "tests/testthat/fixtures/estimNLR_fit_nls_fitted.rds")
  fit_nls_fitted <- readRDS(test_path("fixtures", "estimNLR_fit_nls_fitted.rds"))
  expect_equal(fitted(fit_nls), fit_nls_fitted)

  # residuals
  # saveRDS(residuals(fit_nls), file = "tests/testthat/fixtures/estimNLR_fit_nls_residuals.rds")
  fit_nls_residuals <- readRDS(test_path("fixtures", "estimNLR_fit_nls_residuals.rds"))
  expect_equal(residuals(fit_nls), fit_nls_residuals)
})
