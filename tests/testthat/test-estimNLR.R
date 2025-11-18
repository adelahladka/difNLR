test_that("estimNLR - examples at help page", {
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
  expect_snapshot((fit_nls <- estimNLR(
    y = y, match = match, group = group,
    formula = M$M1$formula, method = "nls",
    lower = M$M1$lower, upper = M$M1$upper, start = start
  )))
  # saveRDS(fit_nls, file = "tests/testthat/fixtures/estimNLR_fit_nls.rds")
  fit_nls_expected <- readRDS(test_path("fixtures", "estimNLR_fit_nls.rds"))
  expect_equal(fit_nls, fit_nls_expected, tolerance = 1e-3)
  expect_snapshot(coef(fit_nls))
  expect_snapshot(logLik(fit_nls))
  expect_snapshot(vcov(fit_nls))
  expect_snapshot(vcov(fit_nls, sandwich = TRUE))
  expect_snapshot(fitted(fit_nls))
  expect_snapshot(residuals(fit_nls))

  # maximum likelihood method
  expect_snapshot((fit_mle <- estimNLR(
    y = y, match = match, group = group,
    formula = M$M1$formula, method = "mle",
    lower = M$M1$lower, upper = M$M1$upper, start = start
  )))
  # saveRDS(fit_mle, file = "tests/testthat/fixtures/estimNLR_fit_mle.rds")
  fit_mle_expected <- readRDS(test_path("fixtures", "estimNLR_fit_mle.rds"))
  expect_equal(fit_mle, fit_mle_expected, tolerance = 1e-3)
  expect_snapshot(coef(fit_mle))
  expect_snapshot(logLik(fit_mle))
  expect_snapshot(vcov(fit_mle))
  expect_snapshot(fitted(fit_mle))
  expect_snapshot(residuals(fit_mle))

  # formula for 3PL model with the same guessing for both groups
  # intercept-slope parameterization
  M <- formulaNLR(model = "3PLcg", type = "both", parameterization = "is")

  # starting values for 3PL model with the same guessing for item 1,
  start <- startNLR(GMAT[, 1:20], group, model = "3PLcg", parameterization = "is")
  start <- start[[1]][M$M1$parameters]

  # EM algorithm
  expect_snapshot((fit_em <- estimNLR(
    y = y, match = match, group = group,
    formula = M$M1$formula, method = "em",
    lower = M$M1$lower, upper = M$M1$upper, start = start
  )))
  # saveRDS(fit_em, file = "tests/testthat/fixtures/estimNLR_fit_em.rds")
  fit_em_expected <- readRDS(test_path("fixtures", "estimNLR_fit_em.rds"))
  expect_equal(fit_em, fit_em_expected, tolerance = 1e-3)
  expect_snapshot(coef(fit_em))
  expect_snapshot(logLik(fit_em))
  expect_snapshot(vcov(fit_em))
  expect_snapshot(fitted(fit_em))
  expect_snapshot(residuals(fit_em))

  # PLF algorithm
  expect_snapshot((fit_plf <- estimNLR(
    y = y, match = match, group = group,
    formula = M$M1$formula, method = "plf",
    lower = M$M1$lower, upper = M$M1$upper, start = start
  )))
  # saveRDS(fit_plf, file = "tests/testthat/fixtures/estimNLR_fit_plf.rds")
  fit_plf_expected <- readRDS(test_path("fixtures", "estimNLR_fit_plf.rds"))
  expect_equal(fit_plf, fit_plf_expected, tolerance = 1e-3)
  expect_snapshot(coef(fit_plf))
  expect_snapshot(logLik(fit_plf))
  expect_snapshot(vcov(fit_plf))
  expect_snapshot(fitted(fit_plf))
  expect_snapshot(residuals(fit_plf))

  # iteratively reweighted least squares for 2PL model
  M <- formulaNLR(model = "2PL", parameterization = "logistic")
  expect_snapshot((fit_irls <- estimNLR(
    y = y, match = match, group = group,
    formula = M$M1$formula, method = "irls"
  )))
  # saveRDS(fit_irls, file = "tests/testthat/fixtures/estimNLR_fit_irls.rds")
  fit_irls_expected <- readRDS(test_path("fixtures", "estimNLR_fit_irls.rds"))
  expect_equal(fit_irls, fit_irls_expected, tolerance = 1e-3)
  expect_snapshot(coef(fit_irls))
  expect_snapshot(logLik(fit_irls))
  expect_snapshot(vcov(fit_irls))
  expect_snapshot(fitted(fit_irls))
  expect_snapshot(residuals(fit_irls))
})
