test_that("difNLR - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")
  # skip_on_os("windows")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # testing both DIF effects with adjacent category logit model
  expect_snapshot((fit1 <- difNLR(Data, group, focal.name = 1, model = "3PLcg")))
  # saveRDS(fit1, file = "tests/testthat/fixtures/difNLR_fit1.rds")
  fit1_expected <- readRDS(test_path("fixtures", "difNLR_fit1.rds"))
  expect_equal(fit1, fit1_expected, tolerance = 1e-3)

  # graphical devices
  fit1_plot1 <- plot(fit1, item = "Item1")[[1]]
  vdiffr::expect_doppelganger("difNLR_fit1_plot1", fit1_plot1)
  fit1_plot2 <- plot(fit1, item = 1, group.names = c("Group 1", "Group 2"))[[1]]
  vdiffr::expect_doppelganger("difNLR_fit1_plot2", fit1_plot2)
  fit1_plot3 <- plot(fit1, plot.type = "stat")
  vdiffr::expect_doppelganger("difNLR_fit1_plot3", fit1_plot3)

  # estimated parameters
  # saveRDS(coef(fit1), file = "tests/testthat/fixtures/difNLR_fit1_coef1.rds")
  fit1_coef1_expected <- readRDS(test_path("fixtures", "difNLR_fit1_coef1.rds"))
  expect_equal(coef(fit1), fit1_coef1_expected, tolerance = 1e-3)
  # saveRDS(coef(fit1, SE = TRUE), file = "tests/testthat/fixtures/difNLR_fit1_coef2.rds")
  fit1_coef2_expected <- readRDS(test_path("fixtures", "difNLR_fit1_coef2.rds"))
  expect_equal(coef(fit1, SE = TRUE), fit1_coef2_expected, tolerance = 1e-3) # with SE
  # saveRDS(coef(fit1, SE = TRUE, simplify = TRUE), file = "tests/testthat/fixtures/difNLR_fit1_coef3.rds")
  fit1_coef3_expected <- readRDS(test_path("fixtures", "difNLR_fit1_coef3.rds"))
  expect_equal(coef(fit1, SE = TRUE, simplify = TRUE), fit1_coef3_expected, tolerance = 1e-3) # with SE, simplified

  # fitted values
  fit1_fitted_summary <- summary(fitted(fit1))
  # saveRDS(summary(fitted(fit1)), file = "tests/testthat/fixtures/difNLR_fit1_fitted_summary.rds")
  fit1_fitted_summary_expected <- readRDS(test_path("fixtures", "difNLR_fit1_fitted_summary.rds"))
  expect_equal(fit1_fitted_summary, fit1_fitted_summary_expected, tolerance = 1e-3)
  fit1_fitted_item1 <- fitted(fit1, item = 1)
  # saveRDS(fitted(fit1, item = 1), file = "tests/testthat/fixtures/difNLR_fit1_fitted_item1.rds")
  fit1_fitted_item1_expected <- readRDS(test_path("fixtures", "difNLR_fit1_fitted_item1.rds"))
  expect_equal(fit1_fitted_item1, fit1_fitted_item1_expected, tolerance = 1e-3)

  # residuals
  fit1_residuals_summary <- summary(residuals(fit1))
  # saveRDS(summary(residuals(fit1)), file = "tests/testthat/fixtures/difNLR_fit1_residuals_summary.rds")
  fit1_residuals_summary_expected <- readRDS(test_path("fixtures", "difNLR_fit1_residuals_summary.rds"))
  expect_equal(fit1_residuals_summary, fit1_residuals_summary_expected, tolerance = 1e-3)
  fit1_residuals_item1 <- residuals(fit1, item = 1)
  # saveRDS(residuals(fit1, item = 1), file = "tests/testthat/fixtures/difNLR_fit1_residuals_item1.rds")
  fit1_residuals_item1_expected <- readRDS(test_path("fixtures", "difNLR_fit1_residuals_item1.rds"))
  expect_equal(fit1_residuals_item1, fit1_residuals_item1_expected, tolerance = 1e-3)

  # predicted values
  fit1_predict_summary <- summary(predict(fit1))
  # saveRDS(summary(predict(fit1)), file = "tests/testthat/fixtures/difNLR_fit1_predict_summary.rds")
  fit1_predict_summary_expected <- readRDS(test_path("fixtures", "difNLR_fit1_predict_summary.rds"))
  expect_equal(fit1_predict_summary, fit1_predict_summary_expected, tolerance = 1e-3)
  fit1_predict_item1 <- predict(fit1, item = 1)
  # saveRDS(predict(fit1, item = 1), file = "tests/testthat/fixtures/difNLR_fit1_predict_item1.rds")
  fit1_predict_item1_expected <- readRDS(test_path("fixtures", "difNLR_fit1_predict_item1.rds"))
  expect_equal(fit1_predict_item1, fit1_predict_item1_expected, tolerance = 1e-3)

  expect_equal(predict(fit1, item = 1)$prob, as.vector(fitted(fit1, item = 1)), tolerance = 1e-3)

  # predicted values for new subjects
  fit1_predict_item1_new <- predict(fit1, item = 1, match = 0, group = c(0, 1))
  # saveRDS(predict(fit1, item = 1, match = 0, group = c(0, 1)), file = "tests/testthat/fixtures/difNLR_fit1_predict_item1_new.rds")
  fit1_predict_item1_new_expected <- readRDS(test_path("fixtures", "difNLR_fit1_predict_item1_new.rds"))
  expect_equal(fit1_predict_item1_new, fit1_predict_item1_new_expected, tolerance = 1e-3)

  # AIC, BIC, log-likelihood
  expect_snapshot(AIC(fit1))
  expect_snapshot(BIC(fit1))
  expect_snapshot(logLik(fit1))

  # AIC, BIC, log-likelihood for the first item
  expect_snapshot(AIC(fit1, item = 1))
  expect_snapshot(BIC(fit1, item = 1))
  expect_snapshot(logLik(fit1, item = 1))

  # testing both DIF effects using Wald test and
  # 3PL model with fixed guessing for groups
  expect_snapshot((fit2 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "W")))
  # saveRDS(fit2, file = "tests/testthat/fixtures/difNLR_fit2.rds")
  fit2_expected <- readRDS(test_path("fixtures", "difNLR_fit2.rds"))
  expect_equal(fit2, fit2_expected, tolerance = 1e-3)

  # testing both DIF effects using F test and
  # 3PL model with fixed guessing for groups
  expect_snapshot((fit3 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "F")))
  # saveRDS(fit3, file = "tests/testthat/fixtures/difNLR_fit3.rds")
  fit3_expected <- readRDS(test_path("fixtures", "difNLR_fit3.rds"))
  expect_equal(fit3, fit3_expected, tolerance = 1e-3)

  # testing both DIF effects using
  # 3PL model with fixed guessing for groups and sandwich estimator
  # of the covariance matrices
  expect_snapshot((fit4 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", sandwich = TRUE)))
  # saveRDS(fit4, file = "tests/testthat/fixtures/difNLR_fit4.rds")
  fit4_expected <- readRDS(test_path("fixtures", "difNLR_fit4.rds"))
  expect_equal(fit4, fit4_expected, tolerance = 1e-3)

  # testing both DIF effects using LR test,
  # 3PL model with fixed guessing for groups
  # and Benjamini-Hochberg correction
  expect_snapshot((fit5 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", p.adjust.method = "BH")))
  # saveRDS(fit5, file = "tests/testthat/fixtures/difNLR_fit5.rds")
  fit5_expected <- readRDS(test_path("fixtures", "difNLR_fit5.rds"))
  expect_equal(fit5, fit5_expected, tolerance = 1e-3)

  # testing both DIF effects using LR test,
  # 3PL model with fixed guessing for groups
  # and item purification
  expect_snapshot((fit6 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", purify = TRUE)))
  # saveRDS(fit6, file = "tests/testthat/fixtures/difNLR_fit6.rds")
  fit6_expected <- readRDS(test_path("fixtures", "difNLR_fit6.rds"))
  expect_equal(fit6, fit6_expected, tolerance = 1e-3)

  # testing both DIF effects using 3PL model with fixed guessing for groups
  # and different matching criteria
  expect_snapshot((fit7a <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "score")))
  # saveRDS(fit7a, file = "tests/testthat/fixtures/difNLR_fit7a.rds")
  fit7a_expected <- readRDS(test_path("fixtures", "difNLR_fit7a.rds"))
  expect_equal(fit7a, fit7a_expected, tolerance = 1e-3)
  expect_snapshot((fit7b <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "restscore")))
  # saveRDS(fit7b, file = "tests/testthat/fixtures/difNLR_fit7b.rds")
  fit7b_expected <- readRDS(test_path("fixtures", "difNLR_fit7b.rds"))
  expect_equal(fit7b, fit7b_expected, tolerance = 1e-3)
  expect_snapshot((fit7c <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "zrestscore")))
  # saveRDS(fit7c, file = "tests/testthat/fixtures/difNLR_fit7c.rds")
  fit7c_expected <- readRDS(test_path("fixtures", "difNLR_fit7c.rds"))
  expect_equal(fit7c, fit7c_expected, tolerance = 1e-3)
  match <- rowSums(Data)
  expect_snapshot((fit7d <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match)))
  match <- replicate(ncol(Data), match)
  expect_snapshot((fit7e <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match)))
  match <- as.data.frame(match)
  expect_snapshot((fit7f <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match)))
  expect_equal(fit7a[!names(fit7a) %in% c("match", "match.name")], fit7d[!names(fit7d) %in% c("match", "match.name")], tolerance = 1e-3)
  expect_equal(fit7a[!names(fit7a) %in% c("match", "match.name")], fit7e[!names(fit7e) %in% c("match", "match.name")], tolerance = 1e-3)
  expect_equal(fit7a[!names(fit7a) %in% c("match", "match.name")], fit7f[!names(fit7f) %in% c("match", "match.name")], tolerance = 1e-3)


  # graphical devices - additional
  fit7b_plot1 <- plot(fit7b, item = "Item5")[[1]]
  vdiffr::expect_doppelganger("difNLR_fit7b_plot1", fit7b_plot1)
  fit7c_plot1 <- plot(fit7c, item = "Item5")[[1]]
  vdiffr::expect_doppelganger("difNLR_fit7c_plot1", fit7c_plot1)
  fit7d_plot1 <- plot(fit7d, item = "Item5")[[1]]
  vdiffr::expect_doppelganger("difNLR_fit7d_plot1", fit7d_plot1)

  # testing uniform DIF effects using 4PL model with the same
  # guessing and inattention
  expect_snapshot((fit8 <- difNLR(Data, group, focal.name = 1, model = "4PLcgdg", type = "udif")))
  # saveRDS(fit8, file = "tests/testthat/fixtures/difNLR_fit8.rds")
  fit8_expected <- readRDS(test_path("fixtures", "difNLR_fit8.rds"))
  expect_equal(fit8, fit8_expected, tolerance = 1e-3)

  # testing non-uniform DIF effects using 2PL model
  expect_snapshot((fit9 <- difNLR(Data, group, focal.name = 1, model = "2PL", type = "nudif")))
  # saveRDS(fit9, file = "tests/testthat/fixtures/difNLR_fit9.rds")
  fit9_expected <- readRDS(test_path("fixtures", "difNLR_fit9.rds"))
  expect_equal(fit9, fit9_expected, tolerance = 1e-3)

  # testing difference in parameter b using 4PL model with fixed
  # a and c parameters
  expect_snapshot((fit10 <- difNLR(Data, group, focal.name = 1, model = "4PL", constraints = "ac", type = "b")))
  # saveRDS(fit10, file = "tests/testthat/fixtures/difNLR_fit10.rds")
  fit10_expected <- readRDS(test_path("fixtures", "difNLR_fit10.rds"))
  expect_equal(fit10, fit10_expected, tolerance = 1e-3)

  # testing both DIF effects using LR test,
  # 3PL model with fixed guessing for groups
  # using maximum likelihood estimation with
  # the L-BFGS-B algorithm, the EM algorithm, and the PLF algorithm
  expect_snapshot((fit11 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "mle")))
  # saveRDS(fit11, file = "tests/testthat/fixtures/difNLR_fit11.rds")
  fit11_expected <- readRDS(test_path("fixtures", "difNLR_fit11.rds"))
  expect_equal(fit11, fit11_expected, tolerance = 1e-3)
  # expect_snapshot((fit12 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "em")))
  # # saveRDS(fit12, file = "tests/testthat/fixtures/difNLR_fit12.rds")
  # fit12_expected <- readRDS(test_path("fixtures", "difNLR_fit12.rds"))
  # expect_equal(fit12, fit12_expected, tolerance = 1e-3)
  expect_snapshot((fit13 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "plf")))
  # saveRDS(fit13, file = "tests/testthat/fixtures/difNLR_fit13.rds")
  fit13_expected <- readRDS(test_path("fixtures", "difNLR_fit13.rds"))
  expect_equal(fit13, fit13_expected, tolerance = 1e-3)

  # testing both DIF effects using LR test and 2PL model
  # using maximum likelihood estimation with iteratively reweighted least squares algorithm
  expect_snapshot((fit14 <- difNLR(Data, group, focal.name = 1, model = "2PL", method = "irls")))
  # saveRDS(fit14, file = "tests/testthat/fixtures/difNLR_fit14.rds")
  fit14_expected <- readRDS(test_path("fixtures", "difNLR_fit14.rds"))
  expect_equal(fit14, fit14_expected, tolerance = 1e-3)
})

test_that("difNLR - checking inputs", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # purification
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", purify = list()))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", match = rowSums(Data), purify = TRUE))
  expect_warning(difNLR(Data, group, focal.name = 1, model = "3PL", anchor = 1:15, purify = TRUE))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", purify = TRUE, nrIter = -10))
  # invalid test
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", test = "XXX"))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", test = TRUE))
  # invalid significance level
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", alpha = 30))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", alpha = logical()))
  # p-adjust method
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", p.adjust.method = "holoms"))
  # sandwich
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", sandwich = 42))
  # invalid method
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", method = "ffff"))
  expect_message(difNLR(Data, group, focal.name = 1, model = "3PL", method = "likelihood"))
  # invalid combination of method and sandwich - warning
  expect_warning(difNLR(Data[, 1], group, focal.name = 1, model = "2PL", method = "mle", sandwich = TRUE, match = GMAT$criterion))
  # invalid anchor
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", anchor = c(1:21)))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", anchor = paste0("Item", 6:27)))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", anchor = TRUE))

  # different dimensions
  expect_error(difNLR(Data, group[-c(1:3)], focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data, group, match = group[-c(1:3)], focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data[1:1999, 1], group, focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data = rep(NA, 2000), group, focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data = c(Data[1:1000, 1], rep(NA, 1000)), group = c(rep(NA, 1000), group[1:1000]), focal.name = 1, model = "3PL"))
  # NA removing - warning
  df <- Data[, 1]
  df[1:20] <- NA
  expect_warning(difNLR(df, group, focal.name = 1, model = "2PL", match = GMAT$criterion))
  # invalid focal.name
  expect_error(difNLR(Data, group, focal.name = 5, model = "3PL"))
  # invalid group
  set.seed(42)
  expect_error(difNLR(Data, rbinom(nrow(Data), 4, prob = runif(nrow(Data))), focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data, group = 25, focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data, group = "GROUP", focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data, group = FALSE, focal.name = 1, model = "3PL"))
  # invalid dimensions
  expect_error(difNLR(Data[-c(1:4), 1], group, match = GMAT$criterion, focal.name = 1, model = "3PL"))
  expect_error(difNLR(Data[-c(1:4), ], group, focal.name = 1, model = "3PL"))
  # invalid match
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", match = "XXX"))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", match = FALSE))
  match <- replicate(ncol(Data), rowSums(Data))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", match = match[, 1:19]))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", match = match[1:1000, ]))
  match <- as.data.frame(match)
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", match = match[, 1:19]))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", match = match[1:1000, ]))

  # invalid model
  expect_error(difNLR(Data, group, focal.name = 1, model = "5PL"))
  expect_error(difNLR(Data, group, focal.name = 1, model = 42))
  expect_error(difNLR(Data, group, focal.name = 1))
  expect_error(difNLR(Data, group, focal.name = 1, model = c("3PL", "2PL")))
  # invalid combination of DIF type and model
  expect_error(difNLR(Data, group, focal.name = 1, model = "1PL", type = "nudif"))
  expect_error(difNLR(Data, group, focal.name = 1, model = "Rasch", type = "nudif"))
  # invalid type
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", type = list()))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", type = c("abx")))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", type = c("udif", "nudif")))
  # invalid combination of purification and external matching
  expect_error(difNLR(Data, group, match = group, focal.name = 1, model = "3PL", purify = TRUE))
  # invalid constraints
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", constraints = c("abx")))
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", constraints = c("a", "b")))
  # invalid combination of type and constraints
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", constraints = "a", type = "a"))

  # invalid number of nrBo with initboot
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", initboot = TRUE, nrBo = -4))

  # different ways to input group
  fit1 <- difNLR(GMAT[, 1:20], GMAT$group, focal.name = 1, model = "3PL")
  fit2 <- difNLR(GMAT[, 1:21], "group", focal.name = 1, model = "3PL")
  fit3 <- difNLR(GMAT[, 1:21], 21, focal.name = 1, model = "3PL")
  expect_equal(fit1, fit2, tolerance = 1e-3)
  expect_equal(fit1, fit3, tolerance = 1e-3)

  # invalid length of start, specifying starting values
  match <- as.data.frame(replicate(ncol(Data), scale(apply(Data, 1, sum))))
  start <- startNLR(Data, group, model = "3PL", match = match, parameterization = "is")
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", start = start[1:19]))
  fit4 <- difNLR(Data, group, focal.name = 1, model = "3PL", start = start)
  fit5 <- difNLR(Data, group, focal.name = 1, model = "3PL")
  expect_equal(fit4, fit5, tolerance = 1e-3)

  start <- startNLR(Data, group, model = "3PL", match = match, parameterization = "irt")
  fit6 <- difNLR(Data, group, focal.name = 1, model = "3PL", start = start)
  expect_equal(fit5$DIFitems, fit6$DIFitems, tolerance = 1e-3)
  names(start[[1]]) <- letters[1:6]
  expect_error(difNLR(Data, group, focal.name = 1, model = "3PL", start = start))
})

test_that("difNLR S3 methods - checking inputs", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # testing both DIF effects using likelihood-ratio test and
  # 3PL model with fixed guessing for groups
  fit1 <- difNLR(Data, group, focal.name = 1, model = "3PL")

  # coef
  expect_error(coef(fit1, item = 35))
  expect_error(coef(fit1, item = "Item45"))
  expect_error(coef(fit1, item = list()))
  expect_error(coef(fit1, SE = list()))
  expect_error(coef(fit1, simplify = numeric()))
  expect_error(coef(fit1, IRTpars = integer()))
  expect_error(coef(fit1, CI = 8))

  # fitted values
  expect_error(fitted(fit1, item = 35))
  expect_error(fitted(fit1, item = "xxx"))
  expect_error(fitted(fit1, item = FALSE))

  # residuals
  expect_error(residuals(fit1, item = c(2, 77)))
  expect_error(residuals(fit1, item = "xxx"))
  expect_error(residuals(fit1, item = FALSE))

  # predicted values
  expect_error(predict(fit1, item = c(2, 77)))
  expect_error(predict(fit1, item = "xxx"))
  expect_error(predict(fit1, item = TRUE))
  expect_error(predict(fit1, item = 3, group = c(0, 0, 1), match = c(-3, 2)))
  expect_error(predict(fit1, CI = 5))
  expect_warning(predict(fit1, interval = "xxx"))

  # AIC
  expect_error(AIC(fit1, item = c(2, 77)))
  expect_error(AIC(fit1, item = "xxx"))
  expect_error(AIC(fit1, item = list()))

  # BIC
  expect_error(BIC(fit1, item = c(5, 2, 88)))
  expect_error(BIC(fit1, item = "xxx"))
  expect_error(BIC(fit1, item = list("Item1")))

  # logLik
  expect_error(logLik(fit1, item = "Item55"))
  expect_error(logLik(fit1, item = c(5, 2, 88)))
  expect_error(logLik(fit1, item = list("Item1")))

  # plot
  expect_warning(plot(fit1, item = 5, group.names = c("A", "B", "C"))[[1]])
  expect_warning(plot(fit1, item = 5, group.names = c("A"))[[1]])
  expect_error(plot(fit1, item = 25)[[1]])
  expect_error(plot(fit1, item = "Item25")[[1]])
  expect_error(plot(fit1, item = list("Item1")))
  expect_error(plot(fit1, plot.type = "XXX"))
})

test_that("difNLR S3 methods - further examples", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMAT)
  Data <- GMAT[, 1:20] # items
  group <- GMAT[, "group"] # group membership variable

  # testing both DIF effects using likelihood-ratio test and
  # 3PL model with fixed guessing for groups
  fit1 <- difNLR(Data, group, focal.name = 1, model = "3PL")

  fit1_plot4 <- plot(fit1, item = 1, draw.CI = TRUE, draw.empirical = FALSE)[[1]]
  expect_warning(vdiffr::expect_doppelganger("difNLR_fit1_plot4", fit1_plot4))
})

test_that("testing paper code - R Journal 2020 - generated data", {
  # skip_on_cran()
  # skip_on_os("linux")
  #-----------------------------------------------------------------------------
  # DIF DETECTION AMONG BINARY DATA
  #-----------------------------------------------------------------------------

  #-----------------
  # DATA GENERATION
  #-----------------

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

  # generating dichotomous data with parameters a, b, c, d
  set.seed(42)
  df <- genNLR(N = 1000, a = a, b = b, c = c, d = d)
  expect_snapshot(head(df[, c(1:5, 16)]))
  DataDIF <- df[, 1:15]
  groupDIF <- df[, 16]

  #-----------------
  # BASIC DIF DETECTION
  #-----------------

  # performing DIF detection based on 4PL model
  expect_snapshot((fit1 <- difNLR(DataDIF, groupDIF, focal.name = 1, model = "4PL", type = "all")))
  # saveRDS(fit1, file = "tests/testthat/fixtures/difNLR_RJournal_fit1.rds")
  fit1_expected <- readRDS(test_path("fixtures", "difNLR_RJournal_fit1.rds"))
  expect_equal(fit1, fit1_expected, tolerance = 1e-3)

  # estimated parameters
  expect_snapshot(round(coef(fit1, simplify = TRUE), 3))
  # estimated parameters with SE for item 5
  expect_snapshot(round(coef(fit1, SE = TRUE)[[5]], 3))

  # plot of characteristic curves of DIF items
  fit1_plot1 <- plot(fit1, item = 5)[[1]]
  vdiffr::expect_doppelganger("difNLR_RJournal_fit1_plot1", fit1_plot1)
  fit1_plot2 <- plot(fit1, item = 8)[[1]]
  vdiffr::expect_doppelganger("difNLR_RJournal_fit1_plot2", fit1_plot2)
  fit1_plot3 <- plot(fit1, item = 11)[[1]]
  vdiffr::expect_doppelganger("difNLR_RJournal_fit1_plot3", fit1_plot3)
  fit1_plot4 <- plot(fit1, item = 15)[[1]]
  vdiffr::expect_doppelganger("difNLR_RJournal_fit1_plot4", fit1_plot4)

  # performing DIF detection with item specific models, types and/or constraints
  # item specific model
  model <- c("1PL", rep("2PL", 2), rep("3PL", 2), rep("3PLd", 2), rep("4PL", 8))
  expect_snapshot((fit2 <- difNLR(DataDIF, groupDIF, focal.name = 1, model = model, type = "all")))
  expect_equal(fit2$DIFitems, c(5, 8, 11, 15))

  # item specific type
  type <- rep("all", 15)
  type[5] <- "b"
  type[8] <- "a"
  type[11] <- "c"
  type[15] <- "d"
  expect_snapshot((fit3 <- difNLR(DataDIF, groupDIF, focal.name = 1, model = model, type = type)))
  expect_equal(fit3$DIFitems, 5)

  # item specific constraints
  constraints <- rep(NA, 15)
  constraints[5] <- "ac"
  constraints[8] <- "bcd"
  constraints[11] <- "abd"
  constraints[15] <- "abc"
  expect_snapshot((fit4 <- difNLR(DataDIF, groupDIF,
    focal.name = 1, model = model,
    constraints = constraints, type = type
  )))
  expect_equal(fit4$DIFitems, c(5, 8, 11, 15))

  # fit measures - AIC, BIC
  expect_snapshot((df <- data.frame(
    AIC = c(AIC(fit2), AIC(fit3), AIC(fit4)),
    BIC = c(BIC(fit2), BIC(fit3), BIC(fit4)),
    Fit = paste("fit", rep(2:4, each = 15), sep = ""),
    Item = as.factor(rep(1:15, 3))
  )))
  # library(ggplot2)
  # (plot1 <- ggplot(df, aes(x = Item, y = AIC, col = Fit)) +
  #     geom_point())
  # vdiffr::expect_doppelganger("difNLR_RJournal_plot1", plot1)
  # (plot2 <- ggplot(df, aes(x = Item, y = BIC, col = Fit)) +
  #     geom_point())
  # vdiffr::expect_doppelganger("difNLR_RJournal_plot2", plot2)

  # fit measures are item specific
  expect_snapshot(logLik(fit3, item = 8))
  expect_snapshot(logLik(fit4, item = 8))

  # predicted values
  expect_snapshot(predict(fit1, item = 5, group = c(0, 1), match = 0))


  # FURTHER FEATURES
  #-----------------------------------------------------------------------------

  #-----------------
  # ANCHOR ITEMS
  #-----------------

  fit8a <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all"
  )
  expect_equal(fit8a$DIFitems, c(5, 6))

  fit8b <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", anchor = 1:4
  )
  expect_equal(fit8b$DIFitems, 5)

  #-----------------
  # ITEM PURIFICATION
  #-----------------

  fit9 <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", purify = TRUE
  )

  # purification scheme
  expect_snapshot(fit9$difPur)

  #-----------------
  # MULTIPLE COMPARISON CORRECTIONS
  #-----------------

  # Holm's p-value adjustment
  fit10 <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", p.adjust.method = "holm"
  )
  expect_equal(fit10$DIFitems, 5)


  # combining item purification and Holm's p-value adjustment
  fit11 <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", p.adjust.method = "holm",
    purify = TRUE
  )
  expect_equal(fit11$DIFitems, 5)

  # comparing significance
  expect_equal(round(fit9$pval, 3), c(0.144, 0.974, 0.244, 0.507, 0.000, 0.126))
  expect_equal(round(fit10$adj.pval, 3), c(1.000, 1.000, 1.000, 0.747, 0.000, 0.137))
  expect_equal(round(fit11$adj.pval, 3), c(0.629, 1.000, 0.733, 1.000, 0.000, 0.629))

  #-----------------------------------------------------------------------------
  # TROUBLE SHOOTING
  #-----------------------------------------------------------------------------

  # issues with convergence
  # sampled data
  set.seed(42)
  sam <- sample(1:1000, 420)
  # using re-calculation of starting values
  expect_message(expect_message(fit12a <- difNLR(DataDIF[sam, ], groupDIF[sam],
    focal.name = 1, model = "4PL",
    type = "all"
  )))
  # turn off option of re-calculating starting values
  expect_warning(fit12b <- difNLR(DataDIF[sam, ], groupDIF[sam],
    focal.name = 1, model = "4PL",
    type = "all", initboot = FALSE
  ))

  # with maximum likelihood estimation
  # expect_warning(
  #   expect_message(
  #     expect_message(
  #       expect_message(fit13 <- difNLR(DataDIF[sam, ], groupDIF[sam], focal.name = 1, model = "4PL",
  #                                               type = "all", method = "likelihood")))))

  # issues with item purification
  expect_warning(fit14 <- difNLR(DataDIF[, 1:12], groupDIF,
    focal.name = 1, model = "4PL",
    type = "all", purify = TRUE
  ))
  expect_snapshot(fit14$difPur)
})

test_that("testing paper code - R Journal 2020 - LearningToLearn", {
  # skip_on_cran()
  # skip_on_os("linux")

  data("LearningToLearn", package = "ShinyItemAnalysis")
  # dichotomous items for Grade 6
  LtL6_gr6 <- LearningToLearn[, c("track", paste0("Item6", LETTERS[1:8], "_6"))]
  # standardized total score achieved in Grade 6
  zscore6 <- scale(LearningToLearn$score_6)

  fitex1 <- difNLR(
    Data = LtL6_gr6, group = "track", focal.name = "AS", model = "3PL",
    match = zscore6
  )
  expect_equal(fitex1$DIFitems, 8)

  fitex2 <- difNLR(
    Data = LtL6_gr6[, c(1, 9)], group = "track", focal.name = "AS",
    model = "3PL", type = "c", match = zscore6
  )
  expect_equal(fitex2$DIFitems, 1)

  plot3 <- plot(fitex2, item = fitex2$DIFitems)
  vdiffr::expect_doppelganger("difNLR_RJournal_plot3", plot3)

  # dichotomous items for Grade 9
  LtL6_gr9 <- LearningToLearn[, c("track", paste0("Item6", LETTERS[1:8], "_9"))]

  fitex3 <- difNLR(
    Data = LtL6_gr9, group = "track", focal.name = "AS", model = "3PL",
    match = zscore6
  )
  expect_equal(fitex3$DIFitems, c(1, 2))

  expect_snapshot(predict(
    fitex3,
    match = rep(c(-1, 0, 1), 2),
    group = rep(c(0, 1), each = 3),
    item = 1,
    interval = "confidence"
  ))
})

test_that("testing paper code - R Journal 2020 - special cases (not included)", {
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

  # generating dichotomous data with parameters a, b, c, d
  set.seed(42)
  df <- genNLR(N = 1000, a = a, b = b, c = c, d = d)
  expect_snapshot(head(df[, c(1:5, 16)]))
  DataDIF <- df[, 1:15]
  groupDIF <- df[, 16]

  # issues with convergence
  # sampled data
  set.seed(42)
  sam <- sample(1:1000, 420)
  # turn off option of re-calculating starting values
  expect_warning(fit12b <- difNLR(DataDIF[sam, ], groupDIF[sam],
    focal.name = 1, model = "4PL",
    type = "all", initboot = FALSE
  ))
  # saveRDS(fit12b, file = "tests/testthat/fixtures/difNLR_RJournal_fit12b.rds")
  fit12b_expected <- readRDS(test_path("fixtures", "difNLR_RJournal_fit12b.rds"))
  expect_equal(fit12b, fit12b_expected, tolerance = 1e-3)

  # plots
  expect_error(plot(fit12b, item = 14))
  expect_warning(plot(fit12b, item = c(1, 14)))
  expect_warning(plot(fit12b, plot.type = "stat"))
  expect_snapshot(coef(fit12b, item = 14))
  # fitted
  expect_error(fitted(fit12b, item = 14))
  expect_warning(fitted(fit12b, item = paste0("Item", c(2, 14))))
  # predict
  expect_error(predict(fit12b, item = 14, match = c(0, 1), group = 0))
  expect_warning(predict(fit12b, item = paste0("Item", c(3, 14)), match = c(0, 1), group = 0))
  # fitted
  expect_error(logLik(fit12b, item = 14))
  expect_warning(logLik(fit12b, item = paste0("Item", c(2, 14))))
  # AIC
  expect_error(AIC(fit12b, item = 14))
  expect_warning(AIC(fit12b, item = paste0("Item", c(15, 14))))
  # BIC
  expect_error(AIC(fit12b, item = 14))
  expect_warning(AIC(fit12b, item = paste0("Item", c(4, 6, 10, 14))))
  # residuals
  expect_error(residuals(fit12b, item = 14))
  expect_warning(residuals(fit12b, item = paste0("Item", c(10, 14))))

  # different tests with convergence issues
  expect_warning(fit12c <- difNLR(DataDIF[sam, ], groupDIF[sam],
    focal.name = 1, model = "4PL",
    type = "all", initboot = FALSE, test = "W"
  ))
  expect_snapshot(fit12c)
  expect_warning(plot(fit12c, plot.type = "stat"))

  expect_warning(fit12d <- difNLR(DataDIF[sam, ], groupDIF[sam],
    focal.name = 1, model = "4PL",
    type = "all", initboot = FALSE, test = "LR"
  ))
  expect_snapshot(fit12d)
  expect_warning(plot(fit12d, plot.type = "stat"))

  # warning with the old "likelihood" option
  expect_message(difNLR(DataDIF, groupDIF, focal.name = 1, model = "4PL", method = "likelihood"))

  # anchoring
  fit8b <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", anchor = c(1:4, 6)
  )
  fit8c <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", anchor = paste0("Item", c(1:4, 6))
  )
  expect_equal(fit8c, fit8b, tolerance = 1e-3)
  fit8d <- difNLR(DataDIF[, 1:6], groupDIF,
    focal.name = 1, match = "score",
    model = "4PL", type = "all", purify = TRUE
  )

  fit8c_plot1 <- plot(fit8c, item = 5)
  fit8d_plot1 <- plot(fit8d, item = 5)
  vdiffr::expect_doppelganger("difNLR_RJournal_fit8c_plot1", fit8c_plot1)
  vdiffr::expect_doppelganger("difNLR_RJournal_fit8d_plot1", fit8d_plot1)
  # vdiffr::expect_doppelganger("difNLR_RJournal_fit8c_plot1", fit8d_plot1)
  # vdiffr::expect_doppelganger("difNLR_RJournal_fit8d_plot1", fit8c_plot1)

  # no DIF items
  expect_snapshot((fit15a <- difNLR(DataDIF[, -c(5, 8, 11, 15)], groupDIF,
    focal.name = 1,
    model = "4PL", type = "all"
  )))
  expect_equal(fit15a$DIFitems, "No DIF item detected")
  # no DIF items, purification
  expect_snapshot((fit15b <- difNLR(DataDIF[, -c(5, 8, 11, 15)], groupDIF,
    focal.name = 1,
    model = "4PL", type = "all", purify = TRUE
  )))
  expect_equal(fit15b$DIFitems, "No DIF item detected")
})
