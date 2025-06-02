test_that("ddfMLR - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMATtest, GMATkey)
  Data <- GMATtest[, 1:20] # items
  group <- GMATtest[, "group"] # group membership variable
  key <- GMATkey # correct answers

  # testing both DIF effects with adjacent category logit model
  expect_snapshot((fit1 <- ddfMLR(Data, group, focal.name = 1, key)))
  # saveRDS(fit1, file = "tests/testthat/fixtures/ddfMLR_fit1.rds")
  fit1_expected <- readRDS(test_path("fixtures", "ddfMLR_fit1.rds"))
  expect_equal(fit1, fit1_expected)

  # graphical devices
  fit1_plot1 <- plot(fit1, item = "Item1", group.names = c("Group 1", "Group 2"))[[1]]
  vdiffr::expect_doppelganger("ddfMLR_fit1_plot1", fit1_plot1)
  fit1_plot2 <- plot(fit1, item = 1)[[1]]
  vdiffr::expect_doppelganger("ddfMLR_fit1_plot2", fit1_plot2)

  # estimated parameters
  # saveRDS(coef(fit1), file = "tests/testthat/fixtures/ddfMLR_fit1_coef1.rds")
  fit1_coef1_expected <- readRDS(test_path("fixtures", "ddfMLR_fit1_coef1.rds"))
  expect_equal(coef(fit1), fit1_coef1_expected)
  # saveRDS(coef(fit1, SE = TRUE), file = "tests/testthat/fixtures/ddfMLR_fit1_coef2.rds")
  fit1_coef2_expected <- readRDS(test_path("fixtures", "ddfMLR_fit1_coef2.rds"))
  expect_equal(coef(fit1, SE = TRUE), fit1_coef2_expected) # with SE
  # saveRDS(coef(fit1, SE = TRUE, simplify = TRUE), file = "tests/testthat/fixtures/ddfMLR_fit1_coef3.rds")
  fit1_coef3_expected <- readRDS(test_path("fixtures", "ddfMLR_fit1_coef3.rds"))
  expect_equal(coef(fit1, SE = TRUE, simplify = TRUE), fit1_coef3_expected) # with SE, simplified

  # AIC, BIC, log-likelihood
  expect_snapshot(AIC(fit1))
  expect_snapshot(BIC(fit1))
  expect_snapshot(logLik(fit1))

  # AIC, BIC, log-likelihood for the first item
  expect_snapshot(AIC(fit1, item = 1))
  expect_snapshot(BIC(fit1, item = 1))
  expect_snapshot(logLik(fit1, item = 1))

  #' # testing both DDF effects with Benjamini-Hochberg adjustment method
  expect_snapshot((fit2 <- ddfMLR(Data, group, focal.name = 1, key, p.adjust.method = "BH")))
  # saveRDS(fit2, file = "tests/testthat/fixtures/ddfMLR_fit2.rds")
  fit2_expected <- readRDS(test_path("fixtures", "ddfMLR_fit2.rds"))
  expect_equal(fit2, fit2_expected)

  # testing both DDF effects with item purification
  expect_snapshot((fit3 <- ddfMLR(Data, group, focal.name = 1, key, purify = TRUE)))
  # saveRDS(fit3, file = "tests/testthat/fixtures/ddfMLR_fit3.rds")
  fit3_expected <- readRDS(test_path("fixtures", "ddfMLR_fit3.rds"))
  expect_equal(fit3, fit3_expected)

  # testing uniform DDF effects
  expect_snapshot((fit4 <- ddfMLR(Data, group, key, focal.name = 1, type = "udif")))
  # saveRDS(fit4, file = "tests/testthat/fixtures/ddfMLR_fit4.rds")
  fit4_expected <- readRDS(test_path("fixtures", "ddfMLR_fit4.rds"))
  expect_equal(fit4, fit4_expected)

  # testing non-uniform DDF effects
  expect_snapshot((fit5 <- ddfMLR(Data, group, key, focal.name = 1, type = "udif")))
  # saveRDS(fit5, file = "tests/testthat/fixtures/ddfMLR_fit5.rds")
  fit5_expected <- readRDS(test_path("fixtures", "ddfMLR_fit5.rds"))
  expect_equal(fit5, fit5_expected)

  # testing both DDF effects with total score as matching criterion
  expect_snapshot((fit6 <- ddfMLR(Data, group, key, focal.name = 1, match = "score")))
  # saveRDS(fit6, file = "tests/testthat/fixtures/ddfMLR_fit6.rds")
  fit6_expected <- readRDS(test_path("fixtures", "ddfMLR_fit6.rds"))
  expect_equal(fit6, fit6_expected)
})
