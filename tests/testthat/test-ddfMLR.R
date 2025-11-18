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
  expect_equal(fit1, fit1_expected, tolerance = 1e-5)

  # graphical devices
  fit1_plot1 <- plot(fit1, item = "Item1", group.names = c("Group 1", "Group 2"))[[1]]
  vdiffr::expect_doppelganger("ddfMLR_fit1_plot1", fit1_plot1)
  fit1_plot2 <- plot(fit1, item = 1)[[1]]
  vdiffr::expect_doppelganger("ddfMLR_fit1_plot2", fit1_plot2)

  # estimated parameters
  # saveRDS(coef(fit1), file = "tests/testthat/fixtures/ddfMLR_fit1_coef1.rds")
  fit1_coef1_expected <- readRDS(test_path("fixtures", "ddfMLR_fit1_coef1.rds"))
  expect_equal(coef(fit1), fit1_coef1_expected, tolerance = 1e-5)
  # saveRDS(coef(fit1, SE = TRUE), file = "tests/testthat/fixtures/ddfMLR_fit1_coef2.rds")
  fit1_coef2_expected <- readRDS(test_path("fixtures", "ddfMLR_fit1_coef2.rds"))
  expect_equal(coef(fit1, SE = TRUE), fit1_coef2_expected, tolerance = 1e-5) # with SE
  # saveRDS(coef(fit1, SE = TRUE, simplify = TRUE), file = "tests/testthat/fixtures/ddfMLR_fit1_coef3.rds")
  fit1_coef3_expected <- readRDS(test_path("fixtures", "ddfMLR_fit1_coef3.rds"))
  expect_equal(coef(fit1, SE = TRUE, simplify = TRUE), fit1_coef3_expected, tolerance = 1e-5) # with SE, simplified

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
  expect_equal(fit2, fit2_expected, tolerance = 1e-5)

  # testing both DDF effects with item purification
  expect_snapshot((fit3 <- ddfMLR(Data, group, focal.name = 1, key, purify = TRUE)))
  # saveRDS(fit3, file = "tests/testthat/fixtures/ddfMLR_fit3.rds")
  fit3_expected <- readRDS(test_path("fixtures", "ddfMLR_fit3.rds"))
  expect_equal(fit3, fit3_expected, tolerance = 1e-5)

  # testing uniform DDF effects
  expect_snapshot((fit4 <- ddfMLR(Data, group, key, focal.name = 1, type = "udif")))
  # saveRDS(fit4, file = "tests/testthat/fixtures/ddfMLR_fit4.rds")
  fit4_expected <- readRDS(test_path("fixtures", "ddfMLR_fit4.rds"))
  expect_equal(fit4, fit4_expected, tolerance = 1e-5)

  # testing non-uniform DDF effects
  expect_snapshot((fit5 <- ddfMLR(Data, group, key, focal.name = 1, type = "nudif")))
  # saveRDS(fit5, file = "tests/testthat/fixtures/ddfMLR_fit5.rds")
  fit5_expected <- readRDS(test_path("fixtures", "ddfMLR_fit5.rds"))
  expect_equal(fit5, fit5_expected, tolerance = 1e-5)

  # testing both DDF effects with different matching criteria
  expect_snapshot((fit6a <- ddfMLR(Data, group, key, focal.name = 1, match = "score")))
  # saveRDS(fit6a, file = "tests/testthat/fixtures/ddfMLR_fit6a.rds")
  fit6a_expected <- readRDS(test_path("fixtures", "ddfMLR_fit6a.rds"))
  expect_equal(fit6a, fit6a_expected, tolerance = 1e-5)
  expect_snapshot((fit6b <- ddfMLR(Data, group, key, focal.name = 1, match = "restscore")))
  # saveRDS(fit6b, file = "tests/testthat/fixtures/ddfMLR_fit6b.rds")
  fit6b_expected <- readRDS(test_path("fixtures", "ddfMLR_fit6b.rds"))
  expect_equal(fit6b, fit6b_expected, tolerance = 1e-5)
  expect_snapshot((fit6c <- ddfMLR(Data, group, key, focal.name = 1, match = "zrestscore")))
  # saveRDS(fit6c, file = "tests/testthat/fixtures/ddfMLR_fit6c.rds")
  fit6c_expected <- readRDS(test_path("fixtures", "ddfMLR_fit6c.rds"))
  expect_equal(fit6c, fit6c_expected, tolerance = 1e-5)
  match <- rowSums(GMAT[, 1:20])
  expect_snapshot((fit6d <- ddfMLR(Data, group, key, focal.name = 1, match = match)))
  match <- replicate(ncol(Data), match)
  expect_snapshot((fit6e <- ddfMLR(Data, group, key, focal.name = 1, match = match)))
  match <- as.data.frame(match)
  expect_snapshot((fit6f <- ddfMLR(Data, group, key, focal.name = 1, match = match)))
  expect_equal(fit6a[!names(fit6a) %in% c("match", "match.name")], fit6d[!names(fit6d) %in% c("match", "match.name")], tolerance = 1e-5)
  expect_equal(fit6a[!names(fit6a) %in% c("match", "match.name")], fit6e[!names(fit6e) %in% c("match", "match.name")], tolerance = 1e-5)
  expect_equal(fit6a[!names(fit6a) %in% c("match", "match.name")], fit6f[!names(fit6f) %in% c("match", "match.name")], tolerance = 1e-5)
})

test_that("ddfMLR - checking inputs", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMATtest, GMATkey)
  Data <- GMATtest[, 1:20] # items
  group <- GMATtest[, "group"] # group membership variable
  key <- GMATkey # correct answers

  # deprecated arguments
  expect_warning(ddfMLR(Data, group, key, focal.name = 1, parametrization = "xyz"))

  # different dimensions
  expect_error(ddfMLR(Data, group[-c(1:3)], key, focal.name = 1))
  expect_error(ddfMLR(Data, group, key, match = group[-c(1:3)], focal.name = 1))
  expect_error(ddfMLR(Data[1:1999, 1:2], group, key, focal.name = 1))
  expect_error(ddfMLR(Data[1:1999, 1], group, key, focal.name = 1))
  expect_error(ddfMLR(Data, group, key[-1], focal.name = 1))
  expect_error(ddfMLR(Data[, 1], group, key, focal.name = 1)) # TODO: this should not be an error?
  # too many NAs
  expect_error(ddfMLR(Data = matrix(NA, ncol = 2, nrow = 2000), group, key[1:2], focal.name = 1))
  expect_error(ddfMLR(
    Data = cbind(c(Data[1:1000, 1], rep(NA, 1000)), c(Data[1:1000, 2], rep(NA, 1000))),
    group = c(rep(NA, 1000), group[1001:2000]), key[1:2], focal.name = 1
  ))
  # removing NAs
  DataNA <- Data
  DataNA[1:10, 3] <- NA
  expect_warning(ddfMLR(Data = DataNA, group, key, focal.name = 1))

  # invalid focal.name
  expect_error(ddfMLR(Data, group, key, focal.name = "xxx"))
  # invalid type of DIF
  expect_error(ddfMLR(Data, group, key, focal.name = 1, type = "xxx"))
  # invalid match
  expect_error(ddfMLR(Data, group, key, focal.name = 1, match = "dscore"))
  # invalid significance level
  expect_error(ddfMLR(Data, group, key, focal.name = 1, alpha = 30))
  # invalid combination of matching and purification
  expect_error(ddfMLR(Data, group, key, focal.name = 1, purify = TRUE, match = GMATtest$criterion))
  # invalid combination of purification and anchor
  expect_warning(ddfMLR(Data, group, key, focal.name = 1, purify = TRUE, anchor = 1:20))
  # deprecated parametrization
  expect_warning(ddfMLR(Data, group, key, focal.name = 1, parametrization = "is"))
  # invalid nrIter argument
  expect_error(ddfMLR(Data, group, key, focal.name = 1, purify = TRUE, nrIter = -10))
  # invalid p.adjust.method
  expect_error(ddfMLR(Data, group, key, focal.name = 1, p.adjust.method = "xyz"))

  # different ways to input group
  fit1 <- ddfMLR(Data, group, key, focal.name = 1)
  fit2 <- ddfMLR(GMATtest[, c("group", paste0("Item", 1:20))], "group", key, focal.name = 1)
  fit3 <- ddfMLR(GMATtest[, c("group", paste0("Item", 1:20))], 1, key, focal.name = 1)
  expect_equal(fit1, fit2, tolerance = 1e-5)
  expect_equal(fit1, fit3, tolerance = 1e-5)

  # different ways to input key
  fit4 <- ddfMLR(Data, group, as.data.frame(key), focal.name = 1)
  expect_equal(fit1, fit4, tolerance = 1e-5)
  # NA values in key
  expect_error(ddfMLR(Data, group, c(NA, key[-1]), focal.name = 1))

  # invalid group
  set.seed(42)
  expect_error(ddfMLR(Data, rbinom(nrow(Data), 4, prob = runif(nrow(Data))), key, focal.name = 1))
})

test_that("ddfMLR - other examples", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMATtest, GMATkey)
  Data <- GMATtest[, 1:20] # items
  group <- GMATtest[, "group"] # group membership variable
  key <- GMATkey # correct answers

  # no DDF items
  expect_snapshot((fit7 <- ddfMLR(Data[, -c(1, 2, 9)], group, key[-c(1, 2, 9)], focal.name = 1)))
  # saveRDS(fit7, file = "tests/testthat/fixtures/ddfMLR_fit7.rds")
  fit7_expected <- readRDS(test_path("fixtures", "ddfMLR_fit7.rds"))
  expect_equal(fit7, fit7_expected, tolerance = 1e-5)
  expect_snapshot((ddfMLR(Data[, -c(1, 2, 9)], group, key[-c(1, 2, 9)], focal.name = 1, type = "udif")))
  expect_snapshot((ddfMLR(Data[, -c(1, 2, 9)], group, key[-c(1, 2, 9)], focal.name = 1, type = "nudif")))

  # 1 DIF item, item purification
  expect_snapshot((fit8 <- ddfMLR(Data[, -c(1, 2)], group, key[-c(1, 2)], focal.name = 1)))
  # saveRDS(fit8, file = "tests/testthat/fixtures/ddfMLR_fit8.rds")
  fit8_expected <- readRDS(test_path("fixtures", "ddfMLR_fit8.rds"))
  expect_equal(fit8, fit8_expected, tolerance = 1e-5)

  # no DDF items, item purification
  expect_snapshot(ddfMLR(Data[, -c(1, 2, 9)], group, key[-c(1, 2, 9)], focal.name = 1, purify = TRUE))
})

test_that("ddfMLR S3 methods - checking inputs", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(GMATtest, GMATkey)
  Data <- GMATtest[, 1:20] # items
  group <- GMATtest[, "group"] # group membership variable
  key <- GMATkey # correct answers

  fit1 <- ddfMLR(Data, group, key, focal.name = 1)

  # plot - invalid item argument
  expect_error(plot(fit1, item = "Item25")[[1]])
  expect_error(plot(fit1, item = 33)[[1]])
  expect_error(plot(fit1, item = list("Item2"))[[1]])
  expect_error(plot(fit1, item = c(1, 42))[[1]])
  # plot - invalid length of group.names
  expect_warning(plot(fit1, item = 3, group.names = letters[1:3])[[1]])
  expect_warning(plot(fit1, item = 3, group.names = letters[1])[[1]])

  # coef - invalid SE
  expect_error(coef(fit1, SE = "yes"))
  # coef - invalid simplify
  expect_error(coef(fit1, simplify = "no"))
  # coef - invalid IRTpars
  expect_error(coef(fit1, IRTpars = list()))
  # coef - invalid CI
  expect_error(coef(fit1, CI = 95))

  # logLik - invalid item
  expect_error(logLik(fit1, item = "Item25"))
  expect_error(logLik(fit1, item = 33))
  expect_error(logLik(fit1, item = list("Item2")))
  expect_error(logLik(fit1, item = c(1, 42)))

  # AIC - invalid item
  expect_error(AIC(fit1, item = "Item25"))
  expect_error(AIC(fit1, item = 33))
  expect_error(AIC(fit1, item = list("Item2")))
  expect_error(AIC(fit1, item = c(1, 42)))

  # BIC - invalid item
  expect_error(BIC(fit1, item = "Item25"))
  expect_error(BIC(fit1, item = 33))
  expect_error(BIC(fit1, item = list("Item2")))
  expect_error(BIC(fit1, item = c(1, 42)))

  # predict - invalid item
  expect_error(predict(fit1, item = "Item25"))
  expect_error(predict(fit1, item = 33))
  expect_error(predict(fit1, item = list("Item2")))
  expect_error(predict(fit1, item = c(1, 42)))
  # predict - invalid dimensions
  expect_error(predict(fit1, item = "Item2", group = c(0, 1), match = c(-1, 0, 1)))
})

test_that("testing paper code - R Journal 2020 - generated data", {
  # skip_on_cran()
  # skip_on_os("linux")

  set.seed(42)
  # discrimination
  a <- matrix(rep(runif(30, -2, -0.5), 2), ncol = 6)
  a[1:5, c(3, 6)] <- NA
  # difficulty
  b <- matrix(rep(runif(30, -3, 1), 2), ncol = 6)
  b[1:5, c(3, 6)] <- NA

  a[1, 4] <- a[1, 1] - 1
  a[1, 5] <- a[1, 2] + 1
  b[6, 4] <- b[6, 1] - 1
  b[6, 5] <- b[6, 2] - 1.5

  DataDDF <- genNLR(N = 1000, itemtype = "nominal", a = a, b = b)
  expect_snapshot(head(DataDDF))

  # testing both DIF effects with adjacent category logit model
  expect_snapshot((fit1 <- ddfMLR(DataDDF, group = "group", focal.name = 1, key = rep("A", 10))))
  # saveRDS(fit1, file = "tests/testthat/fixtures/ddfMLR_RJournal_fit1.rds")
  fit1_expected <- readRDS(test_path("fixtures", "ddfMLR_RJournal_fit1.rds"))
  expect_equal(fit1, fit1_expected, tolerance = 1e-5)

  fit1_plot <- plot(fit1, item = fit1$DDFitems, group.names = c("Group 1", "Group 2"))
  vdiffr::expect_doppelganger("ddfMLR_RJournal_fit1_plot1", fit1_plot[[1]])
  vdiffr::expect_doppelganger("ddfMLR_RJournal_fit1_plot2", fit1_plot[[2]])
})

test_that("testing paper code - R Journal 2020 - LearningToLearn", {
  # skip_on_cran()
  # skip_on_os("linux")

  data(LearningToLearn, package = "ShinyItemAnalysis")
  # nominal data for changes between 6th and 9th grade
  LtL6_change <- LearningToLearn[, c("track", paste0("Item6", LETTERS[1:8], "_changes"))]
  expect_snapshot(summary(LtL6_change[, 1:4]))
  # standardized total score achieved in Grade 6
  zscore6 <- LearningToLearn$score_6

  expect_snapshot((fitex4 <- ddfMLR(
    Data = LtL6_change, group = "track", focal.name = "AS",
    key = rep("11", 8), match = zscore6
  )))
  expect_equal(fitex4$DDFitems, c(2, 5))

  plot1 <- plot(fitex4, item = fitex4$DDFitems)
  vdiffr::expect_doppelganger("ddfMLR_RJournal_plot3", plot1[[1]])
  vdiffr::expect_doppelganger("ddfMLR_RJournal_plot4", plot1[[2]])
})
