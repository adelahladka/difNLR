test_that("difORD - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(Anxiety, package = "ShinyItemAnalysis")
  Data <- Anxiety[, paste0("R", 1:29)] # items
  group <- Anxiety[, "gender"] # group membership variable

  # testing both DIF effects with adjacent category logit model
  expect_snapshot((fit1 <- difORD(Data, group, focal.name = 1, model = "adjacent")))
  # saveRDS(fit1, file = "tests/testthat/fixtures/difORD_fit1.rds")
  fit1_expected <- readRDS(test_path("fixtures", "difORD_fit1.rds"))
  expect_equal(fit1, fit1_expected)

  # graphical devices
  fit1_plot1 <- plot(fit1, item = 6)[[1]]
  vdiffr::expect_doppelganger("difORD_fit1_plot1", fit1_plot1)
  fit1_plot2 <- plot(fit1, item = "R6")[[1]]
  vdiffr::expect_doppelganger("difORD_fit1_plot2", fit1_plot2)
  fit1_plot3 <- plot(fit1, item = "R6", group.names = c("Males", "Females"))[[1]]
  vdiffr::expect_doppelganger("difORD_fit1_plot3", fit1_plot3)

  # estimated parameters
  # saveRDS(coef(fit1), file = "tests/testthat/fixtures/difORD_fit1_coef1.rds")
  fit1_coef1_expected <- readRDS(test_path("fixtures", "difORD_fit1_coef1.rds"))
  expect_equal(coef(fit1), fit1_coef1_expected)
  # saveRDS(coef(fit1, SE = TRUE), file = "tests/testthat/fixtures/difORD_fit1_coef2.rds")
  fit1_coef2_expected <- readRDS(test_path("fixtures", "difORD_fit1_coef2.rds"))
  expect_equal(coef(fit1, SE = TRUE), fit1_coef2_expected) # with SE
  # saveRDS(coef(fit1, SE = TRUE, simplify = TRUE), file = "tests/testthat/fixtures/difORD_fit1_coef3.rds")
  fit1_coef3_expected <- readRDS(test_path("fixtures", "difORD_fit1_coef3.rds"))
  expect_equal(coef(fit1, SE = TRUE, simplify = TRUE), fit1_coef3_expected) # with SE, simplified

  # AIC, BIC, log-likelihood
  expect_snapshot(AIC(fit1))
  expect_snapshot(BIC(fit1))
  expect_snapshot(logLik(fit1))

  # AIC, BIC, log-likelihood for the first item
  expect_snapshot(AIC(fit1, item = 1))
  expect_snapshot(BIC(fit1, item = 1))
  expect_snapshot(logLik(fit1, item = 1))

  # testing both DIF effects with Benjamini-Hochberg adjustment method
  expect_snapshot((fit2 <- difORD(Data, group, focal.name = 1, model = "adjacent", p.adjust.method = "BH")))
  # saveRDS(fit2, file = "tests/testthat/fixtures/difORD_fit2.rds")
  fit2_expected <- readRDS(test_path("fixtures", "difORD_fit2.rds"))
  expect_equal(fit2, fit2_expected)

  # testing both DIF effects with item purification
  expect_snapshot((fit3 <- difORD(Data, group, focal.name = 1, model = "adjacent", purify = TRUE)))
  # saveRDS(fit3, file = "tests/testthat/fixtures/difORD_fit3.rds")
  fit3_expected <- readRDS(test_path("fixtures", "difORD_fit3.rds"))
  expect_equal(fit3, fit3_expected)

  # testing uniform DIF effects
  expect_snapshot((fit4 <- difORD(Data, group, focal.name = 1, model = "adjacent", type = "udif")))
  # saveRDS(fit4, file = "tests/testthat/fixtures/difORD_fit4.rds")
  fit4_expected <- readRDS(test_path("fixtures", "difORD_fit4.rds"))
  expect_equal(fit4, fit4_expected)

  # testing non-uniform DIF effects
  expect_snapshot((fit5 <- difORD(Data, group, focal.name = 1, model = "adjacent", type = "nudif")))
  # saveRDS(fit5, file = "tests/testthat/fixtures/difORD_fit5.rds")
  fit5_expected <- readRDS(test_path("fixtures", "difORD_fit5.rds"))
  expect_equal(fit5, fit5_expected)

  # testing both DIF effects with total score as matching criterion
  expect_snapshot((fit6 <- difORD(Data, group, focal.name = 1, model = "adjacent", match = "score")))
  # saveRDS(fit6, file = "tests/testthat/fixtures/difORD_fit6.rds")
  fit6_expected <- readRDS(test_path("fixtures", "difORD_fit6.rds"))
  expect_equal(fit6, fit6_expected)

  # testing both DIF effects with cumulative logit model
  expect_snapshot((fit7 <- difORD(Data, group, focal.name = 1, model = "cumulative")))
  # saveRDS(fit7, file = "tests/testthat/fixtures/difORD_fit7.rds")
  fit7_expected <- readRDS(test_path("fixtures", "difORD_fit7.rds"))
  expect_equal(fit7, fit7_expected)

  # graphical devices
  fit7_plot1 <- plot(fit7, item = 7, plot.type = "cumulative")[[1]]
  vdiffr::expect_doppelganger("difORD_fit7_plot1", fit7_plot1)
  fit7_plot2 <- plot(fit7, item = 7, plot.type = "category")[[1]]
  vdiffr::expect_doppelganger("difORD_fit7_plot2", fit7_plot2)

  # estimated parameters
  # saveRDS(coef(fit7, simplify = TRUE), file = "tests/testthat/fixtures/difORD_fit7_coef.rds")
  fit7_coef_expected <- readRDS(test_path("fixtures", "difORD_fit7_coef.rds"))
  expect_equal(coef(fit7, simplify = TRUE), fit7_coef_expected)
})

test_that("difORD - checking inputs", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(Anxiety, package = "ShinyItemAnalysis")
  Data <- Anxiety[, paste0("R", 1:29)] # items
  group <- Anxiety[, "gender"] # group membership variable

  # different dimensions
  expect_error(difORD(Data, group[-c(1:3)], focal.name = 1))
  expect_error(difORD(Data, group, match = group[-c(1:3)], focal.name = 1))
  expect_error(difORD(Data[1:765, 1:2], group, focal.name = 1))
  expect_error(difORD(Data[1:765, 1], group, focal.name = 1))
  expect_error(difORD(Data, group, focal.name = 1, match = rnorm(765)))
  # too many NAs
  expect_error(difORD(Data = matrix(NA, ncol = 2, nrow = 766), group, focal.name = 1))
  expect_error(difORD(
    Data = cbind(c(Data[1:750, 1], rep(NA, 16)), c(Data[1:750, 2], rep(NA, 16))),
    group = c(rep(NA, 750), group[1:16]), focal.name = 1
  ))
  # invalid model
  expect_error(difORD(Data, group, focal.name = 1, model = "5PL"))
  # invalid type of DIF
  expect_error(difORD(Data, group, focal.name = 1, type = "xxx"))
  # invalid match
  expect_error(difORD(Data, group, focal.name = 1, match = "dscore"))
  # invalid significance level
  expect_error(difORD(Data, group, focal.name = 1, alpha = 30))
  # deprecated parametrization
  expect_warning(difORD(Data, group, focal.name = 1, parametrization = "is"))
  # invalid combination of purification and matching
  expect_error(difORD(Data, group, focal.name = 1, match = Anxiety$zscore, purify = TRUE))

  # different ways to input group
  fit1 <- difORD(Data, group, focal.name = 1)
  fit2 <- difORD(Anxiety[, c("gender", paste0("R", 1:29))], "gender", focal.name = 1)
  fit3 <- difORD(Anxiety[, c("gender", paste0("R", 1:29))], 1, focal.name = 1)
  expect_equal(fit1, fit2)
  expect_equal(fit1, fit3)

  # invalid group
  set.seed(42)
  expect_error(difORD(Data, rbinom(nrow(Data), 4, prob = runif(nrow(Data))), focal.name = 1))
  # invalid dimensions
  expect_error(difORD(Data[-c(1:4), 1:2], group, match = Anxiety$zscore, focal.name = 1))
  expect_error(difORD(Data[-c(1:4), ], group, focal.name = 1))
})

test_that("difORD S3 methods - checking inputs", {
  # skip_on_cran()
  # skip_on_os("linux")

  # loading data
  data(Anxiety, package = "ShinyItemAnalysis")
  Data <- Anxiety[, paste0("R", 1:29)] # items
  group <- Anxiety[, "gender"] # group membership variable

  fit1 <- difORD(Data, group, focal.name = 1)

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
  a <- matrix(rep(runif(5, 0.25, 1), 8), ncol = 8)
  # difficulty
  b <- t(sapply(1:5, function(i) rep(sort(runif(4, -1, 1)), 2)))

  b[1, 5:8] <- b[1, 5:8] + 0.1
  a[2, 5:8] <- a[2, 5:8] - 0.2

  DataORD <- genNLR(N = 1000, itemtype = "ordinal", a = a, b = b)
  expect_snapshot(summary(DataORD))


  expect_snapshot((fit1 <- difORD(DataORD, group = "group", focal.name = 1, model = "cumulative")))
  # saveRDS(fit1, file = "tests/testthat/fixtures/difORD_RJournal_fit1.rds")
  fit1_expected <- readRDS(test_path("fixtures", "difORD_RJournal_fit1.rds"))
  expect_equal(fit1, fit1_expected)

  fit1_plot1 <- plot(fit1, item = "Item1", plot.type = "cumulative")[[1]]
  vdiffr::expect_doppelganger("difORD_RJournal_fit1_plot1", fit1_plot1)
  fit1_plot2 <- plot(fit1, item = "Item1", plot.type = "category")[[1]]
  vdiffr::expect_doppelganger("difORD_RJournal_fit1_plot2", fit1_plot2)

  expect_snapshot((fit2 <- difORD(DataORD, group = 6, focal.name = 1, model = "adjacent")))
  # saveRDS(fit2, file = "tests/testthat/fixtures/difORD_RJournal_fit2.rds")
  fit2_expected <- readRDS(test_path("fixtures", "difORD_RJournal_fit2.rds"))
  expect_equal(fit2, fit2_expected)

  fit2_plot <- plot(fit2, item = fit2$DIFitems)
  vdiffr::expect_doppelganger("difORD_RJournal_fit2_plot1", fit2_plot[[1]])
  vdiffr::expect_doppelganger("difORD_RJournal_fit2_plot2", fit2_plot[[2]])

  expect_snapshot(coef(fit2)[[1]])
  expect_snapshot(coef(fit2, IRTpars = FALSE)[[1]])
})

test_that("testing paper code - R Journal 2020 - LearningToLearn", {
  # skip_on_cran()
  # skip_on_os("linux")

  data(LearningToLearn, package = "ShinyItemAnalysis")
  # nominal data for changes between 6th and 9th grade
  LtL6_change <- LearningToLearn[, c("track", paste0("Item6", LETTERS[1:8], "_changes"))]
  # ordinal data for change between Grade 6 and 9
  LtL6_change_ord <- data.frame(
    track = LtL6_change$track,
    lapply(
      LtL6_change[, -1],
      function(x) factor(ifelse(x == "10", 0, ifelse(x == "01", 2, 1)))
    )
  )
  expect_snapshot(summary(LtL6_change_ord[, 1:4]))
  # standardized total score achieved in Grade 6
  zscore6 <- LearningToLearn$score_6

  expect_snapshot((fitex5 <- difORD(
    Data = LtL6_change_ord, group = "track", focal.name = "AS",
    model = "adjacent", match = zscore6
  )))
  expect_equal(fitex5$DIFitems, c(2, 4, 5))

  plot1 <- plot(fitex5, item = fitex5$DIFitems)
  vdiffr::expect_doppelganger("ddfMLR_RJournal_plot5", plot1[[1]])
  vdiffr::expect_doppelganger("ddfMLR_RJournal_plot6", plot1[[2]])
  vdiffr::expect_doppelganger("ddfMLR_RJournal_plot7", plot1[[3]])
})
