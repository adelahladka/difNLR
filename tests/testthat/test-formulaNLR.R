test_that("formulaNLR equivalence of models for IRT parametrization", {
  # 1PL vs 2PL model with constraints on a
  frm1 <- formulaNLR(model = "1PL")
  frm2 <- formulaNLR(model = "2PL", constraints = "a")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 3PLcg vs 3PL model with constraints on c
  frm1 <- formulaNLR(model = "3PLcg")
  frm2 <- formulaNLR(model = "3PL", constraints = "c")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 3PLdg vs 3PL model with constraints on d
  frm1 <- formulaNLR(model = "3PLdg")
  frm2 <- formulaNLR(model = "3PLd", constraints = "d")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 4PLcgdg vs 4PL model with constraints on c and d
  frm1 <- formulaNLR(model = "4PLcgdg")
  frm2 <- formulaNLR(model = "4PL", constraints = "cd")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 2PL all DIF vs 2PL both DIF
  frm1 <- formulaNLR(model = "2PL", type = "all")
  frm2 <- formulaNLR(model = "2PL", type = "both")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 2PL uniform DIF vs 1PL both DIF
  frm1 <- formulaNLR(model = "2PL", type = "udif")
  frm2 <- formulaNLR(model = "1PL", type = "all")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))
})

test_that("formulaNLR equivalence of models for IS parametrization", {
  # 1PL vs 2PL model with constraints on a
  frm1 <- formulaNLR(model = "1PL", parameterization = "is")
  frm2 <- formulaNLR(model = "2PL", constraints = "a", parameterization = "is")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 3PLcg vs 3PL model with constraints on c
  frm1 <- formulaNLR(model = "3PLcg", parameterization = "is")
  frm2 <- formulaNLR(model = "3PL", constraints = "c", parameterization = "is")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 3PLdg vs 3PL model with constraints on d
  frm1 <- formulaNLR(model = "3PLdg", parameterization = "is")
  frm2 <- formulaNLR(model = "3PLd", constraints = "d", parameterization = "is")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 4PLcgdg vs 4PL model with constraints on c and d
  frm1 <- formulaNLR(model = "4PLcgdg", parameterization = "is")
  frm2 <- formulaNLR(model = "4PL", constraints = "cd", parameterization = "is")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 2PL all DIF vs 2PL both DIF
  frm1 <- formulaNLR(model = "2PL", type = "all", parameterization = "is")
  frm2 <- formulaNLR(model = "2PL", type = "both", parameterization = "is")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))

  # 2PL uniform DIF vs 1PL both DIF
  frm1 <- formulaNLR(model = "2PL", type = "udif", parameterization = "is")
  frm2 <- formulaNLR(model = "1PL", type = "all", parameterization = "is")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))
})

test_that("formulaNLR equivalence of models for logistic parametrization", {
  # 2PL with uniform DIF vs 2PL model with constraints on a
  frm1 <- formulaNLR(model = "2PL", type = "udif", parameterization = "logistic")
  frm2 <- formulaNLR(model = "2PL", constraints = "a", parameterization = "logistic")
  expect_equal(frm1[[1]][-1], frm2[[1]][-1])
  expect_equal(frm1[[2]][-1], frm2[[2]][-1])
  expect_equal(paste(frm1[[1]][[1]]), paste(frm2[[1]][[1]]))
  expect_equal(paste(frm1[[2]][[1]]), paste(frm2[[2]][[1]]))
})
