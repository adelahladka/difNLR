#' Nonlinear regression DIF models estimation.
#'
#' @aliases estimNLR
#'
#' @description Estimates parameters of nonlinear regression models for DIF
#'   detection using either nonlinear least squares or maximum likelihood method
#'   with various algorithms.
#'
#' @param y numeric: binary vector of responses.
#' @param match numeric: vector of matching criterion.
#' @param group numeric: binary vector of group membership. \code{"0"} for
#'  reference group, \code{"1"} for focal group.
#' @param formula formula: specification of the model. Can be obtained by the
#'  \code{formulaNLR()} function.
#' @param method character: method used to estimate parameters. The options are
#'  \code{"nls"} for nonlinear least squares (default), \code{"mle"} for
#'  maximum likelihood method, and \code{"irls"} for maximum likelihood estimation
#'  with iteratively reweighted least squares. See \strong{Details}.
#' @param lower numeric: lower bounds for parameters of model specified in \code{formula}.
#' @param upper numeric: upper bounds for parameters of model specified in \code{formula}.
#' @param start numeric: initial parameters. Can be obtained by the
#'  \code{startNLR()} function.
#'
#' @usage estimNLR(y, match, group, formula, method, lower, upper, start)
#'
#' @details
#' Function offers either non-linear least squares estimation via
#' \code{\link[stats]{nls}} function, maximum likelihood method with
#' \code{"L-BFGS-B"} method via \code{\link[stats]{optim}} function,
#' or maximum likelihood method with iteratively reweighted least
#' squares via \code{\link[stats]{glm}} function.
#'
#' @author
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references
#' Hladka, A. (2021). Statistical models for detection of differential item
#' functioning. Dissertation thesis.
#' Faculty of Mathematics and Physics, Charles University.
#'
#' @examples
#' # loading datadata(GMAT)
#' y <- GMAT[, 1] # item 1
#' match <- scale(rowSums(GMAT[, 1:20])) # standardized total score
#' group <- GMAT[, "group"] # group membership variable
#'
#' # formula for 3PL model with the same guessing for both groups
#' M <- formulaNLR(model = "3PLcg", type = "both")
#'
#' # starting values for 3PL model with the same guessing for item 1
#' start <- startNLR(GMAT[, 1:20], group, model = "3PLcg", parameterization = "classic")
#' start <- start[[1]][M$M1$parameters]
#'
#' # nonlinear least squares
#' (fit_nls <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M1$formula, method = "nls",
#'   lower = M$M1$lower, upper = M$M1$upper, start = start
#' ))
#'
#' coef(fit_nls)
#' logLik(fit_nls)
#' vcov(fit_nls)
#' vcov(fit_nls, sandwich = TRUE)
#' fitted(fit_nls)
#' residuals(fit_nls)
#'
#' # maximum likelihood method
#' (fit_mle <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M1$formula, method = "mle",
#'   lower = M$M1$lower, upper = M$M1$upper, start = start
#' ))
#'
#' coef(fit_mle)
#' logLik(fit_mle)
#' vcov(fit_mle)
#' fitted(fit_mle)
#' residuals(fit_mle)
#'
#' # iteratively reweighted least squares for 2PL model
#' M <- formulaNLR(model = "2PL", parameterization = "logistic")
#' (fit_irls <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M1$formula, method = "irls"
#' ))
#'
#' coef(fit_irls)
#' logLik(fit_irls)
#' vcov(fit_irls)
#' fitted(fit_irls)
#' residuals(fit_irls)
#' @keywords DIF
#' @export
estimNLR <- function(y, match, group, formula, method, lower, upper, start) {
  M <- switch(method,
    nls = tryCatch(
      nls(
        formula = formula,
        data = data.frame(y = y, x = match, g = group),
        algorithm = "port",
        start = start,
        lower = lower,
        upper = upper
      ),
      error = function(e) {},
      finally = ""
    ),
    mle = tryCatch(
      .MLE_estimation(
        formula = formula,
        data = data.frame(y = y, x = match, g = group),
        par = start,
        lower = lower,
        upper = upper
      ),
      error = function(e) {},
      finally = ""
    ),
    irls = tryCatch(
      glm(
        formula = formula,
        family = binomial(),
        data = data.frame(y = y, x = match, g = group)
      ),
      error = function(e) {},
      finally = ""
    )
  )
  if (!is.null(M)) {
    class(M) <- c("estimNLR", class(M))
  }
  return(M)
}

#' @rdname estimNLR
#' @export
logLik.estimNLR <- function(object, ...) {
  val <- switch(class(object)[2],
         "nls" = {
           res <- object$m$resid()
           N <- length(res)
           w <- rep(1, N)
           zw <- w == 0
           N <- sum(!zw)
           - N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw))/N + log(sum(res^2)))/2
         },
         "mle" = -object$value,
         "glm" = object$rank - object$aic/2
  )
  attr(val, "df") <- length(coef(object))
  class(val) <- "logLik"
  val
}

#' @rdname estimNLR
#' @export
coef.estimNLR <- function(object, ...) {
  switch(class(object)[2],
         "nls" = object$m$getPars(),
         "mle" = object$par,
         "glm" = object$coefficients
  )
}

#' @rdname estimNLR
#' @export
fitted.estimNLR <- function(object, ...) {
  val <- switch(class(object)[2],
         "nls" = as.vector(object$m$fitted()),
         "mle" = object$fitted,
         "glm" = object$fitted.values
  )
  lab <- "Fitted values"
  attr(val, "label") <- lab
  val
}

#' @rdname estimNLR
#' @export
residuals.estimNLR <- function(object, ...) {
  val <- switch(class(object)[2],
         "nls" = as.vector(object$m$resid()),
         "mle" = object$data$y - object$fitted,
         "glm" = object$residuals
  )
  lab <- "Residuals"
  attr(val, "label") <- lab
  val
}

#' @rdname estimNLR
#' @param x an object of \code{"estimNLR"} class.
#' @export
print.estimNLR <- function(x, ...) {
  formula <- switch(class(x)[2],
                    "nls" = paste(x$m$formula()[2], x$m$formula()[1], x$m$formula()[3]),
                    "mle" = paste(x$formula[2], x$formula[1], x$formula[3]),
                    "glm" = paste0(x$formula[2], " ", x$formula[1], " exp(", x$formula[3], ") / (1 + exp(", x$formula[3], "))")
  )
  cat(
    "Nonlinear regression model \n\n",
    "Model: ", formula, "\n"
  )
  pars <- switch(class(x)[2],
                 "nls" = x$m$getPars(),
                 "mle" = x$par,
                 "glm" = x$coefficients
  )
  alg <- switch(class(x)[2],
                "nls" = "Nonlinear least squares estimation",
                "mle" = "Maximum likelihood estimation using L-BFGS-B algorithm",
                "glm" = "Maximum likelihood estimation using iteratively reweighted least squares algorithm"
  )
  cat("\nCoefficients:\n")
  print(round(pars, 4))
  cat("\n", alg)
}

#' @rdname estimNLR
#' @param object an object of \code{"estimNLR"} class.
#' @param sandwich logical: should be sandwich estimator used for covariance
#'   matrix of parameters when using \code{method = "nls"}? Default is
#'   \code{FALSE}.
#' @param ... other generic parameters for S3 methods.
#' @export
vcov.estimNLR <- function(object, sandwich = FALSE, ...) {
  if (inherits(object, "nls")) {
    if (sandwich) {
      e <- object$m$getEnv()
      y <- e$y
      x <- e$x
      g <- e$g
      cov.object <- .sandwich.cov.nls(formula = object$m$formula(), y, x, g, par = object$m$getPars())
    } else {
      cov.object <- tryCatch(
        {
          sm <- summary(object)
          sm$cov.unscaled * sm$sigma^2
        },
        error = function(e) NULL
      )
    }
  } else {
    if (sandwich) {
      message("Sandwich estimator of covariance is available only for method = 'nls'. ")
    }
    if (inherits(object, "mle")) {
      cov.object <- tryCatch(
        {
          solve(object$hessian)
        },
        error = function(e) NULL
      )
    } else {
      cov.object <- tryCatch(
        {
          vcov(summary(object))
        },
        error = function(e) NULL
      )
    }
  }
  return(cov.object)
}

#' @noRd
.sandwich.cov.nls <- function(formula, y, x, group, par) {
  n <- length(y)
  f <- paste0("(y - ", "(", gsub("y ~ ", "", paste(deparse(formula), collapse = "")), "))^2")
  f <- gsub("  ", "", f)

  psi <- calculus::derivative(
    f = f,
    var = names(par)
  )
  psi.fun <- eval(
    parse(
      text =
        paste0(
          "function(",
          paste(c("y", "x", "g", names(par)), collapse = ", "), ") {
      return(list(", paste(as.list(psi), collapse = ", "), "))}"
        )
    )
  )

  hess <- calculus::hessian(
    f = f,
    var = names(par)
  )

  hess.fun <- eval(
    parse(text = paste0(
      "function(",
      paste(c("y", "x", "g", names(par)), collapse = ", "), ") {
      return(list(", paste(as.list(hess), collapse = ", "), "))}"
    ))
  )

  psi.val <- do.call(
    cbind,
    do.call(
      psi.fun,
      append(list(y = y, x = x, g = group), par)
    )
  )

  # calculating matrix Sigma
  mat.sigma <- t(psi.val) %*% psi.val / n
  # calculating matrix Gamma
  mat.gamma <- matrix(
    sapply(
      do.call(
        hess.fun,
        append(list(y = y, x = x, g = group), par)
      ),
      mean
    ),
    ncol = length(par), nrow = length(par)
  )
  # sandwich estimator of covariance matrix
  cov.sandwich <- solve(mat.gamma) %*% mat.sigma %*% solve(mat.gamma) / n
  rownames(cov.sandwich) <- colnames(cov.sandwich) <- names(par)
  return(cov.sandwich)
}

# ---------------------------------------------------------------------------------
# Maximum likelihood estimation
# ---------------------------------------------------------------------------------

# log-likelihood function to be maximized in MLE estimation
.likelihood <- function(data, formula, par) {
  param <- as.list(par)
  param[[length(param) + 1]] <- data$x
  names(param)[length(param)] <- "x"
  param[[length(param) + 1]] <- data$g
  names(param)[length(param)] <- "g"

  y <- data$y

  h. <- parse(text = as.character(formula)[3])
  h <- eval(h., envir = param)

  l <- -sum((y * log(h)) + ((1 - y) * log(1 - h)), na.rm = TRUE)
  return(l)
}

# Maximum likelihood estimation
.MLE_estimation <- function(formula, data, par, lower, upper) {
  m <- optim(
    par = par,
    fn = .likelihood,
    data = data,
    formula = formula,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    hessian = TRUE
  )

  h. <- parse(text = as.character(formula)[3])
  param <- as.list(par)
  param[[length(param) + 1]] <- data$x
  names(param)[length(param)] <- "x"
  param[[length(param) + 1]] <- data$g
  names(param)[length(param)] <- "g"

  m$fitted <- eval(h., param)
  m$data <- data
  m$formula <- formula

  class(m) <- "mle"
  return(m)
}

# ---------------------------------------------------------------------------------
# EM algorithm
# ---------------------------------------------------------------------------------

# E-step in EM algorithm
.expectation <- function(data, par) {
  expit <- function(x) {
    return(exp(x) / (1 + exp(x)))
  }
  par_asymp <- c("c" = 0, "cDif" = 0, "d" = 1, "dDif" = 0)
  par_expit <- c("b0" = 0, "b1" = 0, "b2" = 0, "b3" = 0)

  par_asymp[!is.na(par[c("c", "cDif", "d", "dDif")])] <- c(par[c("c", "cDif", "d", "dDif")])[!is.na(par[c("c", "cDif", "d", "dDif")])]
  par_expit[!is.na(par[c("b0", "b1", "b2", "b3")])] <- c(par[c("b0", "b1", "b2", "b3")])[!is.na(par[c("b0", "b1", "b2", "b3")])]

  phi <- as.vector(expit(par_expit %*% t(cbind(1, data$x, data$g, data$x * data$g))))

  z1 <- data$y * (par_asymp["c"] + par_asymp["cDif"] * data$g) /
    (par_asymp["c"] + par_asymp["cDif"] * data$g + (par_asymp["d"] + par_asymp["dDif"] * data$g - par_asymp["c"] - par_asymp["cDif"] * data$g) * phi)
  z2 <- data$y - z1
  z4 <- (1 - data$y) * (1 - par_asymp["d"] - par_asymp["dDif"] * data$g) /
    (1 - par_asymp["c"] - par_asymp["cDif"] * data$g - (par_asymp["d"] + par_asymp["dDif"] * data$g - par_asymp["c"] - par_asymp["cDif"] * data$g) * phi)
  z3 <- 1 - data$y - z4

  return(list(z1 = z1, z2 = z2, z3 = z3, z4 = z4))
}

# EM algorithm
.EM_algorithm <- function(formula, data, start,
                          lower, upper) {
  # toto je potreba jeste osetrit
  par_estimated <- sapply(c("x", "g", "x:g", "c", "cDif", "d", "dDif"), function(x) grepl(x, formula[3]))
  par_estimated <- c("b0", c("b1", "b2", "b3", "c", "cDif", "d", "dDif")[par_estimated])

  # which parameters are estimated and related parts of formula
  par_estim_M1 <- c("b0", "b1", "b2", "b3")
  par_estim_M2 <- c("c")
  form_M1 <- "x + g + x:g"
  form_M2 <- "c + (1 - c)"

  # initial values
  fit1_par_new <- start[paste0("b", 0:3)]
  fit2_par_new <- start[c("c", "cDif", "d", "dDif")]

  par <- list()
  k <- 1
  dev_new <- 0
  conv <- "DO NOT FINISH"

  # EM algorithm
  repeat ({
    par[[k]] <- c(
      fit1_par_new,
      fit2_par_new
    )

    # checking maximal number of iterations
    # actual number of iterations is k - 1,
    # the first is for the initial run
    if (k == 2001) {
      break
    }

    # --------------------
    # E-step
    # --------------------
    Z <- .expectation(data = data, par = setNames(unlist(c(
      fit1_par_new,
      fit2_par_new
    )), c(paste0("b", 0:3), "c", "cDif", "d", "dDif")))

    # --------------------
    # M-step
    # --------------------

    Z_M1 <- cbind(Z$z2, Z$z3)
    fit1 <- tryCatch(glm(as.formula(paste("Z_M1", "~", form_M1)),
                         family = binomial(),
                         data = data,
                         start = unlist(c(b0_new, b1_new, b2_new, b3_new))
    ), error = function(e) e)

    if (any(class(fit1) %in% c("simpleError", "error", "condition"))) {
      conv <- "CRASHED"
      break
    }
    fit1_par_new <- setNames(rep(0, 4), paste0("b", 0:3))
    fit1_par_new[par_estim_M1] <- coef(fit1)

    Z_M2 <- cbind(Z$z2 + Z$z3, Z$z1, Z$z4)[, c(TRUE, sapply(c("c", "d"), function(x) grepl(x, par_estim_M2)))]
    if (ncol(Z_M2) > 1) {
      group_present_M2 <- grepl("Dif", par_estim_M2)
      form_M2 <- ifelse(group_present_M2, "g", "1")

      fit2 <- tryCatch(
        nnet::multinom(as.formula(paste("Z_M2", "~", form_M2)),
                       trace = FALSE,
                       data = data
        ),
        error = function(e) e
      )
      if (any(class(fit2) %in% c("simpleError", "error", "condition"))) {
        conv <- "CRASHED"
        break
      }
      fit2_par_new <- setNames(c(0, 0, 1, 0), c("c", "cDif", "d", "dDif"))
      # V2 = d - c, V3 = c, V4 = 1 - d
      if (group_present_M2) {
        fit2_par_new[par_estim_M2] <- as.data.frame(cbind(g = unique(data$g), unique(fitted(fit2))))[, c("g", "V3", "V4")]
      } else {
        fit2_par_new[par_estim_M2] <- as.data.frame(unique(fitted(fit2)))[, c("V2")]
      }

      # deviance
      dev_old <- dev_new
      dev_new <- deviance(fit1) + deviance(fit2)
    } else {
      # deviance
      dev_old <- dev_new
      dev_new <- deviance(fit1)
    }

    # checking stopping criterion
    if (abs(dev_old - dev_new) / (0.1 + dev_new) < 1e-6) {
      k <- k + 1
      par[[k]] <- c(
        fit1_par_new,
        fit2_par_new
      )
      conv <- "CONVERGED"
      break
    }
    k <- k + 1
  })

  if (conv == "CRASHED") {
    se <- rep(NA, 8)
    par <- rep(NA, 8)
    num_iter <- NA
  } else {
    par <- as.data.frame(do.call(rbind, par))
    colnames(par) <- c("b0", "b1", "b2", "b3", "c", "cDif", "d", "dDif")

    # number of iterations
    num_iter <- nrow(par) - 1
    # final parameter estimates
    par <- par[nrow(par), ]
    # standard errors of the estimates
    se <- tryCatch(sqrt(diag(.covariance_matrix(unlist(par), data = data))),
                   error = function(e) e
    )
    if (any(class(se) %in% c("simpleError", "error", "condition"))) {
      se <- rep(NA, 8)
    }
  }
}

# Covariance matrix
# Calculates estimate of covariance matrix
# Arguments:
#    par = vector of parameters of the nonlinear model:
#          b0, b1, b2, b3, c, d, dDif, dDif
#    data = data.frame with three variable:
#          y = outcome (binary vector)
#          x = observed ability (numeric vector)
#          g = grouping variable (binary vector)
.covariance_matrix <- function(par, data) {
  # log-likelihood function
  ll <- function(par, data) {
    -sum(data$y * log(par[5] + par[6] * data$g + # c + cDif * g +
                        (par[7] + par[8] * data$g - par[5] - par[6] * data$g) / # (d + dDif * g - c - cDif * g)
                        (1 + exp(-(par[1] + par[2] * data$x + par[3] * data$g + par[4] * data$x * data$g)))) + # b0 + b1 * x + b2 * g + b3 * x * g
           (1 - data$y) * log(1 - par[5] - par[6] * data$g - # 1 - c - cDif * g -
                                (par[7] + par[8] * data$g - par[5] - par[6] * data$g) / # (d + dDif * g - c - cDif * g)
                                (1 + exp(-(par[1] + par[2] * data$x + par[3] * data$g + par[4] * data$x * data$g)))), na.rm = TRUE) # b0 + b1 * x + b2 * g + b3 * x * g
  }

  # evaluating Hessian function and computing its inverse
  hessian <- calculus::hessian(f = ll, var = as.vector(par), params = list(data = data))
  return(solve(hessian))
}

