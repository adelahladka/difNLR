#' Non-linear regression DIF models estimation.
#'
#' @description
#' Estimates parameters of non-linear regression models for DIF detection using
#' either non-linear least squares or maximum likelihood method with various
#' algorithms.
#'
#' @usage
#' estimNLR(y, match, group, formula, method, lower, upper, start)
#'
#' @param y numeric: a binary vector of responses (\code{"1"} correct,
#'   \code{"0"} incorrect).
#' @param match numeric: a numeric vector describing the matching criterion.
#' @param group numeric: a binary vector of a group membership (\code{"0"}
#'   for the reference group, \code{"1"} for the focal group).
#' @param formula formula: specification of the model. It can be obtained by the
#'  \code{formulaNLR()} function.
#' @param method character: an estimation method to be applied. The options are
#'   \code{"nls"} for non-linear least squares (default), \code{"mle"} for the
#'   maximum likelihood method using the \code{"L-BFGS-B"} algorithm with
#'   constraints, \code{"em"} for the maximum likelihood estimation with the EM
#'   algorithm, \code{"plf"} for the maximum likelihood estimation with the
#'   algorithm based on parametric link function, and \code{"irls"} for the maximum
#'   likelihood estimation with the iteratively reweighted least squares algorithm
#'   (available for the \code{"2PL"} model only). See \strong{Details}.
#' @param lower numeric: lower bounds for item parameters of the model specified
#'   in the \code{formula}.
#' @param upper numeric: upper bounds for item parameters of the model specified
#'   in the \code{formula}.
#' @param start numeric: initial values of item parameters. They can be obtained
#'   by the \code{startNLR()} function.
#'
#' @details
#' The function offers either the non-linear least squares estimation via the
#' \code{\link[stats]{nls}} function (Drabinova & Martinkova, 2017; Hladka &
#' Martinkova, 2020), the maximum likelihood method with the \code{"L-BFGS-B"}
#' algorithm with constraints via the \code{\link[stats]{optim}} function (Hladka &
#' Martinkova, 2020), the maximum likelihood method with the EM algorithm (Hladka,
#' Martinkova, & Brabec, 2024), the maximum likelihood method with the algorithm
#' based on parametric link function (PLF; Hladka, Martinkova, & Brabec, 2024), or
#' the maximum likelihood method with the iteratively reweighted least squares
#' algorithm via the \code{\link[stats]{glm}} function.
#'
#' @author
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references
#' Drabinova, A. & Martinkova, P. (2017). Detection of differential item
#' functioning with nonlinear regression: A non-IRT approach accounting for
#' guessing. Journal of Educational Measurement, 54(4), 498--517,
#' \doi{10.1111/jedm.12158}.
#'
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' Hladka, A. (2021). Statistical models for detection of differential item
#' functioning. Dissertation thesis.
#' Faculty of Mathematics and Physics, Charles University.
#'
#' Hladka, A., Martinkova, P., & Brabec, M. (2024). New iterative algorithms
#' for estimation of item functioning. Journal of Educational and Behavioral
#' Statistics. Online first, \doi{10.3102/10769986241312354}.
#'
#' @examples
#' # loading data
#' data(GMAT)
#' y <- GMAT[, 1] # item 1
#' match <- scale(rowSums(GMAT[, 1:20])) # standardized total score
#' group <- GMAT[, "group"] # group membership variable
#'
#' # formula for 3PL model with the same guessing for both groups,
#' # IRT parameterization
#' M <- formulaNLR(model = "3PLcg", type = "both", parameterization = "irt")
#'
#' # starting values for 3PL model with the same guessing for item 1
#' start <- startNLR(GMAT[, 1:20], group, model = "3PLcg", parameterization = "irt")
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
#' # formula for 3PL model with the same guessing for both groups
#' # intercept-slope parameterization
#' M <- formulaNLR(model = "3PLcg", type = "both", parameterization = "is")
#'
#' # starting values for 3PL model with the same guessing for item 1,
#' start <- startNLR(GMAT[, 1:20], group, model = "3PLcg", parameterization = "is")
#' start <- start[[1]][M$M1$parameters]
#'
#' # EM algorithm
#' (fit_em <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M1$formula, method = "em",
#'   lower = M$M1$lower, upper = M$M1$upper, start = start
#' ))
#'
#' coef(fit_em)
#' logLik(fit_em)
#' vcov(fit_em)
#' fitted(fit_em)
#' residuals(fit_em)
#'
#' # PLF algorithm
#' (fit_plf <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M1$formula, method = "plf",
#'   lower = M$M1$lower, upper = M$M1$upper, start = start
#' ))
#'
#' coef(fit_plf)
#' logLik(fit_plf)
#' vcov(fit_plf)
#' fitted(fit_plf)
#' residuals(fit_plf)
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
    "nls" = tryCatch(
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
    "mle" = tryCatch(
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
    "irls" = tryCatch(
      glm(
        formula = formula,
        family = binomial(),
        data = data.frame(y = y, x = match, g = group)
      ),
      error = function(e) {},
      finally = ""
    ),
    "em" = tryCatch(
      .EM_algorithm(
        formula = formula,
        data = data.frame(y = y, x = match, g = group),
        start = start,
        lower = lower,
        upper = upper
      ),
      error = function(e) {},
      finally = ""
    ),
    "plf" = tryCatch(
      .PLF_algorithm(
        formula = formula,
        data = data.frame(y = y, x = match, g = group),
        start = start,
        lower = lower,
        upper = upper
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
      -N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw)) / N + log(sum(res^2))) / 2
    },
    "mle" = -object$value,
    "glm" = object$rank - object$aic / 2,
    "em" = -.loglikelihood(formula = object$formula, par = object$par, data = object$data),
    "plf" = -.loglikelihood(formula = object$formula, par = object$par, data = object$data)
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
    "glm" = object$coefficients,
    "em" = object$par,
    "plf" = object$par
  )
}

#' @rdname estimNLR
#' @export
fitted.estimNLR <- function(object, ...) {
  val <- switch(class(object)[2],
    "nls" = as.vector(object$m$fitted()),
    "mle" = object$fitted,
    "glm" = object$fitted.values,
    "em" = object$fitted,
    "plf" = object$fitted
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
    "glm" = object$residuals,
    "em" = object$data$y - object$fitted,
    "plf" = object$data$y - object$fitted
  )
  lab <- "Residuals"
  attr(val, "label") <- lab
  val
}

#' @rdname estimNLR
#' @param x an object of the \code{"estimNLR"} class.
#' @export
print.estimNLR <- function(x, ...) {
  formula <- switch(class(x)[2],
    "nls" = paste(x$m$formula()[2], x$m$formula()[1], x$m$formula()[3]),
    "mle" = paste(x$formula[2], x$formula[1], x$formula[3]),
    "glm" = paste0(x$formula[2], " ", x$formula[1], " exp(", x$formula[3], ") / (1 + exp(", x$formula[3], "))"),
    "em" = paste(x$formula[2], x$formula[1], x$formula[3]),
    "plf" = paste(x$formula[2], x$formula[1], x$formula[3]),
  )
  cat(
    "Nonlinear regression model \n\n",
    "Model: ", formula, "\n"
  )
  pars <- switch(class(x)[2],
    "nls" = x$m$getPars(),
    "mle" = x$par,
    "glm" = x$coefficients,
    "em" = x$par,
    "plf" = x$par
  )
  alg <- switch(class(x)[2],
    "nls" = "Nonlinear least squares estimation",
    "mle" = "Maximum likelihood estimation using the L-BFGS-B algorithm",
    "glm" = "Maximum likelihood estimation using the iteratively reweighted least squares algorithm",
    "em" = "Maximum likelihood estimation using the EM algorithm",
    "plf" = "Maximum likelihood estimation using the PLF-based algorithm"
  )
  cat("\nCoefficients:\n")
  print(round(pars, 4))
  cat("\n", alg, "\n")
  conv <- switch(class(x)[2],
    "nls" = ifelse(x$convInfo$isConv, "Converged", "Did not converged"),
    "mle" = ifelse(x$convergence == 0, "Converged", "Did not converged"),
    "glm" = ifelse(x$converged, "Converged", "Did not converged"),
    "em" = paste0(toupper(substr(x$conv, 1, 1)), tolower(substr(x$conv, 2, nchar(x$conv)))),
    "plf" = paste0(toupper(substr(x$conv, 1, 1)), tolower(substr(x$conv, 2, nchar(x$conv))))
  )
  numitem <- switch(class(x)[2],
    "nls" = x$convInfo$finIter,
    "mle" = x$counts[1],
    "glm" = x$iter,
    "em" = x$numitem,
    "plf" = x$numitem
  )
  cat(conv, "after", numitem, "iterations")
}

#' @rdname estimNLR
#' @param object an object of the \code{"estimNLR"} class.
#' @param sandwich logical: should the sandwich estimator be applied for
#'   computation of the covariance matrix of item parameters when using
#'   \code{method = "nls"}? (the default is \code{FALSE}).
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
    } else if (inherits(object, "glm")) {
      cov.object <- tryCatch(
        {
          vcov(summary(object))
        },
        error = function(e) NULL
      )
    } else {
      cov.object <- tryCatch(
        {
          .covariance(formula = object$formula, par = object$par, data = object$data)
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

# log-likelihood function to be maximized in MLE estimation (including EM and PLF algorithms)
.loglikelihood <- function(formula, par, data) {
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

# function to compute covariance matrix for MLE estimates (including EM and PLF algorithms)
.covariance <- function(formula, par, data) {
  # ll <- parse(text = paste0("y * log(", as.character(formula)[3], ") + (1 - y) * log(1 - (", as.character(formula)[3], "))"))
  ll <- parse(text = paste0("-sum(y * log(", as.character(formula)[3], ") + (1 - y) * log(1 - (", as.character(formula)[3], ")), na.rm = TRUE)"))

  args <- vector("list", length(par) + ncol(data))
  names(args) <- c(names(par), colnames(data))
  fun <- args
  fun[[length(fun) + 1]] <- ll[[1]]

  fun <- as.function(fun)

  hessian <- calculus::hessian(f = fun, var = par, params = list(y = data$y, x = data$x, g = data$g))
  # AH: there are different results, this needs to be checked
  # H <- calculus::hessian(ll, var = names(par))
  # hess.fun <- eval(
  #   parse(text = paste0(
  #     "function(",
  #     paste(c("y", "x", "g", names(par)), collapse = ", "), ") {
  #     return(list(", paste(as.list(H), collapse = ", "), "))}"
  #   ))
  # )
  #
  # list.H <- do.call(
  #   hess.fun,
  #   append(list(y = data$y, x = data$x, g = data$g), par)
  # )
  #
  # hessian <- -1 * matrix(
  #   sapply(
  #     list.H,
  #     sum, na.rm = TRUE
  #   ),
  #   ncol = length(par), nrow = length(par)
  # )

  return(structure(solve(hessian), dimnames = list(names(par), names(par))))
}

# maximum likelihood estimation
.MLE_estimation <- function(formula, data, par, lower, upper) {
  m <- optim(
    par = par,
    fn = .loglikelihood,
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
.EM_expectation <- function(data, par) {
  expit <- function(x) {
    return(exp(x) / (1 + exp(x)))
  }
  par_asymp <- c("c" = 0, "cR" = 0, "cDif" = 0, "cF" = 0, "d" = 1, "dR" = 1, "dDif" = 0, "dF" = 1)
  par_expit <- c("b0" = 0, "b1" = 0, "b2" = 0, "b3" = 0)

  par_asymp[c("c", "cR", "cDif", "cF", "d", "dR", "dDif", "dF") %in% names(par)] <- par[names(par) %in% c("c", "cR", "cDif", "cF", "d", "dR", "dDif", "dF")]
  par_expit[c("b0", "b1", "b2", "b3") %in% names(par)] <- par[names(par) %in% c("b0", "b1", "b2", "b3")]

  phi <- as.vector(expit(par_expit %*% t(cbind(1, data$x, data$g, data$x * data$g))))

  part_c <- if ("cR" %in% names(par)) {
    par_asymp["cR"] * (1 - data$g) + par_asymp["cF"] * data$g
  } else {
    par_asymp["c"] + par_asymp["cDif"] * data$g
  }
  part_d <- if ("dR" %in% names(par)) {
    par_asymp["dR"] * (1 - data$g) + par_asymp["dF"] * data$g
  } else {
    par_asymp["d"] + par_asymp["dDif"] * data$g
  }

  W1 <- data$y * part_c / (part_c + (part_d - part_c) * phi)
  W2 <- data$y - W1
  W4 <- (1 - data$y) * (1 - part_d) / (1 - part_c - (part_d - part_c) * phi)
  W3 <- 1 - data$y - W4

  return(list(W1 = W1, W2 = W2, W3 = W3, W4 = W4))
}

# EM algorithm
.EM_algorithm <- function(formula, data, start,
                          lower, upper) {
  # extract estimated parameters
  par_estim_M1 <- sapply(c("b0", "b1", "b2", "b3"), function(x) grepl(paste0("\\b", x, "\\b"), formula[3]))
  par_estim_M1 <- names(par_estim_M1[par_estim_M1])
  par_estim_M2 <- sapply(c("c", "cR", "cDif", "cF", "d", "dR", "dDif", "dF"), function(x) grepl(paste0("\\b", x, "\\b"), formula[3]))
  par_estim_M2 <- names(par_estim_M2[par_estim_M2])

  form_split <- strsplit(as.character(formula[3]), split = "/")[[1]]
  # form_M1 <- form_split[2]
  par_old <- c("b0", "b1", "b2", "b3")
  par_new <- c("", "x", "g", "x:g")
  form_M1 <- .MYpaste(diag(sapply(which(par_old %in% par_estim_M1), function(i) gsub(par_old[i], par_new[i], par_estim_M1))), collapse = " + ")
  form_M2 <- form_split[1]

  # initial values
  fit1_par_new <- unlist(start[par_estim_M1])
  fit2_par_new <- unlist(start[par_estim_M2])

  par <- list()
  k <- 1
  dev_new <- 0
  conv <- "DO NOT FINISHED"
  group_present_M2 <- any(grepl("Dif", par_estim_M2) | grepl("F", par_estim_M2))
  form_M2 <- ifelse(group_present_M2, "g", "1")

  # EM algorithm
  repeat ({
    par[[k]] <- setNames(c(
      fit1_par_new,
      fit2_par_new
    ), c(par_estim_M1, par_estim_M2))

    # checking maximal number of iterations
    # actual number of iterations is k - 1,
    # the first is for the initial run
    if (k == 2001) {
      break
    }

    # --------------------
    # E-step
    # --------------------

    W <- .EM_expectation(data = data, par = par[[k]])

    # --------------------
    # M-step
    # --------------------

    W_M1 <- cbind(W$W2, W$W3)
    fit1 <- tryCatch(glm(as.formula(paste("W_M1", "~", form_M1)),
      family = quasibinomial(),
      data = data,
      start = unlist(c(fit1_par_new))
    ), error = function(e) e)

    if (any(class(fit1) %in% c("simpleError", "error", "condition"))) {
      conv <- "CRASHED"
      break
    }
    fit1_par_new <- coef(fit1)

    # this needs to be checked
    W_M2 <- cbind(W23 = W$W2 + W$W3, W1 = W$W1, W4 = W$W4)[, c(TRUE, c(any(grepl("c", par_estim_M2)), any(grepl("d", par_estim_M2))))]
    if (ncol(W_M2) > 1) {
      fit2 <- tryCatch(
        nnet::multinom(as.formula(paste("W_M2", "~", form_M2)),
          trace = FALSE,
          data = data
        ),
        error = function(e) e
      )
      if (any(class(fit2) %in% c("simpleError", "error", "condition"))) {
        conv <- "CRASHED"
        break
      }
      if (group_present_M2) {
        # W23 is probability without guessing and slipping
        # W1 is guessing, W4 is slipping
        fitted_fit2 <- as.data.frame(cbind(g = unique(data$g), unique(fitted(fit2))))
        fitted_fit2 <- fitted_fit2[order(fitted_fit2$g), ]
        if (any(grepl("c", par_estim_M2))) {
          fit2_par_new[grepl("c", par_estim_M2)] <- fitted_fit2[, "W1"]
        }
        if (any(grepl("d", par_estim_M2))) {
          fit2_par_new[grepl("d", par_estim_M2)] <- 1 - fitted_fit2[, "W4"]
        }
      } else {
        fitted_fit2 <- as.data.frame(unique(fitted(fit2)))
        if (any(grepl("c", par_estim_M2))) {
          fit2_par_new[grepl("c", par_estim_M2)] <- fitted_fit2[, "W1"]
        }
        if (any(grepl("d", par_estim_M2))) {
          fit2_par_new[grepl("d", par_estim_M2)] <- 1 - fitted_fit2[, "W4"]
        }
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
    # number of iterations
    num_iter <- nrow(par) - 1
    # final parameter estimates
    par <- unlist(par[nrow(par), ])
    # standard errors of the estimates
    se <- tryCatch(sqrt(diag(.covariance(formula = formula, par = unlist(par), data = data))),
      error = function(e) e
    )
    if (any(class(se) %in% c("simpleError", "error", "condition"))) {
      se <- rep(NA, length(par))
    }
  }

  m <- list()
  m$data <- data
  m$formula <- formula
  m$par <- par
  m$se <- se
  m$convergence <- conv
  m$numitem <- k - 1
  m$W <- do.call(cbind, W)

  h. <- parse(text = as.character(formula)[3])
  param <- as.list(par)
  param[[length(param) + 1]] <- data$x
  names(param)[length(param)] <- "x"
  param[[length(param) + 1]] <- data$g
  names(param)[length(param)] <- "g"

  m$fitted <- eval(h., param)

  class(m) <- "em"
  return(m)
}

# ------------------------------------------------------------------------------
# ALGORITHM BASED ON PARAMETRIC LINK FUNCTION
# ------------------------------------------------------------------------------

.PLF_plogit <- function(cR = 0, cF = 0, dR = 1, dF = 1, g) {
  cp <- cR * (1 - g) + cF * g
  dp <- dR * (1 - g) + dF * g

  logitint <- function(p, cp, dp) {
    log(ifelse((p - cp) / (dp - p) <= 0, 0.00001, (p - cp) / (dp - p)))
  }

  # link function
  linkfun <- function(mu) logitint(mu, cp, dp)

  # the inverse of the link function
  linkinv <- function(eta) {
    cp + (dp - cp) * exp(eta) / (1 + exp(eta))
  }

  # derivative of the inverse-link function with respect
  # to the linear predictor
  mu.eta <- function(eta) {
    (dp - cp) * exp(eta) / (1 + exp(eta))^2
  }

  # TRUE if eta is in the domain of linkinv
  valideta <- function(eta) TRUE

  name <- "plogit"
  structure(
    list(
      linkfun = linkfun,
      linkinv = linkinv,
      valideta = valideta,
      mu.eta = mu.eta,
      name = name
    ),
    class = "link-glm"
  )
}

# likelihood for asymptote parameters, when parameters of the logistic
# curve are fixed
.PLF_loglikelihood_asymptotes <- function(par, formula) {
  par_estim <- sapply(c("c", "cR", "cDif", "cF", "d", "dR", "dDif", "dF"), function(x) grepl(paste0("\\b", x, "\\b"), formula))
  par_estim <- names(which(par_estim))

  formula_new <- formula
  for (x in 1:length(par_estim)) {
    formula_new <- gsub(par_estim[x], paste0("par[[", x, "]]"), formula_new)
  }

  h.fun <- eval(
    parse(text = paste0("function(par, y, group, expit) {
                        ", paste("h <-", formula_new, "* expit"), "
                        h <- ifelse(h <= 0, 0.000001, ifelse(h >= 1, 1 - 0.000001, h))
                        ll <- -sum((y * log(h)) + ((1 - y) * log(1 - h)), na.rm = TRUE)
                        return(ll)
                        }"))
  )
  return(h.fun)
}

# EM algorithm
.PLF_algorithm <- function(formula, data, start,
                           lower, upper) {
  # extract estimated parameters
  par_estim_M1 <- sapply(c("b0", "b1", "b2", "b3"), function(x) grepl(paste0("\\b", x, "\\b"), formula[3]))
  par_estim_M1 <- names(par_estim_M1[par_estim_M1])
  par_estim_M2 <- sapply(c("c", "cR", "cDif", "cF", "d", "dR", "dDif", "dF"), function(x) grepl(paste0("\\b", x, "\\b"), formula[3]))
  par_estim_M2 <- names(par_estim_M2[par_estim_M2])

  form_split <- strsplit(as.character(formula[3]), split = "/")[[1]]
  # form_M1 <- form_split[2]
  par_old <- c("b0", "b1", "b2", "b3")
  par_new <- c("", "x", "g", "x:g")
  form_M1 <- .MYpaste(diag(sapply(which(par_old %in% par_estim_M1), function(i) gsub(par_old[i], par_new[i], par_estim_M1))), collapse = " + ")
  form_M2 <- gsub("g", "group", form_split[1])

  # initial values
  fit1_par_new <- unlist(start[par_estim_M1])
  fit2_par_new <- unlist(start[par_estim_M2])

  par <- list()
  k <- 1
  ll_new <- 0
  conv <- "DO NOT FINISHED"
  group_present_M2 <- any(grepl("Dif", par_estim_M2) | grepl("F", par_estim_M2))
  dif_type_M1 <- setNames(rep(FALSE, 4), paste0("b", 0:3))
  dif_type_M1[par_estim_M1] <- TRUE

  asymp_pars <- c(cR = 0, cF = 0, c = 0, dR = 1, dF = 1, d = 1)

  # algorithm based on parametric link function
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
    # Step 1: fitting GLM with parametric link function
    # --------------------
    asymp_pars[par_estim_M2] <- fit2_par_new
    if (!group_present_M2) {
      asymp_pars[["dR"]] <- asymp_pars[["dF"]] <- asymp_pars[["d"]]
      asymp_pars[["cR"]] <- asymp_pars[["cF"]] <- asymp_pars[["c"]]
    }

    fit_1 <- tryCatch(
      glm(as.formula(paste("y ~", form_M1)),
        data = data,
        family = binomial(
          link = .PLF_plogit(0, 0, asymp_pars[["dR"]], asymp_pars[["dF"]], data$g) # this needs to be fixed
        ),
        start = fit1_par_new
      ),
      error = function(e) e
    )

    if (any(class(fit_1) %in% c("simpleError", "error", "condition"))) {
      conv <- "CRASHED"
      break
    }
    fit1_par_new <- coef(fit_1)
    tmp <- as.vector(coef(fit_1) %*% rbind(1, data$x, data$g, data$x * data$g)[dif_type_M1, ])
    expit <- exp(tmp) / (1 + exp(tmp))

    # --------------------
    # Step 2: estimating asymptotes parameters
    # --------------------
    # bound for asymptotes
    fit2_par_start <- fit2_par_new
    if (any(grepl("c", par_estim_M2))) {
      if (group_present_M2) {
        cR_max <- max(min(fitted(fit_1)[data$g == 0], na.rm = TRUE), 0)
        fit2_par_start[["cR"]] <- (fit2_par_start[["cR"]] + cR_max) / 2
        cF_max <- max(min(fitted(fit_1)[data$g == 1], na.rm = TRUE), 0)
        fit2_par_start[["cF"]] <- (fit2_par_start[["cF"]] + cF_max) / 2
      } else {
        c_max <- max(min(fitted(fit_1), na.rm = TRUE), 0)
        fit2_par_start[["c"]] <- (fit2_par_start[["c"]] + c_max) / 2
      }
    }
    if (any(grepl("d", par_estim_M2))) {
      if (group_present_M2) {
        dR_min <- min(max(fitted(fit_1)[data$g == 0], na.rm = TRUE), 1)
        fit2_par_start[["dR"]] <- (fit2_par_start[["dR"]] + dR_min) / 2
        dF_min <- min(max(fitted(fit_1)[data$g == 1], na.rm = TRUE), 1)
        fit2_par_start[["dF"]] <- (fit2_par_start[["dF"]] + dF_min) / 2
      } else {
        d_min <- min(max(fitted(fit_1), na.rm = TRUE), 1)
        fit2_par_start[["d"]] <- (fit2_par_start[["d"]] + d_min) / 2
      }
    }
    # log-likelihood function for asymptotes
    PLF_ll_asymptotes <- .PLF_loglikelihood_asymptotes(par = par_estim_M2, formula = form_M2)
    # fitting model for asymptotes
    fit_2 <- tryCatch(optim(
      par = fit2_par_start,
      fn = PLF_ll_asymptotes, y = data$y, group = data$g, expit = expit,
      method = "L-BFGS-B",
      lower = lower[par_estim_M2],
      upper = upper[par_estim_M2],
    ), error = function(e) e)

    if (any(class(fit_2) %in% c("simpleError", "error", "condition"))) {
      conv <- "CRASHED"
      break
    }

    fit2_par_new <- fit_2$par[par_estim_M2]

    # log-likelihood
    ll_old <- ll_new
    ll_new <- logLik(fit_1) - fit_2$value

    # checking stopping criterion
    if (abs(abs(ll_old - ll_new) / (0.1 + ll_new)) < 1e-6) {
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
    # number of iterations
    num_iter <- nrow(par) - 1
    # final parameter estimates
    par <- unlist(par[nrow(par), ])
    # standard errors of the estimates
    se <- tryCatch(sqrt(diag(.covariance(formula = formula, par = unlist(par), data = data))),
      error = function(e) e
    )
    if (any(class(se) %in% c("simpleError", "error", "condition"))) {
      se <- rep(NA, length(par))
    }
  }

  m <- list()
  m$data <- data
  m$formula <- formula
  m$par <- par
  m$se <- se
  m$convergence <- conv
  m$numitem <- k

  h. <- parse(text = as.character(formula)[3])
  param <- as.list(par)
  param[[length(param) + 1]] <- data$x
  names(param)[length(param)] <- "x"
  param[[length(param) + 1]] <- data$g
  names(param)[length(param)] <- "g"

  m$fitted <- eval(h., param)

  class(m) <- "plf"
  return(m)
}
