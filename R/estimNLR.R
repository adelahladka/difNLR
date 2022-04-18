#' Non-Linear Regression DIF models estimation.
#'
#' @aliases estimNLR
#'
#' @description Estimates parameters of non-linear regression models for DIF detection using either
#' non-linear least squares or maximum likelihood method.
#'
#' @param y numeric: binary vector of responses.
#' @param match numeric: vector of matching criterion.
#' @param group numeric: binary vector of group membership. \code{"0"} for reference group, \code{"1"} for focal group.
#' @param formula formula: specification of the model. Can be obtained by \code{formulaNLR()} function.
#' @param method character: method used to estimate parameters. The options are \code{"nls"} for non-linear least
#' squares (default), \code{"likelihood"} for maximum likelihood method, and \code{"irls"} for maximum likelihood
#' estimation with iteratively reweighted least squares. See \strong{Details}.
#' @param lower numeric: lower bounds for parameters of model specified in \code{formula}.
#' @param upper numeric: upper bounds for parameters of model specified in \code{formula}.
#' @param start numeric: initial parameters. Can be obtained by \code{startNLR()} function.
#'
#' @usage estimNLR(y, match, group, formula, method, lower, upper, start)
#'
#' @details
#' Function offers either non-linear least squares estimation via \code{\link[stats]{nls}} function,
#' maximum likelihood method with \code{"L-BFGS-B"} method via \code{\link[stats]{optim}} function,
#' or maximum likelihood method with iteratively reweighted least squares via \code{\link[stats]{glm}}
#' function.
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
#' Hladka, A. (2021). Statistical models for detection of differential item functioning. Dissertation thesis.
#' Faculty of Mathematics and Physics, Charles University.
#'
#' @examples
#' # loading data
#' data(GMAT)
#' y <- GMAT[, 1] # item 1
#' match <- scale(rowSums(GMAT[, 1:20])) # standardized total score
#' group <- GMAT[, "group"] # group membership variable
#'
#' # formula for 3PL model with the same guessing
#' M <- formulaNLR(model = "3PLcg", type = "both")
#'
#' # starting values for 3PL model with the same guessing for item 1
#' start <- startNLR(GMAT[, 1:20], group, model = "3PLcg", parameterization = "classic")
#' start <- start[[1]][M$M0$parameters]
#'
#' # non-linear least squares
#' fitNLSM0 <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M0$formula, method = "nls",
#'   lower = M$M0$lower, upper = M$M0$upper, start = start
#' )
#' fitNLSM0
#'
#' coef(fitNLSM0)
#' logLik(fitNLSM0)
#' vcov(fitNLSM0)
#' vcov(fitNLSM0, sandwich = TRUE)
#' fitted(fitNLSM0)
#' residuals(fitNLSM0)
#'
#' # maximum likelihood method
#' fitLKLM0 <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M0$formula, method = "likelihood",
#'   lower = M$M0$lower, upper = M$M0$upper, start = start
#' )
#' fitLKLM0
#'
#' coef(fitLKLM0)
#' logLik(fitLKLM0)
#' vcov(fitLKLM0)
#' fitted(fitLKLM0)
#' residuals(fitLKLM0)
#'
#' # iteratively reweighted least squares for 2PL model
#' M <- formulaNLR(model = "2PL", parameterization = "logistic")
#' fitIRLSM1 <- estimNLR(
#'   y = y, match = match, group = group,
#'   formula = M$M1$formula, method = "irls"
#' )
#' fitIRLSM1
#'
#' coef(fitIRLSM1)
#' logLik(fitIRLSM1)
#' vcov(fitIRLSM1)
#' fitted(fitIRLSM1)
#' residuals(fitIRLSM1)
#' @keywords DIF
#' @export
estimNLR <- function(y, match, group, formula, method, lower, upper, start) {
  M <- switch(method,
    nls = tryCatch(nls(
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
    likelihood = tryCatch(lkl(
      formula = formula,
      data = data.frame(y = y, x = match, g = group),
      par = start,
      lower = lower,
      upper = upper
    ),
    error = function(e) {},
    finally = ""
    ),
    irls = tryCatch(glm(
      formula = formula,
      family = binomial(),
      data = data.frame(y = y, x = match, g = group)
    ),
    error = function(e) {},
    finally = ""
    )
  )
  if (!is.null(M)) {
    class(M) <- c("estNLR", class(M))
  }
  return(M)
}

# private function
.param.likel <- function(data, formula, par) {
  param <- as.list(par)
  param[[length(param) + 1]] <- data$x
  names(param)[length(param)] <- "x"
  param[[length(param) + 1]] <- data$g
  names(param)[length(param)] <- "g"

  y <- data$y

  h. <- parse(text = as.character(formula)[3])
  h <- eval(h., envir = param)

  l <- -sum((y * log(h)) + ((1 - y) * log(1 - h)), na.rm = T)
  return(l)
}

#' @noRd
lkl <- function(formula, data, par, lower, upper) {
  m <- optim(
    par = par,
    fn = .param.likel,
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

  class(m) <- "lkl"
  return(m)
}

#' @rdname lkl
#' @export
logLik.lkl <- function(object, ...) {
  -object$value
}

#' @rdname lkl
#' @export
coef.lkl <- function(object, ...) {
  object$par
}

#' @rdname lkl
#' @export
fitted.lkl <- function(object, ...) {
  object$fitted
}

#' @rdname lkl
#' @export
residuals.lkl <- function(object, ...) {
  object$data$y - object$fitted
}

#' @rdname estNLR
#' @export
print.estNLR <- function(x, ...) {
  formula <- switch(class(x)[2],
    "nls" = paste(x$m$formula()[2], x$m$formula()[1], x$m$formula()[3]),
    "lkl" = paste(x$formula[2], x$formula[1], x$formula[3]),
    "glm" = paste0(x$formula[2], " ", x$formula[1], " exp(", x$formula[3], ") / (1 + exp(", x$formula[3], "))")
  )
  cat(
    "Nonlinear regression model \n\n",
    "Model: ", formula, "\n"
  )
  pars <- switch(class(x)[2],
    "nls" = x$m$getPars(),
    "lkl" = x$par,
    "glm" = x$coefficients
  )
  alg <- switch(class(x)[2],
    "nls" = "Nonlinear least squares estimation",
    "lkl" = "Maximum likelihood estimation using L-BFGS-B algorithm",
    "glm" = "Maximum likelihood estimation using iteratively reweighted least squares algorithm"
  )
  cat("\nCoefficients:\n")
  print(round(pars, 4))
  cat("\n", alg)
}

#' @rdname estNLR
#' @export
vcov.estNLR <- function(object, sandwich = FALSE, ...) {
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
      message("Sandwich estimator of covariance not available for method = 'likelihood'. ")
    }
    if (inherits(object, "lkl")) {
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
