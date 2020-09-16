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
#' squares (default) and \code{"likelihood"} for maximum likelihood method.
#' @param lower numeric: lower bounds for parameters of model specified in \code{formula}.
#' @param upper numeric: upper bounds for parameters of model specified in \code{formula}.
#' @param start numeric: initial parameters. Can be obtained by \code{startNLR()} function.
#'
#' @usage estimNLR(y, match, group, formula, method, lower, upper, start)
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
#' @examples
#' data(GMAT)
#'
#' # item 1
#' y <- GMAT[, 1]
#' match <- scale(rowSums(GMAT[, 1:20]))
#' group <- GMAT[, "group"]
#'
#' # formula for 3PL model with the same guessing
#' M <- formulaNLR(model = "3PLcg", type = "both")
#'
#' # starting values for 3PL model with the same guessing for item 1
#' start <- startNLR(GMAT[, 1:20], group, model = "3PLcg", parameterization = "classic")
#' start <- start[[1]][M$M0$parameters]
#'
#' # Non-linear least squares
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
#' # Maximum likelihood
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
#' @keywords DIF
#' @export
#'
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
    IRLS = tryCatch(glm(formula, family = binomial()),
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
print.lkl <- function(x, ...) {
  cat(
    "Nonlinear regression model \n",
    "model: ", paste(x$formula[2], x$formula[1], x$formula[3]), "\n"
  )
  print(x$par)
  cat(
    "\n",
    "Algorithm L-BFGS-B"
  )
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
vcov.estNLR <- function(object, sandwich = FALSE, ...) {
  if (class(object)[2] == "nls") {
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
    cov.object <- tryCatch(
      {
        solve(object$hessian)},
      error = function(e) NULL
    )
  }
  return(cov.object)
}

#' @noRd
.sandwich.cov.nls <- function(formula, y, x, group, par) {
  n <- length(y)
  f <- paste0("(y - ", "(", gsub("y ~ ", "", paste(deparse(formula), collapse = "")), "))^2")
  f <- gsub("  ", "", f)

  psi <- derivative(
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

  hess <- hessian(
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
