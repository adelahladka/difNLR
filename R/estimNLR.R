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
