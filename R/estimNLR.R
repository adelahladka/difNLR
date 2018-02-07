#' Estimation for non-linear regression DIF models
#'
#' @aliases estimNLR
#'
#' @usage estimNLR(y, match, group, formula, start, method)
#'
#' @description Estimates parameters of non-linear regression models for DIF detection.
#' This is very first version of the function.
#'
#' @param y numeric: binary vector. See \strong{Details}.
#' @param match numeric: matching criterion. See \strong{Details}.
#' @param group numeric: binary vector of group membership. \code{"0"} for reference group, \code{"1"} for focal group.
#' @param formula list: with the same structure as output of \code{formulaNLR} function. See \strong{Details}.
#' @param start numeric: initial parameters. See \strong{Details}.
#' @param method character: method used to estimate parameters. Options are \code{"nls"} for non-linear
#' least squares and \code{"likelihood"} for maximum likelihood. See \strong{Details}.
#'
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' drabinova@cs.cas.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @examples
#' \dontrun{
#' data(GMAT)
#'
#' y  <- GMAT[, 1]
#' x  <- scale(apply(GMAT[, 1:20], 1, sum))
#' g <- GMAT[, "group"]
#'
#' # starting values for 3PL model
#' start <- startNLR(GMAT[, 1:20], g, model = "3PLcg")
#' start <- start[1, c(1, 2, 3, 5, 6)]
#' names(start) <- c("a", "b", "c", "aDif", "bDif")
#'
#' formula <- formulaNLR(model = "3PLcg", type = "both")
#'
#' # Non-linear least squares
#' fitNLS <- estimNLR(y = y, match = x, group = g, formula = formula,
#' start = start, method = "nls")
#' lapply(fitNLS, coef)
#' lapply(fitNLS, logLik)
#' lapply(fitNLS, vcov)
#'
#' # Maximum likelihood
#' fitLKL <- estimNLR(y = y, match = x, group = g, formula = formula,
#' start = start, method = "likelihood")
#' lapply(fitLKL, coef)
#' lapply(fitLKL, logLik)
#' lapply(fitLKL, vcov)
#' }
#'
#' @keywords DIF
#' @export
#' @importFrom stats optim
#'
estimNLR <- function(y, match, group, formula, start, method){
  formM0 <- formula$M0$formula
  lowerM0 <- formula$M0$lower
  upperM0 <- formula$M0$upper
  startM0 <- start

  formM1 <- formula$M1$formula
  lowerM1 <- formula$M1$lower
  upperM1 <- formula$M1$upper
  startM1 <- start[formula$M1$parameters]

  x <- match
  g <- group

  param.likelM0 <- function(theta){
    h. <- parse(text = as.character(formM0)[3])
    h <- function(){eval(h.[[1]])}
    param <- as.list(theta)
    param[[length(param)+1]] <- x; names(param)[length(param)] <- "x"
    param[[length(param)+1]] <- g; names(param)[length(param)] <- "g"

    sapply(1:length(param), function(i) assign(names(param)[i], param[[i]], envir = environment(estimNLR)))

    n <- length(x)
    l <- -(1/n)*sum((y*log(h())) + ((1 - y)*log(1 - h())), na.rm = T)
  }
 param.likelM1 <- function(theta, pos = 1){
    h. <- parse(text = as.character(formM1)[3])
    h <- function(){eval(h.[[1]])}
    param <- as.list(theta)
    param[[length(param)+1]] <- x; names(param)[length(param)] <- "x"
    param[[length(param)+1]] <- g; names(param)[length(param)] <- "g"

    sapply(1:length(param), function(i) assign(names(param)[i], param[[i]], envir = environment(estimNLR)))

    n <- length(x)
    l <- -(1/n)*sum((y*log(h())) + ((1 - y)*log(1 - h())), na.rm = T)
  }

 M0 <- switch(method,
              nls = tryCatch(nls(formula = formM0,
                                 data = data.frame(y, x, g),
                                 algorithm = "port",
                                 start = startM0,
                                 lower = lowerM0,
                                 upper = upperM0),
                             error = function(e){cat("ERROR: ", conditionMessage(e), "\n")},
                             finally = ""),
              likelihood = tryCatch(lkl(par = startM0,
                                        fn = param.likelM0,
                                        lower = lowerM0,
                                        upper = upperM0),
                                    error = function(e){cat("ERROR: ", conditionMessage(e), "\n")},
                                    finally = ""))

 M1 <- switch(method,
              nls = tryCatch(nls(formula = formM1,
                                 data = data.frame(y, x, g),
                                 algorithm = "port",
                                 start = startM1,
                                 lower = lowerM1,
                                 upper = upperM1),
                             error = function(e){cat("ERROR: ", conditionMessage(e), "\n")},
                             finally = ""),
              likelihood = tryCatch(lkl(par = startM1,
                                        fn = param.likelM1,
                                        lower = lowerM1,
                                        upper = upperM1),
                                    error = function(e){cat("ERROR: ", conditionMessage(e), "\n")},
                                    finally = ""))
  return(list(M0, M1))
}

#' @aliases estimNLR
lkl <- function(par, fn, lower, upper){
  m <- optim(par = par,
             fn = fn,
             lower = lower,
             upper = upper,
             method = "L-BFGS-B",
             hessian = T)
  class(m) <- 'lkl'
  return(m)
}
#' @rdname lkl
vcov.lkl <- function(object){object$hessian}
#' @rdname lkl
logLik.lkl <- function(object){object$value}
#' @rdname lkl
coef.lkl <- function(object){object$par}




