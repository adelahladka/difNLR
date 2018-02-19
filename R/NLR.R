#' Non-Linear Regression DIF statistic.
#'
#' @aliases NLR
#'
#' @description Performs DIF detection procedure based on Non-Linear Regression and either
#' likelihood ratio test or F-test of submodel.
#'
#' @param Data numeric: either binary data matrix only, or the binary data matrix plus the vector of group .
#' See \strong{Details}.
#' @param group numeric: binary vector of group membership. \code{"0"} for reference group, \code{"1"} for focal group.
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param constraints character: which parameters should be the same for both groups. Default value is \code{NULL}. See \strong{Details}.
#' @param method character: what method should be used for estimation of parameters in \code{model}. The options are
#' \code{"nls"} for non-linear least squares (default) and \code{"likelihood"} for maximum likelihood method.
#' @param match specifies matching criterion. Can be either \code{"zscore"} (default, standardized total score),
#' \code{"score"} (total test score), or vector of the same length as number of observations in \code{Data}. See \strong{Details}.
#' @param anchor a vector of integers specifying which items are currently considered as anchor (DIF free) items. By
#' default, all items are considered as anchors. Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' See \strong{Details}.
#' @param type character: type of DIF to be tested. Possible values are \code{"both"} (default), \code{"udif"},
#' \code{"nudif"}, \code{"all"}, or combination of parameters 'a', 'b', 'c' and 'd'. See \strong{Details}.
#' @param p.adjust.method character: method for multiple comparison correction. See \strong{Details}.
#' @param start numeric: matrix with n rows (where n is the number of items) and 8 columns containing initial
#' item parameters estimates. See \strong{Details}.
#' @param test character: test to be performed for DIF detection (either \code{"LR"} (default), or \code{"F"}).
#' See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#'
#' @usage NLR(Data, group, model, constraints = NULL, type = "both", method = "nls",
#' match = "zscore", anchor = 1:ncol(Data), start, p.adjust.method = "none", test = "LR",
#' alpha = 0.05)
#'
#' @details
#' DIF detection procedure based on Non-Linear Regression is the extension
#' of Logistic Regression procedure (Swaminathan and Rogers, 1990).
#'
#' The \code{Data} is a matrix whose rows represents examinee scored answers
#' ("1" correct, "0" incorrect) and columns correspond to the items.
#' The \code{group} must be a vector of the same length as \code{nrow(data)}.
#'
#' The unconstrained form of 4PL generalized logistic regression model for probability of correct answer (i.e., y = 1) is
#' P(y = 1) = (c + cDif*g) + (d + dDif*g - c - cDif*g)/(1 + exp(-(a + aDif*g)*(x - b - bDif*g))),
#' where x is standardized total score (also called Z-score) and g is group membership. Parameters a, b, c and d
#' are discrimination, difficulty, guessing and inattention. Parameters aDif, bDif, cDif and dDif
#' then represetn differences between two groups in discrimination, difficulty, guessing and inattention.
#'
#' This 4PL model can be further constrained by \code{model} and \code{constraints} arguments.
#' The arguments \code{model} and \code{constraints} can be also combined.
#'
#' The \code{model} argument offers several predefined models. The options are as follows:
#' \code{Rasch} for 1PL model with discrimination parameter fixed on value 1 for both groups,
#' \code{1PL} for 1PL model with discrimination parameter fixed for both groups,
#' \code{2PL} for logistic regression model,
#' \code{3PLcg} for 3PL model with fixed guessing for both groups,
#' \code{3PLdg} for 3PL model with fixed inattention for both groups,
#' \code{3PLc} (alternatively also \code{3PL}) for 3PL regression model with guessing parameter,
#' \code{3PLd} for 3PL model with inattention parameter,
#' \code{4PLcgdg} for 4PL model with fixed guessing and inattention parameter for both groups,
#' \code{4PLcgd} (alternatively also \code{4PLd}) for 4PL model with fixed guessing for both groups,
#' \code{4PLcdg} (alternatively also \code{4PLc}) for 4PL model with fixed inattention for both groups,
#' or \code{4PL} for 4PL model.
#'
#' The \code{model} can be specified in more detail with \code{constraints} argument which specifies what
#' arguments should be fixed for both groups. For example, choice \code{"ad"} means that discrimination (a) and
#' inattention (d) are fixed for both groups and other parameters (b and c) are not.
#'
#' The \code{type} corresponds to type of DIF to be tested. Possible values are
#' \code{"both"} to detect any DIF caused by difference in difficulty or discrimination (i.e., uniform and/or non-uniform),
#' \code{"udif"} to detect only uniform DIF (i.e., difference in difficulty b),
#' \code{"nudif"} to detect only non-uniform DIF (i.e., difference in discrimination a), or
#' \code{"all"} to detect DIF caused by difference caused by any parameter that can differed between groups. The \code{type}
#' of DIF can be also specified in more detail by using combination of parameters a, b, c and d. For example, with an option
#' \code{"c"} for 4PL model only the difference in parameter c is tested.
#'
#' Argument \code{match} represents the matching criterion. It can be either the standardized test score (default, \code{"zscore"}),
#' total test score (\code{"score"}), or any other continuous or discrete variable of the same length as number of observations
#' in \code{Data}. Matching criterion is used in \code{NLR} function as a covariate in non-linear regression model.
#'
#' The \code{start} is a matrix with a number of rows equal to number of items and with 8 columns.
#' First 4 columns represent parameters (a, b, c, d) of generalized logistic regression model
#' for reference group. Last 4 columns represent differences of parameters (aDif, bDif, cDif, dDif)
#' of generalized logistic regression model between reference and focal group.  If not specified, starting
#' values are calculated with \code{startNLR} function.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the
#' \code{stats} package. Possible values are \code{"holm"}, \code{"hochberg"},
#' \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"},
#' \code{"none"}.
#'
#' In case that model considers difference in guessing or inattention parameter, the different parameterization is
#' used and parameters with standard errors are recalculated by delta method. However, covariance matrices stick with
#' alternative parameterization.
#'
#' @return A list with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of \code{test} statistics.}
#'   \item{\code{pval}}{the p-values by \code{test}.}
#'   \item{\code{adjusted.pval}}{adjusted p-values by \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of \code{test}.}
#'   \item{\code{test}}{used test.}
#'   \item{\code{par.m0}}{the matrix of estimated item parameters for m0 model.}
#'   \item{\code{se.m0}}{the matrix of standard errors of item parameters for m0 model.}
#'   \item{\code{cov.m0}}{list of covariance matrices of item parameters for m0 model.}
#'   \item{\code{par.m1}}{the matrix of estimated item parameters for m1 model.}
#'   \item{\code{se.m1}}{the matrix of standard errors of item parameters for m1 model.}
#'   \item{\code{cov.m1}}{list of covariance matrices of item parameters for m1 model.}
#'   \item{\code{conv.fail}}{numeric: number of convergence issues.}
#'   \item{\code{conv.fail.which}}{the indicators of the items which did not converge.}
#'   \item{\code{ll.m0}}{log-likelihood of m0 model.}
#'   \item{\code{ll.m1}}{log-likelihood of m1 model.}
#' }
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
#' Karel Zvara \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' @references
#' Drabinova, A. & Martinkova P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27, 361-370.
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @examples
#' \dontrun{
#' # loading data based on GMAT
#' data(GMAT)
#'
#' Data  <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using LR test (default)
#' # and model with fixed guessing for both groups
#' NLR(Data, group, model = "3PLcg")
#'
#' # Using F test
#' NLR(Data, group, model = "3PLcg", test = "F")
#'
#' # Testing both DIF effects with Benjamini-Hochberg correction
#' NLR(Data, group, model = "3PLcg", p.adjust.method = "BH")
#'
#' # 4PL model with the same guessing and inattention
#' # to test uniform DIF
#' NLR(Data, group, model = "4PLcgdg", type = "udif")
#'
#' # 2PL model to test non-uniform DIF
#' NLR(Data, group, model = "2PL", type = "nudif")
#'
#' # 4PL model with fixed a and c parameter
#' # to test difference in b
#' NLR(Data, group, model = "4PL", constraints = "ac", type = "b")
#'
#' # using maximum likelihood estimation method
#' NLR(Data, group, model = "3PLcg", method = "likelihood")
#' }
#' @keywords DIF
#' @export
#' @importFrom stats logLik
#' @importFrom msm deltamethod


NLR <- function(Data, group, model, constraints = NULL, type = "both",
                method = "nls", match = "zscore", anchor = 1:ncol(Data),
                start, p.adjust.method = "none", test = "LR", alpha = 0.05){

  if (match[1] == "zscore"){
    x <- scale(apply(as.data.frame(Data[, anchor]), 1, sum))
  } else {
    if (match[1] == "score"){
      x <- apply(as.data.frame(Data[, anchor]), 1, sum)
    } else {
      if (length(match) == dim(Data)[1]){
        x <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  if (model %in% c("3PLc", "3PL", "3PLd", "4PLcgd", "4PLd", "4PLcdg", "4PLc", "4PL")){
    parameterization <- "alternative"
  } else {
    parameterization <- "classic"
  }

  if (missing(start)){
    start <- startNLR(Data, group, model, match = x, parameterization = parameterization)
  }

  m <- ncol(Data)
  n <- nrow(Data)

  M <- formulaNLR(model = model, type = type, constraints, parameterization = parameterization)

  m0 <- lapply(1:m, function(i) estimNLR(y = Data[, i], match = x, group = group,
                                         formula = M$M0$formula, method = method,
                                         start = structure(start[i, M$M0$parameters], names = M$M0$parameters),
                                         lower = M$M0$lower, upper = M$M0$upper))
  m1 <- lapply(1:m, function(i) estimNLR(y = Data[, i], match = x, group = group,
                                         formula = M$M1$formula, method = method,
                                         start = structure(start[i, M$M1$parameters], names = M$M1$parameters),
                                         lower = M$M1$lower, upper = M$M1$upper))
  # convergence failures
  cfM0 <- unlist(lapply(m0, is.null)); cfM1 <- unlist(lapply(m1, is.null))
  conv.fail <- sum(cfM0, cfM1)
  conv.fail.which <- which(cfM0 | cfM1)

  if (conv.fail > 0) {
    warning(paste("Convergence failure in item", conv.fail.which, "\n"))
  }
  # test
  if (test == "F"){
    pval <- Fval <- rep(NA, m)

    df <- c(length(M$M0$parameters) - length(M$M1$parameters), n - length(M$M0$parameters))

    RSS0 <- rep(NA, m); RSS1 <- rep(NA, m)
    RSS0[which(!(cfM1 | cfM0))]  <- sapply(which(!(cfM1 | cfM0)), function(l) sum(residuals(m0[[l]])^2))
    RSS1[which(!(cfM1 | cfM0))]  <- sapply(which(!(cfM1 | cfM0)), function(l) sum(residuals(m1[[l]])^2))

    Fval <- ((RSS1 - RSS0)/df[1])/(RSS0/df[2])
    pval <- 1 - pf(Fval, df[1], df[2])
  } else {
    pval <- LRval <- rep(NA, m)

    df <- length(M$M0$parameters) - length(M$M1$parameters)
    LRval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 |  cfM0)),
                                           function(l) -2 * c(logLik(m1[[l]]) - logLik(m0[[l]])))
    pval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 | cfM0)),
                                          function(l) (1 - pchisq(LRval[l], df)))
  }
  # likelihood
  ll.m0 <- ll.m1 <- rep(NA, m)
  ll.m0[which(!cfM0)] <- sapply(m0[which(!cfM0)], logLik)
  ll.m1[which(!cfM1)] <- sapply(m1[which(!cfM1)], logLik)
  # adjusted p-values
  adjusted.pval <- p.adjust(pval, method = p.adjust.method)
  # parameters
  par.m1 <- se.m1 <- structure(data.frame(matrix(NA, nrow = m, ncol = length(M$M1$parameters))),
                               .Names = M$M1$parameters)
  par.m0 <- se.m0 <- structure(data.frame(matrix(NA, nrow = m, ncol = length(M$M0$parameters))),
                               .Names = M$M0$parameters)

  if (dim(par.m1)[2] == 1){
    par.m1[which(!cfM1), ] <- sapply(m1[which(!cfM1)], coef)
    par.m1 <- structure(data.frame(par.m1), names = unique(names(par.m1)))
  } else {
    par.m1[which(!cfM1), ] <- t(sapply(m1[which(!cfM1)], coef))
  }
  par.m0[which(!cfM0), ] <- t(sapply(m0[which(!cfM0)], coef))

  # covariance structure
  cov.m0 <- cov.m1 <- lapply(1:m, function(i) NA)
  cov.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], vcov)
  cov.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], vcov)

  # se
  if (dim(par.m1)[2] == 1){
    se.m1[which(!cfM1), ] <- sqrt(sapply(cov.m1[which(!cfM1)], diag))
    se.m1 <- structure(data.frame(se.m1), names = unique(names(se.m1)))
  } else {
    se.m1[which(!cfM1), ] <- sqrt(t(sapply(cov.m1[which(!cfM1)], diag)))
  }
  se.m0[which(!cfM0), ] <- sqrt(t(sapply(cov.m0[which(!cfM0)] , diag)))

  # delta method
  if ("cR" %in% colnames(par.m0)){
    if ("cF" %in% colnames(par.m0)){
      cDif0 <- par.m0$cF - par.m0$cR
      se.cDif0 <- sapply(which(!cfM0), function(i) deltamethod(~ x2 - x1, par.m0[i, c("cR", "cF")],
                                                               cov.m0[[i]][c("cR", "cF"), c("cR", "cF")]))
      par.m0$cF <- cDif0
      se.m0$cF[which(!cfM0)] <- se.cDif0
      colnames(par.m0)[colnames(par.m0) == "cF"] <- colnames(se.m0)[colnames(se.m0) == "cF"] <- "cDif"
    }
    colnames(par.m0)[colnames(par.m0) == "cR"] <- colnames(se.m0)[colnames(se.m0) == "cR"] <- "c"
  }
  if ("dR" %in% colnames(par.m0)){
    if ("dF" %in% colnames(par.m0)){
      dDif0 <- par.m0$dF - par.m0$dR
      se.dDif0 <- sapply(which(!cfM0), function(i) deltamethod(~ x2 - x1, par.m0[i, c("dR", "dF")],
                                                               cov.m0[[i]][c("dR", "dF"), c("dR", "dF")]))
      par.m0$dF <- dDif0
      se.m0$dF[which(!cfM0)] <- se.dDif0
      colnames(par.m0)[colnames(par.m0) == "dF"] <- colnames(se.m0)[colnames(se.m0) == "dF"] <- "dDif"
    }
    colnames(par.m0)[colnames(par.m0) == "dR"] <- colnames(se.m0)[colnames(se.m0) == "dR"] <- "d"
  }
  if ("cR" %in% colnames(par.m1)){
    if ("cF" %in% colnames(par.m1)){
      cDif1 <- par.m1$cF - par.m1$cR
      se.cDif1 <- sapply(1:m, function(i) deltamethod(~ x2 - x1, par.m1[i, c("cR", "cF")],
                                                      cov.m1[[i]][c("cR", "cF"), c("cR", "cF")]))
      par.m1$cF <- cDif1
      se.m1$cF[which(!cfM1)] <- se.cDif1
      colnames(par.m1)[colnames(par.m1) == "cF"] <- colnames(se.m1)[colnames(se.m1) == "cF"] <- "cDif"
    }
    colnames(par.m1)[colnames(par.m1) == "cR"] <- colnames(se.m1)[colnames(se.m1) == "cR"] <- "c"
  }
  if ("dR" %in% colnames(par.m1)){
    if ("dF" %in% colnames(par.m1)){
      dDif1 <- par.m1$dF - par.m1$dR
      se.dDif1 <- sapply(1:m, function(i) deltamethod(~ x2 - x1, par.m1[i, c("dR", "dF")],
                                                      cov.m1[[i]][c("dR", "dF"), c("dR", "dF")]))
      par.m1$dF <- dDif1
      se.m1$dF[which(!cfM1)] <- se.dDif1
      colnames(par.m1)[colnames(par.m1) == "dF"] <- colnames(se.m1)[colnames(se.m1) == "dF"] <- "dDif"
    }
    colnames(par.m1)[colnames(par.m1) == "dR"] <- colnames(se.m1)[colnames(se.m1) == "dR"] <- "d"
  }

  rownames(par.m1) <- rownames(par.m0) <- rownames(se.m1) <- rownames(se.m0) <- colnames(Data)

  results <- list(Sval = switch(test, "F" = Fval, "LR" = LRval),
                  pval = pval, adjusted.pval = adjusted.pval,
                  df = df, test = test,
                  par.m0 = par.m0, se.m0 = se.m0, cov.m0 = cov.m0,
                  par.m1 = par.m1, se.m1 = se.m1, cov.m1 = cov.m1,
                  conv.fail = conv.fail, conv.fail.which = conv.fail.which,
                  ll.m0 = ll.m0, ll.m1 = ll.m1)
  return(results)
}
