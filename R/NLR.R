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
#' @param initboot logical: in case of convergence issues, should be starting values recalculated based on
#' bootstraped samples? (default is \code{TRUE}). See \strong{Details}.
#' @param nrBo numeric: the maximal number of iterations for calculation of starting values using
#' bootstraped samples (default is 20).
#'
#' @usage NLR(Data, group, model, constraints = NULL, type = "both", method = "nls",
#' match = "zscore", anchor = 1:ncol(Data), start, p.adjust.method = "none", test = "LR",
#' alpha = 0.05, initboot = T, nrBo = 20)
#'
#' @details
#' DIF detection procedure based on Non-Linear Regression is the extension
#' of Logistic Regression procedure (Swaminathan and Rogers, 1990).
#'
#' The \code{Data} is a matrix which rows represents examinee scored answers
#' ("1" correct, "0" incorrect) and columns correspond to the items.
#' The \code{group} must be a vector of the same length as \code{nrow(data)}.
#'
#' The unconstrained form of 4PL generalized logistic regression model for probability of
#' correct answer (i.e., y = 1) is
#'
#' P(y = 1) = (c + cDif*g) + (d + dDif*g - c - cDif*g)/(1 + exp(-(a + aDif*g)*(x - b - bDif*g))),
#'
#' where x is standardized total score (also called Z-score) and g is group membership.
#' Parameters a, b, c and d are discrimination, difficulty, guessing and inattention.
#' Parameters aDif, bDif, cDif and dDif then represent differences between two groups in
#' discrimination, difficulty, guessing and inattention.
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
#' The \code{start} is a list with as many elements as number of items. Each element is a named numeric
#' vector representing initial values for parameter estimation. Specifically, parameters
#' a, b, c, and d are initial values for discrimination, difficulty, guessing and inattention
#' for reference group. Parameters aDif, bDif, cDif and dDif are then differences in these
#' parameters between reference and focal group. If not specified, starting
#' values are calculated with \code{\link[difNLR]{startNLR}} function.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the
#' \code{stats} package. Possible values are \code{"holm"}, \code{"hochberg"},
#' \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"},
#' \code{"none"}.
#'
#' In case of convergence issues, with an option \code{initboot = TRUE}, the starting values are
#' re-calculated based on bootstraped samples. Newly calculated initial values are applied only to
#' items/models with convergence issues.
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
#'   \item{\code{startBo0}}{the binary matrix. Columns represents iterations of initial values
#'   recalculations, rows represents items. The value of 0 means no convergence issue in m0 model,
#'   1 means convergence issue in m0 model.}
#'   \item{\code{startBo1}}{the binary matrix. Columns represents iterations of initial values
#'   recalculations, rows represents items. The value of 0 means no convergence issue in m1 model,
#'   1 means convergence issue in m1 model.}
#' }
#' @author
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' hladka@cs.cas.cz \cr
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
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
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
#'
#' @keywords DIF
#' @export
NLR <- function(Data, group, model, constraints = NULL, type = "both",
                method = "nls", match = "zscore", anchor = 1:ncol(Data),
                start, p.adjust.method = "none", test = "LR", alpha = 0.05,
                initboot = T, nrBo = 20){

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

  m <- dim(Data)[2]
  n <- dim(Data)[1]

  if (length(model) == 1){
    model <- rep(model, m)
  }
  if (is.null(constraints)){
    constraints <- as.list(rep(NA, m))
  }
  if (length(constraints) == 1){
      constraints <- rep(constraints, m)
  }
  if (length(type) == 1){
    type <- rep(type, m)
  }

  parameterization <- ifelse(model %in% c("3PLc", "3PL", "3PLd", "4PLcgd", "4PLd", "4PLcdg",
                                          "4PLc", "4PL"),
                             "alternative",
                             "classic")
  if (missing(start)){
    start <- startNLR(Data, group, model, match = x, parameterization = parameterization)
  } else {
    if (is.null(start)){
      start <- startNLR(Data, group, model, match = x, parameterization = parameterization)
    } else {
      for (i in 1:m){
        if (parameterization[i] == "alternative"){
          start[[i]]["cDif"] <- start[[i]]["c"] + start[[i]]["cDif"]
          start[[i]]["dDif"] <- start[[i]]["d"] + start[[i]]["dDif"]
          names(start[[i]]) <- c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF")
        } else {
          names(start[[i]]) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
        }
      }
    }
  }


  M <- lapply(1:m,
              function(i) formulaNLR(model = model[i],
                                     type = type[i],
                                     constraints = constraints[[i]],
                                     parameterization = parameterization[i]))

  m0 <- lapply(1:m, function(i) estimNLR(y = Data[, i], match = x, group = group,
                                         formula = M[[i]]$M0$formula,
                                         method = method,
                                         start = structure(start[[i]][M[[i]]$M0$parameters],
                                                           names = M[[i]]$M0$parameters),
                                         lower = M[[i]]$M0$lower,
                                         upper = M[[i]]$M0$upper))
  m1 <- lapply(1:m, function(i) estimNLR(y = Data[, i], match = x, group = group,
                                         formula = M[[i]]$M1$formula,
                                         method = method,
                                         start = structure(start[[i]][M[[i]]$M1$parameters],
                                                           names = M[[i]]$M1$parameters),
                                         lower = M[[i]]$M1$lower,
                                         upper = M[[i]]$M1$upper))
  # convergence failures
  cfM0 <- unlist(lapply(m0, is.null)); cfM1 <- unlist(lapply(m1, is.null))
  conv.fail <- sum(cfM0, cfM1)
  conv.fail.which <- which(cfM0 | cfM1)

  # using starting values for bootstraped samples

  if (initboot){
    startM0 <- startM1 <- start
    startBo0 <- rep(0, m); startBo1 <- rep(0, m)
    startBo0[which(cfM0)] <- 1; startBo1[which(cfM1)] <- 1
    for (i in 1:nrBo){
      if (conv.fail > 0){
        samp <- sample(1:dim(Data)[1], size = dim(Data)[1], replace = T)
        startalt <- startNLR(Data[samp, ], group[samp], model, match = x,
                             parameterization = parameterization)
        if (sum(cfM0) > 0){
          startM0[which(cfM0)] <- startalt[which(cfM0)]
          m0[which(cfM0)] <- lapply(which(cfM0), function(i) estimNLR(y = Data[, i], match = x, group = group,
                                                                      formula = M[[i]]$M0$formula,
                                                                      method = method,
                                                                      start = structure(startM0[[i]][M[[i]]$M0$parameters],
                                                                                        names = M[[i]]$M0$parameters),
                                                                      lower = M[[i]]$M0$lower,
                                                                      upper = M[[i]]$M0$upper))
          cfM0 <- unlist(lapply(m0, is.null))
          startBo0 <- cbind(startBo0, rep(0, m))
          startBo0[which(cfM0), i+1] <- 1
        }
        if (sum(cfM1) > 0){
          startM1[which(cfM1)] <- startalt[which(cfM1)]
          m1[which(cfM1)] <- lapply(which(cfM1), function(i) estimNLR(y = Data[, i], match = x, group = group,
                                                                      formula = M[[i]]$M1$formula,
                                                                      method = method,
                                                                      start = structure(startM1[[i]][M[[i]]$M1$parameters],
                                                                                        names = M[[i]]$M1$parameters),
                                                                      lower = M[[i]]$M1$lower,
                                                                      upper = M[[i]]$M1$upper))
          cfM1 <- unlist(lapply(m1, is.null))
          startBo1 <- cbind(startBo1, rep(0, m))
          startBo1[which(cfM1), i+1] <- 1

        }
        conv.fail <- sum(cfM0, cfM1)
        conv.fail.which <- which(cfM0 | cfM1)

        if (conv.fail > 0) {
          warning(paste("Convergence failure in item", conv.fail.which, "\n",
                        "Trying re-calculate starting values based on bootstraped samples. "),
                  call. = F)
        }
      } else {
        break
      }
    }
  } else {
    startBo0 <- startBo1 <- NULL
    i = 1
  }
  if (i > 1)
    message("Starting values were calculated based on bootstraped samples. ")

  if (conv.fail > 0) {
    warning(paste("Convergence failure in item", conv.fail.which, "\n"), call. = F)
  }
  # test
  if (test == "F"){
    pval <- Fval <- rep(NA, m)
    n0 <- sapply(1:m, function(i) length(M[[i]]$M0$parameters))
    n1 <- sapply(1:m, function(i) length(M[[i]]$M1$parameters))

    df <- cbind(n0 - n1, n - n0)

    RSS0 <- rep(NA, m); RSS1 <- rep(NA, m)
    RSS0[which(!(cfM1 | cfM0))]  <- sapply(which(!(cfM1 | cfM0)), function(l) sum(residuals(m0[[l]])^2))
    RSS1[which(!(cfM1 | cfM0))]  <- sapply(which(!(cfM1 | cfM0)), function(l) sum(residuals(m1[[l]])^2))

    Fval <- ((RSS1 - RSS0)/df[, 1])/(RSS0/df[, 2])
    pval <- 1 - pf(Fval, df[, 1], df[, 2])
  } else {
    pval <- LRval <- rep(NA, m)

    n0 <- sapply(1:m, function(i) length(M[[i]]$M0$parameters))
    n1 <- sapply(1:m, function(i) length(M[[i]]$M1$parameters))

    df <- n0 - n1

    LRval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 |  cfM0)),
                                           function(l) -2 * c(logLik(m1[[l]]) - logLik(m0[[l]])))
    pval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 | cfM0)),
                                          function(l) (1 - pchisq(LRval[l], df[l])))
  }
  # likelihood
  ll.m0 <- ll.m1 <- rep(NA, m)
  ll.m0[which(!cfM0)] <- sapply(m0[which(!cfM0)], logLik)
  ll.m1[which(!cfM1)] <- sapply(m1[which(!cfM1)], logLik)
  # adjusted p-values
  adjusted.pval <- p.adjust(pval, method = p.adjust.method)
  # parameters
  par.m1 <- se.m1 <- lapply(1:m, function(i) structure(rep(NA, length(M[[i]]$M1$parameters)),
                                                       names = M[[i]]$M1$parameters))

  par.m0 <- se.m0 <- lapply(1:m, function(i) structure(rep(NA, length(M[[i]]$M0$parameters)),
                                                       names = M[[i]]$M0$parameters))
  par.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], coef)
  par.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], coef)

  # covariance structure
  cov.m0 <- cov.m1 <- as.list(rep(NA, m))
  cov.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], vcov)
  cov.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], vcov)
  # se
  se.m1[which(!cfM1)] <- lapply(cov.m1[which(!cfM1)] , diag)
  se.m0[which(!cfM0)] <- lapply(cov.m0[which(!cfM0)] , diag)

  # delta method
  for (i in 1:m){
    if ("cR" %in% names(par.m0[[i]])){
      if ("cF" %in% names(par.m0[[i]])){
        cDif0 <- par.m0[[i]]['cF'] - par.m0[[i]]['cR']

        if (!cfM0[i]){
          se.cDif0 <- deltamethod(~ x2 - x1, par.m0[[i]][c("cR", "cF")],
                                  cov.m0[[i]][c("cR", "cF"), c("cR", "cF")])
          se.m0[[i]]['cF'] <- se.cDif0
        }
        par.m0[[i]]['cF'] <- cDif0

        names(par.m0[[i]])[names(par.m0[[i]]) == "cF"] <- names(se.m0[[i]])[names(se.m0[[i]]) == "cF"] <- "cDif"
      }
      names(par.m0[[i]])[names(par.m0[[i]]) == "cR"] <- names(se.m0[[i]])[names(se.m0[[i]]) == "cR"] <- "c"
    }
    if ("dR" %in% names(par.m0[[i]])){
      if ("dF" %in% names(par.m0[[i]])){
        cDif0 <- par.m0[[i]]['dF'] - par.m0[[i]]['dR']

        if (!cfM0[i]){
          se.cDif0 <- deltamethod(~ x2 - x1, par.m0[[i]][c("dR", "dF")],
                                  cov.m0[[i]][c("dR", "dF"), c("dR", "dF")])
          se.m0[[i]]['dF'] <- se.cDif0
        }
        par.m0[[i]]['dF'] <- cDif0

        names(par.m0[[i]])[names(par.m0[[i]]) == "dF"] <- names(se.m0[[i]])[names(se.m0[[i]]) == "dF"] <- "dDif"
      }
      names(par.m0[[i]])[names(par.m0[[i]]) == "dR"] <- names(se.m0[[i]])[names(se.m0[[i]]) == "dR"] <- "d"
    }
    if ("cR" %in% names(par.m1[[i]])){
      if ("cF" %in% names(par.m1[[i]])){
        cDif1 <- par.m1[[i]]['cF'] - par.m1[[i]]['cR']

        if (!cfM1[i]){
          se.cDif1 <- deltamethod(~ x2 - x1, par.m1[[i]][c("cR", "cF")],
                                  cov.m1[[i]][c("cR", "cF"), c("cR", "cF")])
          se.m1[[i]]['cF'] <- se.cDif1
        }
        par.m1[[i]]['cF'] <- cDif1

        names(par.m1[[i]])[names(par.m1[[i]]) == "cF"] <- names(se.m1[[i]])[names(se.m1[[i]]) == "cF"] <- "cDif"
      }
      names(par.m1[[i]])[names(par.m1[[i]]) == "cR"] <- names(se.m1[[i]])[names(se.m1[[i]]) == "cR"] <- "c"
    }
    if ("dR" %in% names(par.m1[[i]])){
      if ("dF" %in% names(par.m1[[i]])){
        cDif1 <- par.m1[[i]]['dF'] - par.m1[[i]]['dR']

        if (!cfM1[i]){
          se.cDif1 <- deltamethod(~ x2 - x1, par.m1[[i]][c("dR", "dF")],
                                  cov.m1[[i]][c("dR", "dF"), c("dR", "dF")])
          se.m1[[i]]['dF'] <- se.cDif1
        }
        par.m1[[i]]['dF'] <- cDif1

        names(par.m1[[i]])[names(par.m1[[i]]) == "dF"] <- names(se.m1[[i]])[names(se.m1[[i]]) == "dF"] <- "dDif"
      }
      names(par.m1[[i]])[names(par.m1[[i]]) == "dR"] <- names(se.m1[[i]])[names(se.m1[[i]]) == "dR"] <- "d"
    }
  }

  names(par.m1) <- names(par.m0) <- names(se.m1) <- names(se.m0) <- colnames(Data)

  results <- list(Sval = switch(test, "F" = Fval, "LR" = LRval),
                  pval = pval, adjusted.pval = adjusted.pval,
                  df = df, test = test,
                  par.m0 = par.m0, se.m0 = se.m0, cov.m0 = cov.m0,
                  par.m1 = par.m1, se.m1 = se.m1, cov.m1 = cov.m1,
                  conv.fail = conv.fail, conv.fail.which = conv.fail.which,
                  ll.m0 = ll.m0, ll.m1 = ll.m1,
                  startBo0 = startBo0, startBo1 = startBo1)
  return(results)
}
