#' DIF statistics based on non-linear regression model.
#'
#' @aliases NLR
#'
#' @description Calculates either DIF likelihood ratio statistics or F statistics for dichotomous
#' data based on non-linear regression model (generalized logistic regression model).
#'
#' @param Data data.frame or matrix: dataset which rows represent scored examinee answers (\code{"1"}
#' correct, \code{"0"} incorrect) and columns correspond to the items.
#' @param group numeric: binary vector of group membership. \code{"0"} for reference group, \code{"1"} for
#' focal group.
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param constraints character: which parameters should be the same for both groups. Possible values
#' are any combinations of parameters \code{"a"}, \code{"b"}, \code{"c"}, and \code{"d"}. Default value
#' is \code{NULL}. See \strong{Details}.
#' @param method character: method used to estimate parameters. The options are \code{"nls"} for
#' non-linear least squares (default) and \code{"likelihood"} for maximum likelihood method.
#' @param match character or numeric: matching criterion to be used as estimate of trait. Can be
#' either \code{"zscore"} (default, standardized total score), \code{"score"} (total test score),
#' or numeric vector of the same length as number of observations in \code{Data}.
#' @param anchor character or numeric: specification of DIF free items. A vector of item identifiers
#' (integers specifying the column  number) specifying which items are currently considered as anchor
#' (DIF free) items. Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' @param type character: type of DIF to be tested. Possible values are \code{"all"} for detecting
#' difference in any parameter (default), \code{"udif"} for uniform DIF only (i.e., difference in
#' difficulty parameter \code{"b"}), \code{"nudif"} for non-uniform DIF only (i.e., difference in
#' discrimination parameter \code{"a"}), \code{"both"} for uniform and non-uniform DIF (i.e.,
#' difference in parameters \code{"a"} and \code{"b"}), or combination of parameters \code{"a"},
#' \code{"b"}, \code{"c"}, and \code{"d"}. Can be specified as a single value (for all items) or as
#' an item-specific vector.
#' @param p.adjust.method character: method for multiple comparison correction. Possible values are
#' \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"},
#' \code{"fdr"}, and \code{"none"} (default). For more details see \code{\link[stats]{p.adjust}}.
#' @param start numeric: initial values for estimation of parameters. If not specified, starting
#' values are calculated with \code{\link[difNLR]{startNLR}} function. Otherwise, list with as many
#' elements as number of items. Each element is a named numeric vector of length 8 representing initial
#' values for parameter estimation. Specifically, parameters \code{"a"}, \code{"b"}, \code{"c"}, and
#' \code{"d"} are initial values for discrimination, difficulty, guessing, and inattention for reference
#' group. Parameters \code{"aDif"}, \code{"bDif"}, \code{"cDif"}, and \code{"dDif"} are then differences
#' in these parameters between reference and focal group.
#' @param test character: test to be performed for DIF detection. Can be either \code{"LR"} for
#' likelihood ratio test of a submodel (default), or \code{"F"} for F-test of a submodel.
#' @param alpha numeric: significance level (default is 0.05).
#' @param initboot logical: in case of convergence issues, should be starting values re-calculated based on
#' bootstraped samples? (default is \code{TRUE}; newly calculated initial values are applied only to
#' items/models with convergence issues).
#' @param nrBo numeric: the maximal number of iterations for calculation of starting values using
#' bootstraped samples (default is 20).
#'
#' @usage
#' NLR(Data, group, model, constraints = NULL, type = "all", method = "nls",
#'     match = "zscore", anchor = 1:ncol(Data), start, p.adjust.method = "none", test = "LR",
#'     alpha = 0.05, initboot = TRUE, nrBo = 20)
#'
#' @details
#' Calculation of the test statistics using DIF detection procedure based on non-linear regression
#' (extension of logistic regression procedure; Swaminathan and Rogers, 1990; Drabinova and Martinkova, 2017).
#'
#' The unconstrained form of 4PL generalized logistic regression model for probability of correct
#' answer (i.e., \eqn{y = 1}) is
#' \deqn{P(y = 1) = (c + cDif*g) + (d + dDif*g - c - cDif*g)/(1 + exp(-(a + aDif*g)*(x - b - bDif*g))), }
#' where \eqn{x} is by default standardized total score (also called Z-score) and \eqn{g} is a group membership.
#' Parameters \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{d} are discrimination, difficulty, guessing, and inattention.
#' Terms \eqn{aDif}, \eqn{bDif}, \eqn{cDif}, and \eqn{dDif} then represent differences between two groups
#' (reference and focal) in relevant parameters.
#'
#' This 4PL model can be further constrained by \code{model} and \code{constraints} arguments.
#' The arguments \code{model} and \code{constraints} can be also combined. Both arguments can
#' be specified as a single value (for all items) or as an item-specific vector (where each
#' element correspond to one item).
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
#' parameters should be fixed for both groups. For example, choice \code{"ad"} means that discrimination
#' (parameter \code{"a"}) and inattention (parameter \code{"d"}) are fixed for both groups and other parameters
#' (\code{"b"} and \code{"c"}) are not. The \code{NA} value for \code{constraints} means no constraints.
#'
#' In case that model considers difference in guessing or inattention parameter, the different parameterization is
#' used and parameters with standard errors are re-calculated by delta method.
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
#'   re-calculations, rows represents items. The value of 0 means no convergence issue in m0 model,
#'   1 means convergence issue in m0 model.}
#'   \item{\code{startBo1}}{the binary matrix. Columns represents iterations of initial values
#'   re-calculations, rows represents items. The value of 0 means no convergence issue in m1 model,
#'   1 means convergence issue in m1 model.}
#' }
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
#' Karel Zvara \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' @references
#' Drabinova, A. & Martinkova P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27(4), 361-370,
#' \url{https://doi.org/10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @examples
#' \dontrun{
#' # loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
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
NLR <- function(Data, group, model, constraints = NULL, type = "all",
                method = "nls", match = "zscore", anchor = 1:ncol(Data),
                start, p.adjust.method = "none", test = "LR", alpha = 0.05,
                initboot = TRUE, nrBo = 20) {
  if (match[1] == "zscore") {
    x <- scale(apply(as.data.frame(Data[, anchor]), 1, sum))
  } else {
    if (match[1] == "score") {
      x <- apply(as.data.frame(Data[, anchor]), 1, sum)
    } else {
      if (length(match) == dim(Data)[1]) {
        x <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore', or vector of the same length as number
             of observations in 'Data'!", call. = FALSE)
      }
    }
  }

  m <- dim(Data)[2]
  n <- dim(Data)[1]

  if (length(model) == 1) {
    model <- rep(model, m)
  }
  if (is.null(constraints)) {
    constraints <- as.list(rep(NA, m))
  }
  if (length(constraints) == 1) {
    constraints <- rep(constraints, m)
  }
  if (length(type) == 1) {
    type <- rep(type, m)
  }

  parameterization <- ifelse(model %in% c(
    "3PLc", "3PL", "3PLd", "4PLcgd", "4PLd", "4PLcdg",
    "4PLc", "4PL"
  ),
  "alternative",
  "classic"
  )
  if (missing(start)) {
    start <- startNLR(Data, group, model, match = x, parameterization = parameterization)
  } else {
    if (is.null(start)) {
      start <- startNLR(Data, group, model, match = x, parameterization = parameterization)
    } else {
      for (i in 1:m) {
        if (parameterization[i] == "alternative") {
          start[[i]]["cDif"] <- start[[i]]["c"] + start[[i]]["cDif"]
          start[[i]]["dDif"] <- start[[i]]["d"] + start[[i]]["dDif"]
          names(start[[i]]) <- c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF")
        } else {
          names(start[[i]]) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
        }
      }
    }
  }

  M <- lapply(
    1:m,
    function(i) {
      formulaNLR(
        model = model[i],
        type = type[i],
        constraints = constraints[[i]],
        parameterization = parameterization[i]
      )
    }
  )
  m0 <- lapply(1:m, function(i) {
    estimNLR(
      y = Data[, i], match = x, group = group,
      formula = M[[i]]$M0$formula,
      method = method,
      start = structure(start[[i]][M[[i]]$M0$parameters],
        names = M[[i]]$M0$parameters
      ),
      lower = M[[i]]$M0$lower,
      upper = M[[i]]$M0$upper
    )
  })
  m1 <- lapply(1:m, function(i) {
    estimNLR(
      y = Data[, i], match = x, group = group,
      formula = M[[i]]$M1$formula,
      method = method,
      start = structure(start[[i]][M[[i]]$M1$parameters],
        names = M[[i]]$M1$parameters
      ),
      lower = M[[i]]$M1$lower,
      upper = M[[i]]$M1$upper
    )
  })
  # convergence failures
  cfM0 <- unlist(lapply(m0, is.null))
  cfM1 <- unlist(lapply(m1, is.null))
  conv.fail <- sum(cfM0, cfM1)
  conv.fail.which <- which(cfM0 | cfM1)

  # using starting values for bootstraped samples

  if (initboot) {
    startM0 <- startM1 <- start
    startBo0 <- rep(0, m)
    startBo1 <- rep(0, m)
    startBo0[which(cfM0)] <- 1
    startBo1[which(cfM1)] <- 1
    set.seed(42)
    for (i in 1:nrBo) {
      if (conv.fail > 0) {
        samp <- sample(1:dim(Data)[1], size = dim(Data)[1], replace = T)
        startalt <- startNLR(Data[samp, ], group[samp], model,
          match = x,
          parameterization = parameterization
        )
        if (sum(cfM0) > 0) {
          startM0[which(cfM0)] <- startalt[which(cfM0)]
          m0[which(cfM0)] <- lapply(which(cfM0), function(i) {
            estimNLR(
              y = Data[, i], match = x, group = group,
              formula = M[[i]]$M0$formula,
              method = method,
              start = structure(startM0[[i]][M[[i]]$M0$parameters],
                names = M[[i]]$M0$parameters
              ),
              lower = M[[i]]$M0$lower,
              upper = M[[i]]$M0$upper
            )
          })
          cfM0 <- unlist(lapply(m0, is.null))
          startBo0 <- cbind(startBo0, rep(0, m))
          startBo0[which(cfM0), i + 1] <- 1
        }
        if (sum(cfM1) > 0) {
          startM1[which(cfM1)] <- startalt[which(cfM1)]
          m1[which(cfM1)] <- lapply(which(cfM1), function(i) {
            estimNLR(
              y = Data[, i], match = x, group = group,
              formula = M[[i]]$M1$formula,
              method = method,
              start = structure(startM1[[i]][M[[i]]$M1$parameters],
                names = M[[i]]$M1$parameters
              ),
              lower = M[[i]]$M1$lower,
              upper = M[[i]]$M1$upper
            )
          })
          cfM1 <- unlist(lapply(m1, is.null))
          startBo1 <- cbind(startBo1, rep(0, m))
          startBo1[which(cfM1), i + 1] <- 1
        }
        conv.fail <- sum(cfM0, cfM1)
        conv.fail.which <- which(cfM0 | cfM1)

        if (conv.fail > 0) {
          warning(paste(
            "Convergence failure in item", conv.fail.which, "\n",
            "Trying re-calculate starting values based on bootstraped samples. "
          ),
          call. = FALSE
          )
        }
      } else {
        break
      }
    }
  } else {
    startBo0 <- startBo1 <- NULL
    i <- 1
  }
  if (i > 1) {
    message("Starting values were calculated based on bootstraped samples. ")
  }

  if (conv.fail > 0) {
    warning(paste("Convergence failure in item", conv.fail.which, "\n"), call. = FALSE)
  }
  # test
  if (test == "F") {
    pval <- Fval <- rep(NA, m)
    n0 <- sapply(1:m, function(i) length(M[[i]]$M0$parameters))
    n1 <- sapply(1:m, function(i) length(M[[i]]$M1$parameters))

    df <- cbind(n1 - n0, n - n1)

    RSS0 <- rep(NA, m)
    RSS1 <- rep(NA, m)
    RSS0[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 | cfM0)), function(l) sum(residuals(m0[[l]])^2))
    RSS1[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 | cfM0)), function(l) sum(residuals(m1[[l]])^2))

    Fval <- ((RSS0 - RSS1) / df[, 1]) / (RSS1 / df[, 2])
    pval <- 1 - pf(Fval, df[, 1], df[, 2])
  } else {
    pval <- LRval <- rep(NA, m)

    n0 <- sapply(1:m, function(i) length(M[[i]]$M0$parameters))
    n1 <- sapply(1:m, function(i) length(M[[i]]$M1$parameters))

    df <- n1 - n0

    LRval[which(!(cfM1 | cfM0))] <- sapply(
      which(!(cfM1 | cfM0)),
      function(l) -2 * c(logLik(m0[[l]]) - logLik(m1[[l]]))
    )
    pval[which(!(cfM1 | cfM0))] <- sapply(
      which(!(cfM1 | cfM0)),
      function(l) (1 - pchisq(LRval[l], df[l]))
    )
  }
  # likelihood
  ll.m0 <- ll.m1 <- rep(NA, m)
  ll.m0[which(!cfM0)] <- sapply(m0[which(!cfM0)], logLik)
  ll.m1[which(!cfM1)] <- sapply(m1[which(!cfM1)], logLik)

  # R2 computation
  # N <- dim(Data)[1]
  # r2cs <- 1 - exp(-2/N * (ll.m0 - ll.m1)) # Cox and Snell
  # r2n  <- r2cs / (1 - exp(2*ll.m1/N))     # Nagelkerge
  #
  # symnum(r2n, c(0, 0.13, 0.26, 1), symbols = c("A", "B", "C"))  # Zumbo & Thomas
  # symnum(r2n, c(0, 0.035, 0.07, 1), symbols = c("A", "B", "C")) # Jodoin & Gierl

  # adjusted p-values
  adjusted.pval <- p.adjust(pval, method = p.adjust.method)
  # parameters
  par.m1 <- se.m1 <- lapply(1:m, function(i) {
    structure(rep(NA, length(M[[i]]$M1$parameters)),
      names = M[[i]]$M1$parameters
    )
  })
  par.m0 <- se.m0 <- lapply(1:m, function(i) {
    structure(rep(NA, length(M[[i]]$M0$parameters)),
      names = M[[i]]$M0$parameters
    )
  })
  par.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], coef)
  par.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], coef)

  # covariance structure
  cov.m0 <- cov.m1 <- as.list(rep(NA, m))
  cov.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], vcov)
  cov.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], vcov)
  # se
  se.m1[which(!cfM1)] <- lapply(cov.m1[which(!cfM1)], function(x) sqrt(diag(x)))
  se.m0[which(!cfM0)] <- lapply(cov.m0[which(!cfM0)], function(x) sqrt(diag(x)))

  # delta method
  for (i in which(!cfM0)) {
    if (parameterization[i] == "alternative") {
      nms.m0 <- which(c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF") %in% names(par.m0[[i]]))

      par.tmp.m0 <- setNames(
        rep(0, 8),
        c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF")
      )
      par.tmp.m0[nms.m0] <- par.m0[[i]]
      par.m0[[i]] <- setNames(
        c(par.tmp.m0[1:6], par.tmp.m0[7] - par.tmp.m0[3], par.tmp.m0[8] - par.tmp.m0[4]),
        c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
      )[nms.m0]

      cov.tmp.m0 <- matrix(
        0,
        ncol = 8, nrow = 8,
        dimnames = list(
          c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF"),
          c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF")
        )
      )
      cov.tmp.m0[nms.m0, nms.m0] <- cov.m0[[i]]
      cov.m0[[i]] <- deltamethod(
        list(~x1, ~x2, ~x3, ~x4, ~x5, ~x6, ~ x7 - x3, ~ x8 - x4),
        par.tmp.m0,
        cov.tmp.m0,
        ses = FALSE
      )[nms.m0, nms.m0]
      colnames(cov.m0[[i]]) <- rownames(cov.m0[[i]]) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")[nms.m0]
      se.m0[[i]] <- sqrt(diag(cov.m0[[i]]))
    }
  }

  for (i in which(!cfM1)) {
    if (parameterization[i] == "alternative") {
      nms.m1 <- which(c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF") %in% names(par.m1[[i]]))

      par.tmp.m1 <- setNames(
        rep(0, 8),
        c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF")
      )
      par.tmp.m1[nms.m1] <- par.m1[[i]]
      par.m1[[i]] <- setNames(
        c(par.tmp.m1[1:6], par.tmp.m1[7] - par.tmp.m1[3], par.tmp.m1[8] - par.tmp.m1[4]),
        c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
      )[nms.m1]

      cov.tmp.m1 <- matrix(
        0,
        ncol = 8, nrow = 8,
        dimnames = list(
          c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF"),
          c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF")
        )
      )
      cov.tmp.m1[nms.m1, nms.m1] <- cov.m1[[i]]
      cov.m1[[i]] <- deltamethod(
        list(~x1, ~x2, ~x3, ~x4, ~x5, ~x6, ~ x7 - x3, ~ x8 - x4),
        par.tmp.m1,
        cov.tmp.m1,
        ses = FALSE
      )[nms.m1, nms.m1]
      colnames(cov.m1[[i]]) <- rownames(cov.m1[[i]]) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")[nms.m1]
      se.m1[[i]] <- sqrt(diag(cov.m1[[i]]))
    }
  }

  names(par.m1) <- names(par.m0) <- names(se.m1) <- names(se.m0) <- names(cov.m0) <- names(cov.m1) <- colnames(Data)

  results <- list(
    Sval = switch(test, "F" = Fval, "LR" = LRval),
    pval = pval, adjusted.pval = adjusted.pval,
    df = df, test = test,
    par.m0 = par.m0, se.m0 = se.m0, cov.m0 = cov.m0,
    par.m1 = par.m1, se.m1 = se.m1, cov.m1 = cov.m1,
    conv.fail = conv.fail, conv.fail.which = conv.fail.which,
    ll.m0 = ll.m0, ll.m1 = ll.m1,
    startBo0 = startBo0, startBo1 = startBo1
  )
  return(results)
}
