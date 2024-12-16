#' DIF statistics for non-linear regression models.
#'
#' @description
#' Calculates likelihood ratio test statistics, F-test statistics, or Wald's
#' test statistics for DIF detection among dichotomous items using non-linear
#' regression models (generalized logistic regression models).
#'
#' @usage
#' NLR(Data, group, model, constraints = NULL, type = "all", method = "nls",
#'     match = "zscore", anchor = 1:ncol(Data), start, p.adjust.method = "none",
#'     test = "LR", alpha = 0.05, initboot = TRUE, nrBo = 20, sandwich = FALSE)
#'
#' @param Data data.frame or matrix: dataset in which rows represent scored
#'   examinee answers (\code{"1"} correct, \code{"0"} incorrect) and columns
#'   correspond to the items.
#' @param group numeric: a binary vector of a group membership (\code{"0"}
#'   for the reference group, \code{"1"} for the focal group).
#' @param model character: generalized logistic regression model to be fitted. See
#'   \strong{Details}.
#' @param constraints character: which parameters should be the same for both
#'   groups. Possible values are any combinations of parameters \code{"a"},
#'   \code{"b"}, \code{"c"}, and \code{"d"}. Default value is \code{NULL}.
#'   See \strong{Details}.
#' @param method character: an estimation method to be applied. The options are
#'   \code{"nls"} for non-linear least squares (default), \code{"mle"} for the
#'   maximum likelihood method using the \code{"L-BFGS-B"} algorithm with
#'   constraints, \code{"em"} for the maximum likelihood estimation with the EM
#'   algorithm, \code{"plf"} for the maximum likelihood estimation with the
#'   algorithm based on parametric link function, and \code{"irls"} for the maximum
#'   likelihood estimation with the iteratively reweighted least squares algorithm
#'   (available for the \code{"2PL"} model only). See \strong{Details}.
#' @param match character or numeric: matching criterion to be used as
#'   an estimate of the trait. It can be either \code{"zscore"} (default,
#'   standardized total score), \code{"score"} (total test score), or
#'   a numeric vector of the same length as a number of observations in
#'   the \code{Data}.
#' @param anchor character or numeric: specification of DIF free items. A vector
#'   of item identifiers (integers specifying the column number) specifying
#'   which items are currently considered as anchor (DIF free) items. Argument
#'   is ignored if the \code{match} is not \code{"zscore"} or \code{"score"}.
#' @param type character: type of DIF to be tested. Possible values are
#'   \code{"all"} for detecting difference in any parameter (default),
#'   \code{"udif"} for uniform DIF only (i.e., difference in difficulty
#'   parameter \code{"b"}),
#'   \code{"nudif"} for non-uniform DIF only (i.e., difference in discrimination
#'   parameter \code{"a"}),
#'   \code{"both"} for uniform and non-uniform DIF (i.e., difference in
#'   parameters \code{"a"} and \code{"b"}),
#'   or any combination of parameters \code{"a"}, \code{"b"}, \code{"c"}, and
#'   \code{"d"}. Can be specified as a single value (for all items) or as an
#'   item-specific vector.
#' @param p.adjust.method character: a method for a multiple comparison
#'   correction. Possible values are \code{"holm"}, \code{"hochberg"},
#'   \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"},
#'   \code{"fdr"}, and \code{"none"} (default). For more details see
#'   \code{\link[stats]{p.adjust}}.
#' @param start numeric: initial values for the estimation of item parameters. If
#'   not specified, starting values are calculated with the
#'   \code{\link[difNLR]{startNLR}} function. Otherwise, a list with as many
#'   elements as a number of items. Each element is a named numeric vector
#'   representing initial values for estimation of item parameters. Specifically,
#'   parameters \code{"a"}, \code{"b"}, \code{"c"}, and \code{"d"} are initial
#'   values for discrimination, difficulty, guessing, and inattention for the
#'   reference group. Parameters \code{"aDif"}, \code{"bDif"}, \code{"cDif"}, and
#'   \code{"dDif"} are then differences in these parameters between the reference
#'   and focal groups. For the \code{method = "irls"}, default initial values from
#'   the \code{\link[stats]{glm}} function are used.
#' @param test character: a statistical test to be performed for DIF detection.
#'   Can be either \code{"LR"} for the likelihood ratio test of a submodel
#'   (default), \code{"W"} for the Wald's test, or \code{"F"} for the F-test of
#'   a submodel.
#' @param alpha numeric: a significance level (the default is 0.05).
#' @param initboot logical: in the case of convergence issues, should starting
#'   values be re-calculated based on bootstrapped samples? (the default is
#'   \code{TRUE}; newly calculated initial values are applied only to
#'   items/models with convergence issues).
#' @param nrBo numeric: the maximal number of iterations for the calculation of
#'   starting values using bootstrapped samples (the default is 20).
#' @param sandwich logical: should the sandwich estimator be applied for
#'   computation of the covariance matrix of item parameters when using
#'   \code{method = "nls"}? (the default is \code{FALSE}).
#'
#' @details
#' The function calculates test statistics using a DIF detection procedure based
#' on non-linear regression models (i.e., extensions of the logistic regression
#' procedure; Swaminathan & Rogers, 1990; Drabinova & Martinkova, 2017).
#'
#' The unconstrained form of the 4PL generalized logistic regression model for
#' probability of correct answer (i.e., \eqn{Y_{pi} = 1}) using IRT
#' parameterization is
#' \deqn{P(Y_{pi} = 1|X_p, G_p) = (c_{iR} \cdot G_p + c_{iF} \cdot (1 - G_p)) +
#' (d_{iR} \cdot G_p + d_{iF} \cdot (1 - G_p) - c_{iR} \cdot G_p - c_{iF} \cdot
#' (1 - G_p)) / (1 + \exp(-(a_i + a_{i\text{DIF}} \cdot G_p) \cdot
#' (X_p - b_p - b_{i\text{DIF}} \cdot G_p))), }
#' where \eqn{X_p} is the matching criterion (e.g., standardized total score)
#' and \eqn{G_p} is a group membership variable for respondent \eqn{p}.
#' Parameters \eqn{a_i}, \eqn{b_i}, \eqn{c_{iR}}, and \eqn{d_{iR}} are
#' discrimination, difficulty, guessing, and inattention for the reference group
#' for item \eqn{i}. Terms \eqn{a_{i\text{DIF}}} and \eqn{b_{i\text{DIF}}} then
#' represent differences between the focal and reference groups in
#' discrimination and difficulty for item \eqn{i}. Terms \eqn{c_{iF}}, and
#' \eqn{d_{iF}} are guessing and inattention parameters for the focal group for
#' item \eqn{i}. In the case that there is no assumed difference between the
#' reference and focal group in the guessing or inattention parameters, the
#' terms \eqn{c_i} and \eqn{d_i} are used.
#'
#' Alternatively, intercept-slope parameterization may be applied:
#' \deqn{P(Y_{pi} = 1|X_p, G_p) = (c_{iR} \cdot G_p + c_{iF} \cdot (1 - G_p)) +
#' (d_{iR} \cdot G_p + d_{iF} \cdot (1 - G_p) - c_{iR} \cdot G_p - c_{iF} \cdot
#' (1 - G_p)) / (1 + \exp(-(\beta_{i0} + \beta_{i1} \cdot X_p +
#' \beta_{i2} \cdot G_p + \beta_{i3} \cdot X_p \cdot G_p))), }
#' where parameters \eqn{\beta_{i0}, \beta_{i1}, \beta_{i2}, \beta_{i3}} are
#' intercept, effect of the matching criterion, effect of the group membership,
#' and their mutual interaction, respectively.
#'
#' The \code{model} and \code{constraints} arguments can further constrain the
#' 4PL model. The arguments \code{model} and \code{constraints} can also be
#' combined. Both arguments can be specified as a single value (for all items)
#' or as an item-specific vector (where each element corresponds to one item).
#'
#' The \code{model} argument offers several predefined models. The options are as follows:
#' \code{Rasch} for 1PL model with discrimination parameter fixed on value 1 for both groups,
#' \code{1PL} for 1PL model with discrimination parameter set the same for both groups,
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
#' The function uses intercept-slope parameterization for the estimation via the
#' \code{\link[difNLR]{estimNLR}} function. Item parameters are then
#' re-calculated into the IRT parameterization using the delta method.
#'
#' The function offers either the non-linear least squares estimation via the
#' \code{\link[stats]{nls}} function (Drabinova & Martinkova, 2017; Hladka &
#' Martinkova, 2020), the maximum likelihood method with the \code{"L-BFGS-B"}
#' algorithm with constraints via the \code{\link[stats]{optim}} function (Hladka &
#' Martinkova, 2020), the maximum likelihood method with the EM algorithm (Hladka,
#' Martinkova, & Brabec, 2024), the maximum likelihood method with the algorithm
#' based on parametric link function (PLF, the default option; Hladka, Martinkova,
#' & Brabec, 2024), or the maximum likelihood method with the iteratively
#' reweighted least squares algorithm via the \code{\link[stats]{glm}} function.
#'
#' @return A list with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of the \code{test} statistics.}
#'   \item{\code{pval}}{the p-values by the \code{test}.}
#'   \item{\code{adjusted.pval}}{adjusted p-values by the \code{p.adjust.method}.}
#'   \item{\code{df}}{the degrees of freedom of the \code{test}.}
#'   \item{\code{test}}{used test.}
#'   \item{\code{par.m0}}{the matrix of estimated item parameters for the null model.}
#'   \item{\code{se.m0}}{the matrix of standard errors of item parameters for the null model.}
#'   \item{\code{cov.m0}}{list of covariance matrices of item parameters for the null model.}
#'   \item{\code{par.m1}}{the matrix of estimated item parameters for the alternative model.}
#'   \item{\code{se.m1}}{the matrix of standard errors of item parameters for the alternative model.}
#'   \item{\code{cov.m1}}{list of covariance matrices of item parameters for the alternative model.}
#'   \item{\code{conv.fail}}{numeric: a number of convergence issues.}
#'   \item{\code{conv.fail.which}}{the indicators of the items that did not converge.}
#'   \item{\code{ll.m0}}{log-likelihood of null model.}
#'   \item{\code{ll.m1}}{log-likelihood of alternative model.}
#'   \item{\code{startBo0}}{the binary matrix. Columns represent iterations of initial values
#'   re-calculations, rows represent items. The value of 0 means no convergence issue in the null model,
#'   1 means convergence issue in the null model.}
#'   \item{\code{startBo1}}{the binary matrix. Columns represent iterations of initial values
#'   re-calculations, rows represent items. The value of 0 means no convergence issue in the alternative model,
#'   1 means convergence issue in the alternative model.}
#' }
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
#' Karel Zvara \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' @references
#' Drabinova, A. & Martinkova, P. (2017). Detection of differential item
#' functioning with nonlinear regression: A non-IRT approach accounting for
#' guessing. Journal of Educational Measurement, 54(4), 498--517,
#' \doi{10.1111/jedm.12158}.
#'
#' Hladka, A. (2021). Statistical models for detection of differential item
#' functioning. Dissertation thesis. Faculty of Mathematics and Physics, Charles
#' University.
#'
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' Hladka, A., Martinkova, P., & Brabec, M. (2024). New iterative algorithms
#' for estimation of item functioning. Journal of Educational and Behavioral
#' Statistics. Accepted.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting differential item
#' functioning using logistic regression procedures. Journal of Educational
#' Measurement, 27(4), 361--370, \doi{10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # testing both DIF effects using the LR test (default)
#' # and the model with fixed guessing for both groups
#' NLR(Data, group, model = "3PLcg")
#'
#' # using the F test and Wald's test
#' NLR(Data, group, model = "3PLcg", test = "F")
#' NLR(Data, group, model = "3PLcg", test = "W")
#'
#' # using the Benjamini-Hochberg correction
#' NLR(Data, group, model = "3PLcg", p.adjust.method = "BH")
#'
#' # 4PL model with the same guessing and inattention
#' # to test uniform DIF
#' NLR(Data, group, model = "4PLcgdg", type = "udif")
#'
#' # 2PL model to test non-uniform DIF
#' NLR(Data, group, model = "2PL", type = "nudif")
#'
#' # 4PL model with fixed a and c parameters
#' # to test difference in parameter b
#' NLR(Data, group, model = "4PL", constraints = "ac", type = "b")
#'
#' # using various estimation algorithms
#' NLR(Data, group, model = "3PLcg", method = "nls")
#' NLR(Data, group, model = "3PLcg", method = "mle")
#' NLR(Data, group, model = "3PLcg", method = "em")
#' NLR(Data, group, model = "3PLcg", method = "plf")
#' NLR(Data, group, model = "2PL", method = "irls")
#' }
#'
#' @keywords DIF
#' @export
NLR <- function(Data, group, model, constraints = NULL, type = "all", method = "nls",
                match = "zscore", anchor = 1:ncol(Data), start, p.adjust.method = "none",
                test = "LR", alpha = 0.05, initboot = TRUE, nrBo = 20, sandwich = FALSE) {
  # if (method == "plf") {
  #   message("Note that the default option for `method` is now 'plf' instead of 'nls'.")
  # }

  if (match[1] == "zscore") {
    x <- as.vector(scale(apply(as.data.frame(Data[, anchor]), 1, sum)))
  } else {
    if (match[1] == "score") {
      x <- apply(as.data.frame(Data[, anchor]), 1, sum)
    } else {
      if (length(match) == dim(Data)[1]) {
        x <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore', or a vector of the same length as a number
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

  if (method == "irls") {
    parameterization <- rep("logistic", m)
  } else {
    parameterization <- rep("is", m)
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

  if (method == "irls") {
    start <- NULL
  } else if (missing(start) || is.null(start)) {
    start <- startNLR(Data, group, model, match = x, parameterization = parameterization)
  } else {
    if (!all(sapply(1:m, function(i) length(start[[i]]) == length(M[[i]]$M1$parameters))) ||
      !all(sapply(1:m, function(i) all(sort(names(start[[i]])) == sort(M[[i]]$M1$parameters))))) {
      warning("Invalid names of item parameters in 'start'. Initial values are calculated with the 'startNLR' function.", call. = FALSE)
      start <- startNLR(Data, group, model, match = x, parameterization = parameterization)
    }
  }

  m0 <- lapply(1:m, function(i) {
    start_tmp <- start[[i]]
    names_start_tmp <- names(start_tmp)
    if (!("cR" %in% M[[i]]$M0$parameters) & "cR" %in% names_start_tmp) {
      names(start_tmp)[names_start_tmp == "cR"] <- "c"
    }
    if (!("dR" %in% M[[i]]$M0$parameters) & "dR" %in% names_start_tmp) {
      names(start_tmp)[names_start_tmp == "dR"] <- "d"
    }

    estimNLR(
      y = Data[, i], match = x, group = group,
      formula = M[[i]]$M0$formula,
      method = method,
      start = structure(start_tmp[M[[i]]$M0$parameters],
        names = M[[i]]$M0$parameters
      ),
      lower = M[[i]]$M0$lower,
      upper = M[[i]]$M0$upper
    )
  })
  m1 <- lapply(1:m, function(i) {
    start_tmp <- start[[i]]
    names_start_tmp <- names(start_tmp)
    if (!("cR" %in% M[[i]]$M1$parameters) & "cR" %in% names_start_tmp) {
      names(start_tmp)[names_start_tmp == "cR"] <- "c"
    }
    if (!("dR" %in% M[[i]]$M1$parameters) & "dR" %in% names_start_tmp) {
      names(start_tmp)[names_start_tmp == "dR"] <- "d"
    }

    estimNLR(
      y = Data[, i], match = x, group = group,
      formula = M[[i]]$M1$formula,
      method = method,
      start = structure(start_tmp[M[[i]]$M1$parameters],
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

  # covariance structure
  cov.m0 <- cov.m1 <- as.list(rep(NA, m))
  if (method == "irls") {
    cov.m0[which(!cfM0)] <- suppressWarnings(lapply(lapply(m0[which(!cfM0)], summary), vcov))
    cov.m1[which(!cfM1)] <- suppressWarnings(lapply(lapply(m1[which(!cfM1)], summary), vcov))
  } else {
    cov.m0[which(!cfM0)] <- suppressWarnings(lapply(m0[which(!cfM0)], vcov, sandwich))
    cov.m1[which(!cfM1)] <- suppressWarnings(lapply(m1[which(!cfM1)], vcov, sandwich))
  }
  cov.fail0 <- which(sapply(cov.m0, is.null))
  cov.fail1 <- which(sapply(cov.m1, is.null))
  cov.fail <- sort(union(cov.fail0, cov.fail1))

  if (length(cov.fail) > 0) {
    warning(paste0(
      "Covariance matrix cannot be computed for ",
      ifelse(length(cov.fail) > 1, "items ", "item "),
      paste(cov.fail, collapse = ", "), "."
    ), call. = FALSE)
  }
  conv.m0 <- setdiff(1:m, unique(c(which(cfM0), which(sapply(cov.m0[which(!cfM0)], function(x) is.null(x))))))
  conv.m1 <- setdiff(1:m, unique(c(which(cfM1), which(sapply(cov.m1[which(!cfM1)], function(x) is.null(x))))))

  # standard errors
  se.m0 <- se.m1 <- as.list(rep(NA, m))
  se.m0[conv.m0] <- suppressWarnings(lapply(cov.m0[conv.m0], function(x) sqrt(diag(x))))
  se.m1[conv.m1] <- suppressWarnings(lapply(cov.m1[conv.m1], function(x) sqrt(diag(x))))

  se.fail0 <- which(sapply(se.m0, function(x) any(is.na(x))))
  se.fail1 <- which(sapply(se.m1, function(x) any(is.na(x))))
  se.fail <- sort(union(se.fail0, se.fail1))
  if (length(se.fail) > 0) {
    warning(paste0(
      "Standard errors of item parameter estimates cannot be computed for ",
      ifelse(length(se.fail) > 1, "items ", "item "),
      paste(se.fail, collapse = ", "), ". \n",
      "Computed covariance", ifelse(length(se.fail) > 1, " matrices are", " matrix is"), " probably not positive semi-definite."
    ), call. = FALSE)
  }

  # using starting values for bootstrapped samples
  if (initboot & (conv.fail > 0 || length(cov.fail) > 0 || length(se.fail) > 0)) {
    item.failed0 <- sort(union(which(cfM0), union(cov.fail0, se.fail0)))
    item.failed1 <- sort(union(which(cfM1), union(cov.fail1, se.fail1)))

    startM0 <- startM1 <- start
    startBo0 <- rep(0, m)
    startBo1 <- rep(0, m)
    startBo0[item.failed0] <- 1
    startBo1[item.failed1] <- 1

    for (i in 1:nrBo) {
      if (length(item.failed0) > 0 || length(item.failed1) > 0) {
        # re-calculating initial values
        set.seed(i)
        samp <- sample(1:dim(Data)[1], size = dim(Data)[1], replace = TRUE)
        startalt <- startNLR(Data[samp, ], group[samp], model,
          match = x,
          parameterization = parameterization
        )
        if (length(item.failed0) > 0) {
          startM0[item.failed0] <- startalt[item.failed0]
          m0[item.failed0] <- lapply(item.failed0, function(i) {
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
          if (method == "irls") {
            cov.m0[which(which(!cfM0) %in% item.failed0)] <- suppressWarnings(lapply(lapply(m0[which(!cfM0)], summary), vcov))
          } else {
            cov.m0[which(!cfM0)] <- suppressWarnings(lapply(m0[which(!cfM0)], vcov, sandwich))
          }
          cov.fail0 <- which(sapply(cov.m0, is.null))
          se.m0[conv.m0] <- suppressWarnings(lapply(cov.m0[conv.m0], function(x) sqrt(diag(x))))
          se.fail0 <- which(sapply(se.m0, function(x) any(is.na(x))))

          startBo0 <- cbind(startBo0, rep(0, m))
          startBo0[which(cfM0), i + 1] <- 1
          item.failed0 <- sort(union(which(cfM0), union(cov.fail0, se.fail0)))
        }
        if (length(item.failed1) > 0) {
          startM1[item.failed1] <- startalt[item.failed1]
          m1[item.failed1] <- lapply(item.failed1, function(i) {
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
          if (method == "irls") {
            cov.m1[which(!cfM1)] <- suppressWarnings(lapply(lapply(m1[which(!cfM1)], summary), vcov))
          } else {
            cov.m1[which(!cfM1)] <- suppressWarnings(lapply(m1[which(!cfM1)], vcov, sandwich))
          }
          cov.fail1 <- which(sapply(cov.m1, is.null))
          se.m1[conv.m1] <- suppressWarnings(lapply(cov.m1[conv.m1], function(x) sqrt(diag(x))))
          se.fail1 <- which(sapply(se.m1, function(x) any(is.na(x))))

          startBo1 <- cbind(startBo1, rep(0, m))
          startBo1[which(cfM1), i + 1] <- 1
          item.failed1 <- sort(union(which(cfM1), union(cov.fail1, se.fail1)))
        }
        item.failed <- sort(union(item.failed0, item.failed1))

        if (length(item.failed) > 0) {
          warning(
            paste0(
              "Convergence issues in ", ifelse(length(item.failed) > 0, "items ", "item "),
              paste0(item.failed, collapse = ", "), "\n",
              "Trying re-calculate starting values based on bootstrapped samples. "
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
    message("Starting values were calculated based on bootstrapped samples. ")
  }

  if (conv.fail > 0) {
    warning(paste("Convergence failure in item", conv.fail.which, "\n"))
  }

  # log-likelihood
  ll.m0 <- ll.m1 <- rep(NA, m)
  ll.m0[which(!cfM0)] <- sapply(m0[which(!cfM0)], logLik)
  ll.m1[which(!cfM1)] <- sapply(m1[which(!cfM1)], logLik)

  # parameters
  par.m0 <- lapply(1:m, function(i) {
    structure(rep(NA, length(M[[i]]$M0$parameters)),
      names = M[[i]]$M0$parameters
    )
  })
  par.m1 <- lapply(1:m, function(i) {
    structure(rep(NA, length(M[[i]]$M1$parameters)),
      names = M[[i]]$M1$parameters
    )
  })
  par.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], coef)
  par.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], coef)



  # delta method
  # dm.m0 <- lapply(1:m, function(i) {
  #   switch(parameterization[[i]],
  #     "logistic" = .deltamethod.NLR.log2irt(
  #       par = par.m0[[i]], cov = cov.m0[[i]],
  #       conv = (i %in% conv.m0),
  #       cov_fail = cfM0[i]
  #     ),
  #     "alternative" = .deltamethod.NLR.alt2irt(
  #       par = par.m0[[i]], cov = cov.m0[[i]],
  #       conv = (i %in% conv.m0),
  #       cov_fail = cfM0[i]
  #     ),
  #     "irt" = list(par = par.m0[[i]], cov = cov.m0[[i]], se = se.m0[[i]])
  #   )
  # })
  # par.m0 <- lapply(dm.m0, function(x) x$par)
  # cov.m0 <- lapply(dm.m0, function(x) x$cov)
  # se.m0 <- lapply(dm.m0, function(x) x$se)

  # dm.m1 <- lapply(1:m, function(i) {
  #   switch(parameterization[[i]],
  #     "logistic" = .deltamethod.NLR.log2irt(
  #       par = par.m1[[i]], cov = cov.m1[[i]],
  #       conv = (i %in% conv.m1),
  #       cov_fail = cfM1[i]
  #     ),
  #     "alternative" = .deltamethod.NLR.alt2irt(
  #       par = par.m1[[i]], cov = cov.m1[[i]],
  #       conv = (i %in% conv.m1),
  #       cov_fail = cfM1[i]
  #     ),
  #     "irt" = list(par = par.m1[[i]], cov = cov.m1[[i]], se = se.m1[[i]])
  #   )
  # })
  # par.m1 <- lapply(dm.m1, function(x) x$par)
  # cov.m1 <- lapply(dm.m1, function(x) x$cov)
  # se.m1 <- lapply(dm.m1, function(x) x$se)

  names(par.m1) <- names(par.m0) <- names(se.m1) <- names(se.m0) <- names(cov.m0) <- names(cov.m1) <- colnames(Data)

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

    Fval[which(!(cfM1 | cfM0))] <- sapply(
      which(!(cfM1 | cfM0)),
      function(l) ((RSS0[l] - RSS1[l]) / df[l, 1]) / (RSS1[l] / df[l, 2])
    )
    pval[which(!(cfM1 | cfM0))] <- sapply(
      which(!(cfM1 | cfM0)),
      function(l) (1 - pf(Fval[l], df[l, 1], df[l, 2]))
    )
  } else if (test == "LR") {
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
  } else if (test == "W") {
    pval <- Wval <- rep(NA, m)

    n0 <- sapply(1:m, function(i) length(M[[i]]$M0$parameters))
    n1 <- sapply(1:m, function(i) length(M[[i]]$M1$parameters))

    df <- n1 - n0

    Wval[which(!(cfM1 | cfM0))] <- sapply(
      which(!(cfM1 | cfM0)),
      function(l) {
        nams <- which(M[[l]]$M1$parameters %in% setdiff(M[[l]]$M1$parameters, M[[l]]$M0$parameters))
        V <- cov.m1[[l]][nams, nams]
        par <- par.m1[[l]][nams]
        par %*% solve(V) %*% par
      }
    )
    pval[which(!(cfM1 | cfM0))] <- sapply(
      which(!(cfM1 | cfM0)),
      function(l) (1 - pchisq(Wval[l], df[l]))
    )
  }
  # adjusted p-values
  adjusted.pval <- p.adjust(pval, method = p.adjust.method)

  results <- list(
    Sval = switch(test,
      "F" = Fval,
      "LR" = LRval,
      "W" = Wval
    ),
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

#' @noRd
.deltamethod.NLR.log2irt <- function(par, cov, conv, cov_fail) {
  if (conv) {
    par_names <- which(c("(Intercept)", "x", "g", "x:g") %in% names(par))
    par_tmp <- setNames(
      rep(0, 4),
      c("(Intercept)", "x", "g", "x:g")
    )
    par_tmp[par_names] <- par
    par_new <- setNames(
      c(
        par_tmp[2],
        -par_tmp[1] / par_tmp[2],
        (par_tmp[1] * par_tmp[4] - par_tmp[2] * par_tmp[3]) / (par_tmp[2] * (par_tmp[2] + par_tmp[4])),
        par_tmp[4]
      ),
      c("a", "b", "bDif", "aDif")
    )[par_names]

    if (cov_fail) {
      cov_new <- se_new <- NULL
    } else {
      cov_tmp <- matrix(
        0,
        ncol = 4, nrow = 4,
        dimnames = list(
          c("(Intercept)", "x", "g", "x:g"),
          c("(Intercept)", "x", "g", "x:g")
        )
      )
      cov_tmp[par_names, par_names] <- cov
      cov_new <- msm::deltamethod(
        list(~x2, ~ -x1 / x2, ~ (x1 * x4 - x2 * x3) / (x2 * (x2 + x4)), ~x4),
        par_tmp,
        cov_tmp,
        ses = FALSE
      )[par_names, par_names]
      colnames(cov_new) <- rownames(cov_new) <- c("a", "b", "bDif", "aDif")[par_names]
      se_new <- sqrt(diag(cov_new))
    }

    return(list(par = par_new, cov = cov_new, se = se_new))
  }
}

#' @noRd
.deltamethod.NLR.is2irt <- function(par, cov, conv, cov_fail) {
  if (conv) {
    par_names <- which(c("b0", "b1", "b2", "b3", "c", "cR", "cF", "d", "dR", "dF") %in% names(par))

    new_order <- sapply(c("a", "b", "aDif", "bDif", "c", "cR", "cF", "d", "dR", "dF"), function(x) {
      which(x == c("b", "a", "bDif", "aDif", "c", "cR", "cF", "d", "dR", "dF")[par_names])
    })
    new_order <- unlist(new_order[sapply(new_order, function(x) length(x) > 0)])

    par_tmp <- setNames(
      c(rep(0, 7), rep(1, 3)),
      c("b0", "b1", "b2", "b3", "c", "cR", "cF", "d", "dR", "dF")
    )
    par_tmp[par_names] <- par
    par_new <- setNames(
      c(
        -par_tmp[1] / par_tmp[2],
        par_tmp[2],
        (par_tmp[1] * par_tmp[4] - par_tmp[2] * par_tmp[3]) / (par_tmp[2] * (par_tmp[2] + par_tmp[4])),
        par_tmp[4],
        par_tmp[5],
        par_tmp[6],
        par_tmp[7],
        par_tmp[8],
        par_tmp[9],
        par_tmp[10]
      ),
      c("b", "a", "bDif", "aDif", "c", "cR", "cF", "d", "dR", "dF")
    )[par_names]

    if (cov_fail) {
      cov_new <- se_new <- NULL
    } else {
      cov_tmp <- matrix(
        0,
        ncol = 10, nrow = 10,
        dimnames = list(
          c("b0", "b1", "b2", "b3", "c", "cR", "cF", "d", "dR", "dF"),
          c("b0", "b1", "b2", "b3", "c", "cR", "cF", "d", "dR", "dF")
        )
      )
      cov_tmp[par_names, par_names] <- cov
      cov_new <- msm::deltamethod(
        list(
          ~ -x1 / x2, ~x2, ~ (x1 * x4 - x2 * x3) / (x2 * (x2 + x4)), ~x4,
          ~x5, ~x6, ~x7, ~x8, ~x9, ~x10
        ),
        par_tmp,
        cov_tmp,
        ses = FALSE
      )[par_names, par_names]
      colnames(cov_new) <- rownames(cov_new) <- c("b", "a", "bDif", "aDif", "c", "cR", "cF", "d", "dR", "dF")[par_names]
      cov_new <- cov_new[new_order, new_order]
      se_new <- sqrt(diag(cov_new))
    }

    return(list(par = par_new[new_order], cov = cov_new, se = se_new))
  }
}

#' @noRd
.deltamethod.NLR.irt2log <- function(par, cov, conv, cov_fail) {
  if (conv) {
    par_names <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif") %in% names(par)
    par_names_new <- as.logical(c(
      -par_names[1] * par_names[2],
      par_names[1],
      par_names[3],
      par_names[4],
      -par_names[1] * par_names[6] - par_names[5] * par_names[2] - par_names[5] * par_names[6],
      par_names[5],
      par_names[7],
      par_names[8]
    ))
    par_names <- which(par_names)
    par_names_new <- which(par_names_new)
    par_tmp <- setNames(
      rep(0, 8),
      c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
    )
    par_tmp[par_names] <- par
    par_new <- setNames(
      c(
        -par_tmp[1] * par_tmp[2],
        par_tmp[1],
        par_tmp[3],
        par_tmp[4],
        -par_tmp[1] * par_tmp[6] - par_tmp[5] * par_tmp[2] - par_tmp[5] * par_tmp[6],
        par_tmp[5],
        par_tmp[7],
        par_tmp[8]
      ),
      c("(Intercept)", "x", "c", "d", "g", "x:g", "cDif", "dDif")
    )[par_names_new]

    if (cov_fail) {
      cov_new <- se_new <- NULL
    } else {
      cov_tmp <- matrix(
        0,
        ncol = 8, nrow = 8,
        dimnames = list(
          c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif"),
          c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
        )
      )
      cov_tmp[par_names, par_names] <- cov
      cov_new <- msm::deltamethod(
        list(
          ~ -x1 * x2, ~x1, ~x3, ~x4,
          ~ -x1 * x6 - x5 * x2 - x5 * x6,
          ~x5, ~x7, ~x8
        ),
        par_tmp,
        cov_tmp,
        ses = FALSE
      )[par_names_new, par_names_new]
      colnames(cov_new) <- rownames(cov_new) <- c("(Intercept)", "x", "c", "d", "g", "x:g", "cDif", "dDif")[par_names]
      se_new <- sqrt(diag(cov_new))
    }

    return(list(par = par_new, cov = cov_new, se = se_new))
  }
}

#' @noRd
.deltamethod.NLR.alt2irt <- function(par, cov, conv, cov_fail) {
  if (conv) {
    # par_names <- names(par)
    par_old <- par
    names(par_old)[names(par_old) == "cR"] <- "c"
    names(par_old)[names(par_old) == "dR"] <- "d"
    par_new <- setNames(
      c(
        par_old["a"], par_old["b"], par_old["c"], par_old["d"],
        par_old["aDif"], par_old["bDif"],
        par_old["cF"] + par_old["c"], par_old["dF"] - par_old["d"]
      ),
      c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
    )

    par_names <- names(par_old)[names(par_old) %in% c("a", "b", "c", "d", "aDif", "bDif", "cF", "dF")]
    which_par_names <- which(names(par_old) %in% c("a", "b", "c", "d", "aDif", "bDif", "cF", "dF"))

    if (cov_fail) {
      cov_new <- se_new <- NULL
    } else {
      cov_old <- cov
      colnames(cov_old)[colnames(cov_old) == "cR"] <- "c"
      colnames(cov_old)[colnames(cov_old) == "dR"] <- "d"
      rownames(cov_old)[rownames(cov_old) == "cR"] <- "c"
      rownames(cov_old)[rownames(cov_old) == "dR"] <- "d"

      cov_tmp <- matrix(
        0,
        ncol = 8, nrow = 8,
        dimnames = list(
          c("a", "b", "c", "d", "aDif", "bDif", "cF", "dF"),
          c("a", "b", "c", "d", "aDif", "bDif", "cF", "dF")
        )
      )
      cov_tmp[par_names, par_names] <- cov
      cov_new <- msm::deltamethod(
        list(~x1, ~x2, ~x3, ~x4, ~x5, ~x6, ~ x7 - x3, ~ x8 - x4)[which_par_names],
        par_old,
        cov_tmp[par_names, par_names],
        ses = FALSE
      )
      colnames(cov_new) <- rownames(cov_new) <- par_names
      se_new <- sqrt(diag(cov_new))
    }
  } else {
    par_new <- par
    se_new <- sqrt(diag(cov))
    names(par_new) <- names(se_new) <- gsub("cF", "cDif", names(par))
    names(par_new) <- names(se_new) <- gsub("dF", "dDif", names(par))
    names(par_new) <- names(se_new) <- gsub("cR", "c", names(par))
    names(par_new) <- names(se_new) <- gsub("dR", "d", names(par))
  }

  return(list(par = par_new, cov = cov_new, se = se_new))
}
