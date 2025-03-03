#' DIF detection using non-linear regression method.
#'
#' @description
#' Performs DIF detection procedure in dichotomous data based on non-linear
#' regression model (generalized logistic regression) and either
#' likelihood-ratio test, F-test, or Wald's test of a submodel.
#'
#' @usage
#' difNLR(Data, group, focal.name, model, constraints, type = "all",
#'        method = "nls", match = "zscore", anchor = NULL, purify = FALSE,
#'        nrIter = 10, test = "LR", alpha = 0.05, p.adjust.method = "none", start,
#'        initboot = TRUE, nrBo = 20, sandwich = FALSE)
#'
#' @param Data data.frame or matrix: dataset in which rows represent scored
#'   examinee answers (\code{"1"} correct, \code{"0"} incorrect) and columns
#'   correspond to the items. In addition, \code{Data} can hold the vector of group
#'   membership.
#' @param group numeric or character: a binary vector of the same length as
#'   \code{nrow(Data)} or a column identifier in the \code{Data}.
#' @param focal.name numeric or character: indicates the level of the \code{group}
#'   corresponding to the focal group.
#' @param model character: generalized logistic regression model to be fitted.
#'   See \strong{Details}.
#' @param constraints character: which parameters should be the same for both
#'   groups. Possible values are any combinations of parameters \code{"a"},
#'   \code{"b"}, \code{"c"}, and \code{"d"}. See \strong{Details}.
#' @param type character: type of DIF to be tested. Possible values are
#'   \code{"all"} for detecting differences in any parameters (default),
#'   \code{"udif"} for uniform DIF only (i.e., difference in difficulty
#'   parameter \code{"b"}), \code{"nudif"} for non-uniform DIF only (i.e.,
#'   difference in discrimination parameter \code{"a"}), \code{"both"} for
#'   uniform and non-uniform DIF (i.e., difference in parameters \code{"a"} and
#'   \code{"b"}), or a combination of parameters \code{"a"}, \code{"b"},
#'   \code{"c"}, and \code{"d"}. Can be specified as a single value (for all
#'   items) or as an item-specific vector.
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
#' @param anchor character or numeric: specification of DIF free items. Either
#'   \code{NULL} (default), or a vector of item identifiers (integers specifying
#'   the column number) specifying which items are currently considered as anchor
#'   (DIF free) items. Argument is ignored if the \code{match} is not
#'   \code{"zscore"} or \code{"score"}.
#' @param purify logical: should the item purification be applied? (the default is
#'   \code{FALSE}).
#' @param nrIter numeric: the maximal number of iterations in the item
#'   purification (the default is 10).
#' @param test character: a statistical test to be performed for DIF detection.
#'   Can be either \code{"LR"} for the likelihood ratio test of a submodel
#'   (default), \code{"W"} for the Wald's test, or \code{"F"} for the F-test of
#'   a submodel.
#' @param alpha numeric: a significance level (the default is 0.05).
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
#' DIF detection procedure based on non-linear regression is the extension of
#' the logistic regression procedure (Swaminathan & Rogers, 1990) accounting for
#' possible guessing and/or inattention when responding (Drabinova & Martinkova,
#' 2017; Hladka & Martinkova, 2020).
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
#' The underlying generalized logistic regression model can be further specified in
#' more detail with the \code{constraints} argument which specifies what parameters
#' should be fixed for both groups. For example, a choice \code{"ad"} means that
#' discrimination (parameter \code{"a"}) and inattention (parameter \code{"d"}) are
#' fixed (and estimated for) both groups and other parameters (\code{"b"} and
#' \code{"c"}) are not. The \code{NA} value for \code{constraints} means no
#' constraints.
#'
#' Missing values are allowed but discarded for an item estimation. They must be
#' coded as \code{NA} for both, the \code{Data} and \code{group} arguments.
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
#' @return
#' The \code{difNLR()} function returns an object of class \code{"difNLR"}. The
#' output, including values of the test statistics, p-values, and items detected
#' as function differently, is displayed by the \code{print()} method.
#'
#' Object of class \code{"difNLR"} is a list with the following components:
#' \describe{
#'   \item{\code{Sval}}{the values of the \code{test} statistics.}
#'   \item{\code{nlrPAR}}{the item parameter estimates of the final model.}
#'   \item{\code{nlrSE}}{the standard errors of the item parameter estimates of the final model.}
#'   \item{\code{parM0}}{the item parameter estimates of the null (smaller) model.}
#'   \item{\code{seM0}}{the standard errors of item parameter estimates of the null (smaller) model.}
#'   \item{\code{covM0}}{the covariance matrices of the item parameter estimates of the null (smaller) model.}
#'   \item{\code{llM0}}{the log-likelihood values of the null (smaller) model.}
#'   \item{\code{parM1}}{the item parameter estimates of the alternative (larger) model.}
#'   \item{\code{seM1}}{the standard errors of the item parameter estimates of the alternative (larger) model.}
#'   \item{\code{covM1}}{the covariance matrices of the item parameter estimates of alternative (larger) model.}
#'   \item{\code{llM1}}{the log-likelihood values of the alternative (larger) model.}
#'   \item{\code{DIFitems}}{either the column identifiers of the items which were detected as DIF, or \code{"No DIF item detected"} in the case no item was detected as function differently.}
#'   \item{\code{model}}{fitted model.}
#'   \item{\code{constraints}}{constraints for the \code{model}.}
#'   \item{\code{type}}{character: type of DIF that was tested. If a combination of the item parameters was specified, the value is \code{"other"}.}
#'   \item{\code{types}}{character: the parameters (specified by user, \code{type} has value \code{"other"}) which were tested for difference.}
#'   \item{\code{p.adjust.method}}{character: a method for the multiple comparison correction which was applied.}
#'   \item{\code{pval}}{the p-values by the \code{test}.}
#'   \item{\code{adjusted.pval}}{adjusted p-values by the \code{p.adjust.method}.}
#'   \item{\code{df}}{the degrees of freedom of the \code{test}.}
#'   \item{\code{test}}{used test.}
#'   \item{\code{purification}}{\code{purify} value.}
#'   \item{\code{nrPur}}{number of iterations in item purification process. Returned only if \code{purify} is \code{TRUE}.}
#'   \item{\code{difPur}}{a binary matrix with one row per iteration of item purification and one column per item.
#'   \code{"1"} in i-th row and j-th column means that j-th item was identified as DIF in i-th iteration. Returned only if \code{purify} is \code{TRUE}.}
#'   \item{\code{conv.puri}}{logical: indicating whether item purification process converged before the maximal number \code{nrIter} of iterations. Returned only if \code{purify} is \code{TRUE}.}
#'   \item{\code{method}}{used estimation method.}
#'   \item{\code{conv.fail}}{numeric: number of convergence issues.}
#'   \item{\code{conv.fail.which}}{the identifiers of the items which did not converge.}
#'   \item{\code{alpha}}{numeric: significance level.}
#'   \item{\code{Data}}{the data matrix.}
#'   \item{\code{group}}{the vector of group membership.}
#'   \item{\code{group.names}}{names of groups.}
#'   \item{\code{match}}{matching criterion.}
#' }
#'
#' Several methods are available for an object of the \code{"difNLR"} class (e.g.,
#' \code{methods(class = "difNLR")}).
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
#' Statistics. Online first, \doi{10.3102/10769986241312354}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting differential item
#' functioning using logistic regression procedures. Journal of Educational
#' Measurement, 27(4), 361--370, \doi{10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso
#' \code{\link[difNLR]{plot.difNLR}} for a graphical representation of item characteristic curves and DIF statistics. \cr
#' \code{\link[difNLR]{coef.difNLR}} for an extraction of item parameters with their standard errors in various parameterizations. \cr
#' \code{\link[difNLR]{predict.difNLR}} for prediction. \cr
#' \code{\link[difNLR]{fitted.difNLR}} and \code{\link[difNLR]{residuals.difNLR}} for an extraction of fitted
#' values and residuals. \cr
#' \code{\link[difNLR]{logLik.difNLR}}, \code{\link[difNLR]{AIC.difNLR}}, \code{\link[difNLR]{BIC.difNLR}}
#' for an extraction of log-likelihood values and information criteria. \cr
#'
#' \code{\link[stats]{p.adjust}} for multiple comparison corrections. \cr
#' \code{\link[stats]{nls}} for a nonlinear least squares estimation. \cr
#' \code{\link[difNLR]{startNLR}} for a calculation of initial values of fitting algorithms in \code{difNLR()}.
#'
#' @examples
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#' \dontrun{
#' # graphical devices
#' plot(x, item = x$DIFitems)
#' plot(x, item = "Item1")
#' plot(x, item = 1, group.names = c("Group 1", "Group 2"))
#' plot(x, plot.type = "stat")
#'
#' # coefficients
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
#' coef(x, item = 1, CI = 0)
#'
#' # fitted values
#' fitted(x)
#' fitted(x, item = 1)
#'
#' # residuals
#' residuals(x)
#' residuals(x, item = 1)
#'
#' # predicted values
#' predict(x)
#' predict(x, item = 1)
#'
#' # predicted values for new subjects
#' predict(x, item = 1, match = 0, group = c(0, 1))
#'
#' # AIC, BIC, log-likelihood
#' AIC(x)
#' BIC(x)
#' logLik(x)
#'
#' # AIC, BIC, log-likelihood for the first item
#' AIC(x, item = 1)
#' BIC(x, item = 1)
#' logLik(x, item = 1)
#'
#' # testing both DIF effects using Wald test and
#' # 3PL model with fixed guessing for groups
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "W")
#'
#' # testing both DIF effects using F test and
#' # 3PL model with fixed guessing for groups
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "F")
#'
#' # testing both DIF effects using
#' # 3PL model with fixed guessing for groups and sandwich estimator
#' # of the covariance matrices
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", sandwich = TRUE)
#'
#' # testing both DIF effects using LR test,
#' # 3PL model with fixed guessing for groups
#' # and Benjamini-Hochberg correction
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", p.adjust.method = "BH")
#'
#' # testing both DIF effects using LR test,
#' # 3PL model with fixed guessing for groups
#' # and item purification
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", purify = TRUE)
#'
#' # testing both DIF effects using 3PL model with fixed guessing for groups
#' # and total score as matching criterion
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "score")
#'
#' # testing uniform DIF effects using 4PL model with the same
#' # guessing and inattention
#' difNLR(Data, group, focal.name = 1, model = "4PLcgdg", type = "udif")
#'
#' # testing non-uniform DIF effects using 2PL model
#' difNLR(Data, group, focal.name = 1, model = "2PL", type = "nudif")
#'
#' # testing difference in parameter b using 4PL model with fixed
#' # a and c parameters
#' difNLR(Data, group, focal.name = 1, model = "4PL", constraints = "ac", type = "b")
#'
#' # testing both DIF effects using LR test,
#' # 3PL model with fixed guessing for groups
#' # using maximum likelihood estimation with
#' # the L-BFGS-B algorithm, the EM algorithm, and the PLF algorithm
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "mle")
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "em")
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "plf")
#'
#' # testing both DIF effects using LR test and 2PL model
#' # using maximum likelihood estimation with iteratively reweighted least squares algorithm
#' difNLR(Data, group, focal.name = 1, model = "2PL", method = "irls")
#' }
#'
#' @keywords DIF
#' @export
difNLR <- function(Data, group, focal.name, model, constraints, type = "all", method = "nls",
                   match = "zscore", anchor = NULL, purify = FALSE, nrIter = 10,
                   test = "LR", alpha = 0.05, p.adjust.method = "none", start,
                   initboot = TRUE, nrBo = 20, sandwich = FALSE) {
  if (any(type == "nudif" & model == "1PL")) {
    stop("Detection of non-uniform DIF is not possible with the 1PL model.", call. = FALSE)
  }
  if (any(type == "nudif" & model == "Rasch")) {
    stop("Detection of non-uniform DIF is not possible with the Rasch model.", call. = FALSE)
  }
  # input check
  # model
  if (missing(model)) {
    stop("'model' is missing", call. = FALSE)
  }
  # constraints
  if (missing(constraints)) {
    constraints <- NULL
  }
  # matching criterion
  if (!(match[1] %in% c("score", "zscore"))) {
    if (is.null(dim(Data))) {
      n <- length(Data)
    } else {
      n <- dim(Data)[1]
    }
    if (length(match) != n) {
      stop("Invalid value for 'match'. Possible values are 'score', 'zscore', or a vector of the same length as a number
           of observations in 'Data'.", call. = FALSE)
    }
  }
  # purification
  if (purify & !(match[1] %in% c("score", "zscore"))) {
    stop("Purification is not allowed when the matching variable is not 'zscore' or 'score'.", call. = FALSE)
  }
  # test
  if (!test %in% c("F", "LR", "W") | !is.character(type)) {
    stop("Invalid value for 'test'. Test for DIF detection must be either 'LR', 'W', or 'F'.", call. = FALSE)
  }
  # significance level
  if (alpha > 1 | alpha < 0) {
    stop("Argument 'alpha' must be between 0 and 1.", call. = FALSE)
  }
  # starting values
  if (missing(start)) {
    start <- NULL
  }
  # starting values with bootstrapped samples
  if (initboot) {
    if (nrBo < 1) {
      stop("The maximal number of iterations for a calculation of starting values using bootstrapped
           samples 'nrBo' need to be greater than 1.", call. = FALSE)
    }
  }
  # estimation method
  if (method == "likelihood") {
    method <- "mle"
  }
  if (!(method %in% c("nls", "mle", "em", "plf", "irls"))) {
    stop("Invalid value for 'method'. Estimation method must be either 'nls', 'mle', 'em', 'plf', or 'irls'. ", call. = FALSE)
  }

  # internal NLR function
  internalNLR <- function() {
    if (length(group) == 1) {
      if (is.numeric(group)) {
        GROUP <- Data[, group]
        DATA <- as.data.frame(Data[, (1:dim(Data)[2]) != group])
        colnames(DATA) <- colnames(Data)[(1:dim(Data)[2]) != group]
      } else {
        GROUP <- Data[, colnames(Data) == group]
        DATA <- as.data.frame(Data[, colnames(Data) != group])
        colnames(DATA) <- colnames(Data)[colnames(Data) != group]
      }
    } else {
      GROUP <- group
      DATA <- as.data.frame(Data)
    }
    if (length(levels(as.factor(GROUP))) != 2) {
      stop("'group' must be a binary vector.", call. = FALSE)
    }
    if (is.matrix(DATA) | is.data.frame(DATA)) {
      if (!all(apply(DATA, 2, function(i) {
        length(levels(as.factor(i))) == 2
      }))) {
        stop("'Data' must be a data.frame or a matrix of binary vectors.", call. = FALSE)
      }
      if (dim(DATA)[1] != length(GROUP)) {
        stop("'Data' must have the same number of rows as is the length of vector 'group'.", call. = FALSE)
      }
    } else {
      if (is.vector(DATA)) {
        if (length(DATA) != length(GROUP)) {
          stop("'Data' must have the same number of rows as is the length of vector 'group'.", call. = FALSE)
        }
        DATA <- as.data.frame(DATA)
      } else {
        stop("'Data' must be a data.frame or a matrix of binary vectors.", call. = FALSE)
      }
    }
    group.names <- unique(GROUP)[!is.na(unique(GROUP))]
    if (group.names[1] == focal.name) {
      group.names <- rev(group.names)
    }
    GROUP <- as.numeric(as.factor(GROUP) == focal.name)

    if (length(match) == dim(DATA)[1]) {
      df <- data.frame(DATA, GROUP, match, check.names = FALSE)
    } else {
      df <- data.frame(DATA, GROUP, check.names = FALSE)
    }

    df <- df[complete.cases(df), ]

    if (dim(df)[1] == 0) {
      stop("It seems that your 'Data' does not include any observations that are complete.",
        call. = FALSE
      )
    }

    GROUP <- df[, "GROUP"]
    DATA <- as.data.frame(df[, !(colnames(df) %in% c("GROUP", "match"))])
    colnames(DATA) <- colnames(df)[!(colnames(df) %in% c("GROUP", "match"))]

    if (length(match) > 1) {
      match <- df[, "match"]
    }

    # number of items
    m <- dim(DATA)[2]
    # model
    if (length(model) == 1) {
      model <- rep(model, m)
    } else {
      if (length(model) != m) {
        stop("Invalid length of 'model'. Model needs to be specified for each item or
             by single string. ", call. = FALSE)
      }
    }
    if (!all(model %in% c(
      "Rasch", "1PL", "2PL", "3PLcg", "3PLdg", "3PLc", "3PL", "3PLd",
      "4PLcgdg", "4PLcgd", "4PLd", "4PLcdg", "4PLc", "4PL"
    ))) {
      stop("Invalid value for 'model'.", call. = FALSE)
    }

    # type of DIF to be tested
    if (length(type) == 1) {
      type <- rep(type, m)
    } else {
      if (length(type) != dim(DATA)[2]) {
        stop("Invalid length of 'type'. Type of DIF needs to be specified for each item or
             by a single string. ", call. = FALSE)
      }
    }
    tmptype <- type[!(type %in% c("udif", "nudif", "both", "all"))]
    if (length(tmptype) > 0) {
      tmptypes <- unique(unlist(sapply(
        1:length(tmptype),
        function(i) unique(unlist(strsplit(tmptype[i], split = "")))
      )))
      if (!all(tmptypes %in% letters[1:4])) {
        stop("Type of DIF to be tested not recognized. Only parameters 'a', 'b', 'c' or 'd' can be tested
             or 'type' must be one of predefined options: either 'udif', 'nudif', 'both', or 'all'.", call. = FALSE)
      }
    }

    # constraints
    if (!(is.null(constraints))) {
      if (length(constraints) == 1) {
        constraints <- rep(constraints, m)
      } else {
        if (length(constraints) != m) {
          stop("Invalid length of 'constraints'. Constraints need to be specified for each item or
               by single string. ", call. = FALSE)
        }
      }
      constraints <- lapply(1:m, function(i) unique(unlist(strsplit(constraints[i], split = ""))))
      if (!all(sapply(1:m, function(i) {
        all(constraints[[i]] %in% letters[1:4]) | all(is.na(constraints[[i]]))
      }))) {
        stop("Constraints can be only combinations of parameters 'a', 'b', 'c', and 'd'. ", call. = FALSE)
      }
      if (any(!(type %in% c("udif", "nudif", "both", "all")))) {
        types <- as.list(rep(NA, m))
        wht <- !(type %in% c("udif", "nudif", "both", "all"))
        types[wht] <- unlist(strsplit(type[wht], split = ""))
        if (any(sapply(1:m, function(i) (all(!is.na(constraints[[i]])) & (types[[i]] %in% constraints[[i]]))))) {
          stop("The difference in constrained parameters cannot be tested.", call. = FALSE)
        }
      } else {
        types <- NULL
      }
    } else {
      constraints <- as.list(rep(NA, m))
      types <- NULL
    }
    # anchors
    if (!is.null(anchor)) {
      if (is.numeric(anchor)) {
        ANCHOR <- anchor
      } else {
        ANCHOR <- NULL
        for (i in 1:length(anchor)) {
          ANCHOR[i] <- (1:m)[colnames(DATA) == anchor[i]]
        }
      }
    } else {
      ANCHOR <- 1:m
    }

    # parameterization
    parameterization <- rep("is", m)
    if (!is.null(start)) {
      if (length(start) != m) {
        stop("Invalid value for 'start'. Initial values must be a list with as many elements
             as a number of items in 'Data'.", call. = FALSE)
      }
      start_names <- unique(unlist(lapply(start, names)))
      if (!all(start_names %in% c(paste0("b", 0:3), letters[3:4], paste0(letters[3:4], "Dif"))) &
        !all(start_names %in% c(letters[1:4], paste0(letters[1:4], "Dif")))) {
        stop("Invalid names in 'start'. Each element of 'start' needs to be a numeric vector
             with names 'b0', 'b1', 'b2', 'b3', 'c', 'd', 'cDif', and 'dDif' or
             'a', 'b', 'c', 'd', 'aDif', 'bDif', 'cDif', and 'dDif'. ", call. = FALSE)
      } else {
        if (all(start_names %in% c(letters[1:4], paste0(letters[1:4], "Dif")))) {
          start_old <- start
          start <- lapply(start_old, function(x) {
            tmp <- unlist(c(
              b0 = -x["a"] * x["b"], b1 = x["a"],
              b2 = -x["a"] * x["bDif"] - x["aDif"] * x["b"] - x["aDif"] * x["bDif"],
              b3 = x["aDif"],
              cR = x["c"], dR = x["d"],
              cF = x["c"] + x["cDif"], dF = x["d"] + x["dDif"]
            ))
            names(tmp) <- c("b0", "b1", "b2", "b3", "cR", "dR", "cF", "dF")
            return(tmp)
          })
        }
      }
    }
    if (!purify | !(match[1] %in% c("zscore", "score")) | !is.null(anchor)) {
      PROV <- NLR(DATA, GROUP,
        model = model, constraints = constraints, type = type, method = method,
        match = match, anchor = ANCHOR, start = start, p.adjust.method = p.adjust.method,
        test = test, alpha = alpha, initboot = initboot, nrBo = nrBo, sandwich = sandwich
      )
      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      significant <- which(ADJ.PVAL < alpha)

      nlrPAR <- nlrSE <- lapply(
        1:length(PROV$par.m1),
        function(i) {
          if (i %in% PROV$conv.fail.which) {
            structure(rep(NA, length(PROV$par.m1[[i]])),
              names = names(PROV$par.m1[[i]])
            )
          } else {
            structure(rep(0, length(PROV$par.m1[[i]])),
              names = names(PROV$par.m1[[i]])
            )
          }
        }
      )
      for (i in 1:length(PROV$par.m1)) {
        if (!(i %in% PROV$conv.fail.which)) {
          nlrPAR[[i]][names(PROV$par.m0[[i]])] <- PROV$par.m0[[i]]
          nlrSE[[i]][names(PROV$se.m0[[i]])] <- PROV$se.m0[[i]]
        }
      }

      if (length(significant) > 0) {
        DIFitems <- significant
        for (idif in 1:length(DIFitems)) {
          nlrPAR[[DIFitems[idif]]] <- PROV$par.m1[[DIFitems[idif]]]
          nlrSE[[DIFitems[idif]]] <- PROV$se.m1[[DIFitems[idif]]]
        }
      } else {
        DIFitems <- "No DIF item detected"
      }

      if (any(!(type %in% c("udif", "nudif", "both", "all")))) {
        wht <- (!(type %in% c("udif", "nudif", "both", "all")))
        type[wht] <- "other"
      }
      RES <- list(
        Sval = STATS,
        nlrPAR = nlrPAR, nlrSE = nlrSE,
        parM0 = PROV$par.m0, seM0 = PROV$se.m0, covM0 = PROV$cov.m0, llM0 = PROV$ll.m0,
        parM1 = PROV$par.m1, seM1 = PROV$se.m1, covM1 = PROV$cov.m1, llM1 = PROV$ll.m1,
        DIFitems = DIFitems,
        model = model, constraints = constraints, type = type, types = types,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval,
        df = PROV$df, test = test,
        purification = purify,
        method = method,
        conv.fail = PROV$cf, conv.fail.which = PROV$cf.which,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, match = match
      )
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- NLR(DATA, GROUP,
        model = model, constraints = constraints, type = type, method = method,
        match = match, start = start, p.adjust.method = p.adjust.method, test = test,
        alpha = alpha, initboot = initboot, nrBo = nrBo, sandwich = sandwich
      )
      stats1 <- prov1$Sval
      pval1 <- prov1$pval
      significant1 <- which(pval1 < alpha)

      if (length(significant1) == 0) {
        PROV <- prov1
        STATS <- stats1
        DIFitems <- "No DIF item detected"
        nlrPAR <- nlrSE <- structure(data.frame(matrix(0, ncol = dim(PROV$par.m1)[2], nrow = dim(PROV$par.m1)[1])),
          .Names = colnames(PROV$par.m1)
        )
        nlrPAR[, colnames(PROV$par.m0)] <- PROV$par.m0
        nlrSE[, colnames(PROV$par.m0)] <- PROV$se.m0
        noLoop <- TRUE
      } else {
        dif <- significant1
        difPur <- rep(0, length(stats1))
        difPur[dif] <- 1
        repeat {
          if (nrPur >= nrIter) {
            break
          } else {
            nrPur <- nrPur + 1
            nodif <- NULL
            if (is.null(dif)) {
              nodif <- 1:m
            } else {
              for (i in 1:m) {
                if (sum(i == dif) == 0) {
                  nodif <- c(nodif, i)
                }
              }
            }
            prov2 <- NLR(DATA, GROUP,
              model = model, constraints = constraints, type = type, method = method,
              match = match, anchor = nodif, start = start, p.adjust.method = p.adjust.method,
              test = test, alpha = alpha, initboot = initboot, nrBo = nrBo, sandwich = sandwich
            )
            stats2 <- prov2$Sval
            pval2 <- prov2$pval
            significant2 <- which(pval2 < alpha)
            if (length(significant2) == 0) {
              dif2 <- NULL
            } else {
              dif2 <- significant2
            }
            difPur <- rbind(difPur, rep(0, m))
            difPur[nrPur + 1, dif2] <- 1
            if (length(dif) != length(dif2)) {
              dif <- dif2
            } else {
              dif <- sort(dif)
              dif2 <- sort(dif2)
              if (sum(dif == dif2) == length(dif)) {
                noLoop <- TRUE
                break
              } else {
                dif <- dif2
              }
            }
          }
        }
        PROV <- prov2
        STATS <- stats2
        significant1 <- which(PROV$adjusted.pval < alpha)

        nlrPAR <- nlrSE <- lapply(
          1:length(PROV$par.m1),
          function(i) {
            if (i %in% PROV$conv.fail.which) {
              structure(rep(NA, length(PROV$par.m1[[i]])),
                names = names(PROV$par.m1[[i]])
              )
            } else {
              structure(rep(0, length(PROV$par.m1[[i]])),
                names = names(PROV$par.m1[[i]])
              )
            }
          }
        )
        for (i in 1:length(PROV$par.m1)) {
          if (!(i %in% PROV$conv.fail.which)) {
            nlrPAR[[i]][names(PROV$par.m0[[i]])] <- PROV$par.m0[[i]]
            nlrSE[[i]][names(PROV$se.m0[[i]])] <- PROV$se.m0[[i]]
          }
        }

        if (length(significant1) > 0) {
          DIFitems <- significant1
          for (idif in 1:length(DIFitems)) {
            nlrPAR[[DIFitems[idif]]] <- PROV$par.m1[[DIFitems[idif]]]
            nlrSE[[DIFitems[idif]]] <- PROV$se.m1[[DIFitems[idif]]]
          }
        } else {
          DIFitems <- "No DIF item detected"
        }
      }
      if (!is.null(difPur)) {
        rownames(difPur) <- paste0("Step", 0:(dim(difPur)[1] - 1))
        colnames(difPur) <- colnames(DATA)
      }

      if (any(!(type %in% c("udif", "nudif", "both", "all")))) {
        wht <- (!(type %in% c("udif", "nudif", "both", "all")))
        type[wht] <- "other"
      }
      RES <- list(
        Sval = STATS,
        nlrPAR = nlrPAR, nlrSE = nlrSE,
        parM0 = PROV$par.m0, seM0 = PROV$se.m0, covM0 = PROV$cov.m0, llM0 = PROV$ll.m0,
        parM1 = PROV$par.m1, seM1 = PROV$se.m1, covM1 = PROV$cov.m1, llM1 = PROV$ll.m1,
        DIFitems = DIFitems,
        model = model, constraints = constraints, type = type, types = types,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval,
        df = PROV$df, test = test,
        purification = purify, nrPur = nrPur, difPur = difPur, conv.puri = noLoop,
        method = method,
        conv.fail = PROV$cf, conv.fail.which = PROV$cf.which,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, match = match
      )
    }
    if (purify) {
      if (!noLoop) {
        warning(
          paste0(
            "Item purification process not converged after ",
            nrPur, ifelse(nrPur <= 1, " iteration.", " iterations."), "\n",
            "Results are based on the last iteration of the item purification.\n"
          ),
          call. = FALSE
        )
      }
    }
    class(RES) <- "difNLR"
    return(RES)
  }

  resToReturn <- internalNLR()
  return(resToReturn)
}

#' @export
print.difNLR <- function(x, ...) {
  cat(paste0(
    "Detection of ",
    ifelse(length(unique(x$type)) == 1,
      switch(unique(x$type),
        both = "both types of ",
        udif = "uniform ",
        nudif = "non-uniform ",
        all = "all types of ",
        other = ""
      ),
      ""
    ),
    "differential item functioning\n",
    "using the generalized logistic regression model"
  ))
  cat(
    paste0(
      "\n\nGeneralized logistic regression ",
      switch(x$test,
        "F" = "F-test",
        "LR" = "likelihood ratio chi-square",
        "W" = "Wald test"
      ),
      " statistics",
      ifelse(length(unique(x$model)) == 1,
        paste0(
          "\nbased on ",
          switch(unique(x$model),
            "Rasch" = "Rasch model",
            "1PL" = "1PL model",
            "2PL" = "2PL model",
            "3PL" = "3PL model",
            "3PLcg" = "3PL model with fixed guessing for groups",
            "3PLdg" = "3PL model with fixed inattention parameter for groups",
            "3PLc" = "3PL model",
            "3PLd" = "3PL model with inattention parameter",
            "4PLcgdg" = "4PL model with fixed guessing and inattention parameter for groups",
            "4PLcgd" = "4PL model with fixed guessing for groups",
            "4PLd" = "4PL model with fixed guessing for groups",
            "4PLcdg" = "4PL model with fixed inattention parameter for groups",
            "4PLc" = "4PL model with fixed inattention parameter for groups",
            "4PL" = "4PL model"
          )
        ), ""
      )
    ),
    ifelse((!all(is.na(x$constraints)) & length(unique(x$constraints)) == 1),
      paste(
        " with constraints on",
        ifelse(length(unique(unlist(x$constraints))) == 1, "parameter", "parameters"),
        paste(unique(unlist(x$constraints)), collapse = ", "), "\n"
      ),
      "\n"
    )
  )
  cat(paste(
    "\nParameters were estimated using",
    switch(x$method,
      "nls" = "non-linear least squares\n",
      "mle" = "maximum likelihood method \nwith the L-BFGS-B algorithm\n",
      "em" = "maximum likelihood method \nwith the EM algorithm\n",
      "plf" = "maximum likelihood method \nwith the PLF algorithm\n",
      "irls" = "maximum likelihood method \nwith the iteratively reweighted least squares algorithm\n"
    )
  ))
  cat(paste0(
    "\nItem purification was",
    ifelse(x$purification, " ", " not "), "applied",
    ifelse(x$purification, paste0(
      " with ", x$nrPur,
      ifelse(x$nrPur <= 1, " iteration.", " iterations.")
    ), ""), "\n"
  ))
  if (x$p.adjust.method == "none") {
    cat("No p-value adjustment for multiple comparisons\n\n")
  } else {
    cat(paste(
      "Multiple comparisons made with",
      switch(x$p.adjust.method,
        holm = "Holm",
        hochberg = "Hochberg",
        hommel = "Hommel",
        bonferroni = "Bonferroni",
        BH = "Benjamini-Hochberg",
        BY = "Benjamini-Yekutieli",
        fdr = "FDR"
      ), "adjustment of p-values\n\n"
    ))
  }
  sign <- ifelse(is.na(x$adj.pval), " ",
    symnum(x$adj.pval,
      c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", "")
    )
  )
  if (x$p.adjust.method == "none") {
    tab <- format(round(cbind(x$Sval, x$pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 3)
    colnames(tab) <- switch(x$test,
      "F" = c("F-value", "P-value", ""),
      "LR" = c("Chisq-value", "P-value", ""),
      "W" = c("Chisq-value", "P-value", "")
    )
  } else {
    tab <- format(round(cbind(x$Sval, x$pval, x$adj.pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 4)
    colnames(tab) <- switch(x$test,
      "F" = c("F-value", "P-value", "Adj. P-value", ""),
      "LR" = c("Chisq-value", "P-value", "Adj. P-value", ""),
      "W" = c("Chisq-value", "P-value", "Adj. P-value", "")
    )
  }

  rownames(tab) <- colnames(x$Data)

  print(tab, quote = FALSE, digits = 4, zero.print = F)
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  if (x$test == "F") {
    if (dim(unique(x$df))[1] == 1) {
      critical <- unique(qf(1 - x$alpha, x$df[, 1], x$df[, 2]))
    } else {
      critical <- NULL
    }
  } else {
    if (length(unique(x$df)) == 1) {
      critical <- unique(qchisq(1 - x$alpha, x$df))
    } else {
      critical <- NULL
    }
  }
  if (!is.null(critical)) {
    cat(paste0("\nDetection thresholds: ", round(critical, 4), " (significance level: ", x$alpha, ")"))
  }

  if (is.character(x$DIFitems)) {
    cat("\nNone of items is detected as DIF \n")
  } else {
    cat("\n\nItems detected as DIF items:")
    cat("\n", paste0(colnames(x$Data)[x$DIFitems], "\n"))
  }
}

#' ICC and test statistics plots for an object of the \code{"difNLR"} class.
#'
#' @description
#' A plotting method for an object of the \code{"difNLR"} class using the
#' \pkg{ggplot2} package.
#'
#' Two types of plots are available. The first one is obtained by setting
#' \code{plot.type = "cc"} (default). The characteristic curves for items specified
#' in the \code{item} argument are plotted. Plotted curves represent the best
#' fitted model.
#'
#' The second plot is obtained by setting \code{plot.type = "stat"}. The test
#' statistics (either LR-test, F-test, or Wald test; depending on argument
#' \code{test}) are displayed on the Y axis, for each converged item. The detection
#' threshold is displayed by a horizontal line and items detected as DIF are
#' printed with the red color. Only parameters \code{size} and \code{title} are
#' used.
#'
#' @param x an object of the \code{"difNLR"} class.
#' @param plot.type character: a type of a plot to be plotted (either \code{"cc"}
#'   for characteristic curves (default), or \code{"stat"} for test statistics).
#' @param item numeric or character: either character \code{"all"} to apply for all
#'   converged items (default), or a vector of item names (column names of the
#'   \code{Data}), or item identifiers (integers specifying the column number).
#' @param draw.empirical logical: should empirical probabilities be plotted as
#'   points? (the default value is \code{TRUE}).
#' @param draw.CI logical: should confidence intervals for predicted values be
#'   plotted? (the default value is \code{FALSE}).
#' @param group.names character: names of the reference and focal groups.
#' @param ... other generic parameters for the \code{plot()} method.
#'
#' @return
#' For an option \code{plot.type = "stat"}, returns object of the \code{"ggplot"}
#' class. In the case of \code{plot.type = "cc"}, returns a list of objects of the
#' \code{"ggplot"} class.
#'
#' Outputs can be edited and modified as a standard \code{"ggplot"} object
#' including colours, titles, shapes, or linetypes.
#'
#' Note that the option \code{draw.CI = TRUE} returns confidence intervals for
#' predicted values as calculated by the \code{\link[difNLR]{predict.difNLR}}.
#' Confidence intervals may overlap even in the case that item functions
#' differently.
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
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using the generalized logistic regression model. \cr
#' \code{\link[difNLR]{predict.difNLR}} for prediction.
#' \code{\link[ggplot2]{ggplot}} for a general function to plot with the \pkg{"ggplot2"} package.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # item characteristic curves
#' plot(x)
#' plot(x, item = x$DIFitems)
#' plot(x, item = 1)
#' plot(x, item = "Item2", group.names = c("Group 1", "Group 2"))
#'
#' # item characteristic curves without empirical probabilities
#' plot(x, item = 1, draw.empirical = FALSE)
#'
#' # item characteristic curves without empirical probabilities but with CI
#' plot(x, item = 1, draw.empirical = FALSE, draw.CI = TRUE)
#'
#' # graphical devices - test statistics
#' plot(x, plot.type = "stat")
#' }
#' @export
plot.difNLR <- function(x, plot.type = "cc", item = "all",
                        group.names, draw.empirical = TRUE, draw.CI = FALSE, ...) {
  plotstat <- function(x) {
    if (x$conv.fail != 0) {
      if (length(x$conv.fail) == sum(!is.na(x$Sval))) {
        switch(x$test,
          "F" = stop(
            "None of items does converge. F-statistic values not plotted",
            call. = FALSE
          ),
          "LR" = stop(
            "None of items does converge. LR-statistic values not plotted",
            call. = FALSE
          )
        )
      } else {
        switch(x$test,
          "F" = warning(paste0("Item ", x$conv.fail.which,
            " does not converge. F-statistic value not plotted",
            collapse = "\n"
          ), call. = FALSE),
          "LR" = warning(paste0("Item ", x$conv.fail.which,
            " does not converge. LR-statistic value not plotted",
            collapse = "\n"
          ), call. = FALSE)
        )
      }
    }

    title <- "Non-linear regression DIF detection with none multiple comparison correction"

    n <- dim(x$Data)[1]
    if (x$test == "F") {
      if (dim(unique(x$df))[1] == 1) {
        Sval_critical <- unique(qf(1 - x$alpha, x$df[, 1], x$df[, 2]))
      } else {
        Sval_critical <- NULL
      }
    } else {
      if (length(unique(x$df)) == 1) {
        Sval_critical <- unique(qchisq(1 - x$alpha, x$df))
      } else {
        Sval_critical <- NULL
      }
    }
    if (is.null(Sval_critical)) {
      stop("Critical values are different for different items. Plot cannot be rendered.")
    }

    items <- setdiff(1:length(x$Sval), x$conv.fail.which)
    g <- as.factor(ifelse(x$Sval > Sval_critical, 1, 0))
    hv <- na.omit(as.data.frame(cbind(1:length(x$Sval), x$Sval, g)))
    hv$g <- as.factor(hv$g)
    hv$V1 <- as.factor(hv$V1)
    plot_stat <- ggplot2::ggplot(data = hv, ggplot2::aes(
      x = .data$V1, y = .data$V2,
      label = .data$V1,
      col = .data$g
    )) +
      # points
      ggplot2::geom_text() +
      ggplot2::scale_color_manual(values = c("black", "red")) +
      # critical value
      ggplot2::geom_hline(yintercept = Sval_critical) +
      # theme
      ggplot2::ggtitle(title) +
      ggplot2::labs(x = "Item", y = switch(x$test,
        "F" = "F-statistic",
        "LR" = "Chisq-statistic"
      )) +
      .plot.theme() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", vjust = 1.5),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "none"
      )

    return(plot_stat)
  }

  if (missing(group.names)) {
    group.names <- x$group.names
    if (all(group.names %in% c(0, 1))) group.names <- c("Reference", "Focal")
  } else {
    if (length(group.names) > 2) {
      group.names <- group.names[1:2]
      warning("Only first two values for 'group.names' argument are used.", call. = FALSE)
    } else {
      if (length(group.names) < 2) {
        group.names <- c("Reference", "Focal")
        warning("Argument 'group.names' need to have the length of two. Default value is used.", call. = FALSE)
      }
    }
  }

  plotCC <- function(x, item = item) {
    m <- length(x$nlrPAR)
    nams <- colnames(x$Data)

    if (inherits(item, "character")) {
      if (any(item != "all") & !all(item %in% nams)) {
        stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
          call. = FALSE
        )
      }
      if (any(item == "all")) {
        items <- 1:m
      } else {
        items <- which(nams %in% item)
      }
    } else {
      if (!inherits(item, "integer") & !inherits(item, "numeric")) {
        stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
          call. = FALSE
        )
      } else {
        if (!all(item %in% 1:m)) {
          stop("Invalid number for 'item'.",
            call. = FALSE
          )
        } else {
          items <- item
        }
      }
    }

    if (any(x$conv.fail.which %in% items)) {
      if (length(setdiff(items, x$conv.fail.which)) == 0) {
        stop(
          paste0("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted.",
            collapse = "\n"
          ),
          call. = FALSE
        )
      } else {
        warning(
          paste0("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted.",
            collapse = "\n"
          ),
          call. = FALSE
        )
        items <- setdiff(items, x$conv.fail.which)
      }
    }

    if (x$purification) {
      anchor <- c(1:m)[!c(1:m) %in% x$DIFitems]
    } else {
      anchor <- 1:m
    }

    if (length(x$match) > 1) {
      xlab <- "Matching criterion"
      match <- x$match
    } else {
      if (x$match == "score") {
        xlab <- "Total score"
        match <- rowSums(as.data.frame(x$Data[, anchor]))
      } else {
        xlab <- "Standardized total score"
        match <- scale(rowSums(as.data.frame(x$Data[, anchor])))
      }
    }
    xR <- match[x$group == 0]
    xF <- match[x$group == 1]

    max_match <- max(c(xR, xF), na.rm = TRUE)
    min_match <- min(c(xR, xF), na.rm = TRUE)

    plot_CC <- list()

    for (i in items) {
      TITLE <- colnames(x$Data)[i]

      df_line <- predict(x,
        item = i,
        match = rep(
          seq(floor(min_match),
            ceiling(max_match),
            length.out = 100
          ),
          2
        ),
        group = rep(c(0, 1), each = 100),
        interval = ifelse(draw.CI, "confidence", "none")
      )

      if (draw.empirical) {
        df_point_0 <- data.frame(cbind(
          as.numeric(levels(as.factor(xR))),
          tapply(
            x$Data[x$group == 0, i],
            as.factor(xR), mean
          )
        ))
        df_point_1 <- data.frame(cbind(
          as.numeric(levels(as.factor(xF))),
          tapply(
            x$Data[x$group == 1, i],
            as.factor(xF), mean
          )
        ))
        df_point <- data.frame(rbind(
          cbind(df_point_0, group = "0"),
          cbind(df_point_1, group = "1")
        ))
        colnames(df_point) <- c("match", "prob", "group")
        rownames(df_point) <- NULL
        df_point$size <- c(table(xR), table(xF))
      }

      g <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = df_line, ggplot2::aes(
            x = .data$match, y = .data$prob,
            col = .data$group, linetype = .data$group
          ),
          linewidth = 0.8
        )

      # empirical probabilities
      if (draw.empirical) {
        g <- g +
          ggplot2::geom_point(
            data = df_point, ggplot2::aes(
              x = .data$match, y = .data$prob,
              col = .data$group, fill = .data$group,
              size = .data$size
            ),
            shape = 21, alpha = 0.5
          )
      }

      # confidence interval
      if (draw.CI) {
        g <- g +
          ggplot2::geom_ribbon(
            data = df_line, ggplot2::aes(
              x = .data$match, y = .data$prob,
              ymin = .data$lwr.conf, ymax = .data$upr.conf,
              col = .data$group, fill = .data$group
            ),
            alpha = 0.25
          )
      }

      g <- g +
        # adjusting colours
        ggplot2::scale_fill_manual(values = c("dodgerblue2", "goldenrod2"), labels = group.names) +
        ggplot2::scale_colour_manual(values = c("dodgerblue2", "goldenrod2"), labels = group.names) +
        ggplot2::scale_linetype_manual(values = c("solid", "dashed"), labels = group.names) +
        ggplot2::ggtitle(TITLE) +
        ggplot2::labs(x = xlab, y = "Probability of correct answer") +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        # theme
        .plot.theme() +
        # legend
        .plot.theme.legend() +
        ggplot2::guides(
          size = ggplot2::guide_legend(title = "Count", order = 1),
          colour = ggplot2::guide_legend(title = "Group", order = 2),
          fill = ggplot2::guide_legend(title = "Group", order = 2),
          linetype = ggplot2::guide_legend(title = "Group", order = 2)
        )
      plot_CC[[i]] <- g
    }
    plot_CC <- plot_CC[items]
    names(plot_CC) <- nams[items]
    return(plot_CC)
  }
  # checking input
  if (!(plot.type %in% c("cc", "stat"))) {
    stop("Invalid value for 'plot.type'. Possible values of 'plot.type' is 'cc' or 'stat'.",
      call. = FALSE
    )
  } else {
    if (plot.type == "cc") {
      plotCC(x, item = item)
    } else {
      plotstat(x)
    }
  }
}

#' @aliases residuals.difNLR
#' @rdname residuals.difNLR
#' @export
fitted.difNLR <- function(object, item = "all", ...) {
  # checking input
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (inherits(item, "character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item, "integer") & !inherits(item, "numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
          call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  if (any(object$conv.fail.which %in% items)) {
    if (length(setdiff(items, object$conv.fail.which)) == 0) {
      stop(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
          collapse = "\n"
        ),
        call. = FALSE
      )
    } else {
      warning(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
          collapse = "\n"
        ),
        call. = FALSE
      )
      ITEMS <- setdiff(items, object$conv.fail.which)
    }
  } else {
    ITEMS <- items
  }

  PAR <- data.frame(
    a = rep(1, m), b = 0, c = 0, d = 1,
    aDif = 0, bDif = 0, cDif = 0, dDif = 0
  )
  for (i in ITEMS) {
    PAR[i, names(object$nlrPAR[[i]])] <- object$nlrPAR[[i]]
  }

  # data
  if (length(object$match) > 1) {
    match <- object$match
  } else {
    if (object$match == "score") {
      match <- c(apply(object$Data, 1, sum))
    } else {
      match <- c(scale(apply(object$Data, 1, sum)))
    }
  }

  # fitted values
  FV <- as.list(rep(NA, m))
  FV[ITEMS] <- lapply(ITEMS, function(i) {
    .gNLR(
      match, object$group,
      PAR[i, "a"], PAR[i, "b"],
      PAR[i, "c"], PAR[i, "d"],
      PAR[i, "aDif"], PAR[i, "bDif"],
      PAR[i, "cDif"], PAR[i, "dDif"]
    )
  })
  FV <- do.call(cbind, FV)
  colnames(FV) <- colnames(object$Data)
  rownames(FV) <- rownames(object$Data)
  FV <- FV[, items]

  return(FV)
}

#' Predicted values for an object of the \code{"difNLR"} class.
#'
#' @description
#' S3 method for predictions from the fitted model used in the object of the
#' \code{"difNLR"} class.
#'
#' @param object an object of the \code{"difNLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all
#'   converged items (default), or a vector of item names (column names of the
#'   \code{Data}), or item identifiers (integers specifying the column number).
#' @param match numeric: a matching criterion for new observations.
#' @param group numeric: a group membership variable for new observations.
#' @param interval character: a type of interval calculation, either
#'   \code{"none"} (default) or \code{"confidence"} for confidence
#'   interval.
#' @param CI numeric: a significance level for confidence interval (the default is
#'   \code{0.95} for 95\% confidence interval).
#' @param ... other generic parameters for the \code{predict()} method.
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
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using the generalized logistic regression model. \cr
#' \code{\link[stats]{predict}} for a generic function for prediction.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # predicted values
#' summary(predict(x))
#' predict(x, item = 1)
#' predict(x, item = "Item1")
#'
#' # predicted values for new observations - average score
#' predict(x, item = 1, match = 0, group = 0) # reference group
#' predict(x, item = 1, match = 0, group = 1) # focal group
#' predict(x, item = 1, match = 0, group = c(0, 1)) # both groups
#'
#' # predicted values for new observations - various Z-scores and groups
#' new.match <- rep(c(-1, 0, 1), each = 2)
#' new.group <- rep(c(0, 1), 3)
#' predict(x, item = 1, match = new.match, group = new.group)
#'
#' # predicted values for new observations with confidence intervals
#' predict(x, item = 1, match = new.match, group = new.group, interval = "confidence")
#' predict(x, item = c(2, 4), match = new.match, group = new.group, interval = "confidence")
#' }
#'
#' @export
predict.difNLR <- function(object, item = "all", match, group, interval = "none", CI = 0.95, ...) {
  # checking input
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (inherits(item, "character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item, "integer") & !inherits(item, "numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
          call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  if (missing(match)) {
    if (length(object$match) > 1) {
      match <- object$match
    } else {
      if (object$match == "score") {
        match <- c(apply(object$Data, 1, sum))
      } else {
        match <- c(scale(apply(object$Data, 1, sum)))
      }
    }
  }

  if (missing(group)) {
    group <- object$group
  }
  if (length(match) != length(group)) {
    if (length(match) == 1) {
      match <- rep(match, length(group))
    } else if (length(group) == 1) {
      group <- rep(group, length(match))
    } else {
      stop("Arguments 'match' and 'group' must be of the same length.",
        call. = FALSE
      )
    }
  }

  if (any(object$conv.fail.which %in% items)) {
    if (length(setdiff(items, object$conv.fail.which)) == 0) {
      stop(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
          collapse = "\n"
        ),
        call. = FALSE
      )
    } else {
      warning(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
          collapse = "\n"
        ),
        call. = FALSE
      )
      ITEMS <- setdiff(items, object$conv.fail.which)
    }
  } else {
    ITEMS <- items
  }
  m <- length(ITEMS)
  nams <- nams[ITEMS]

  if (!interval %in% c("none", "confidence")) {
    warning(
      "Only confidence interval is supported. ",
      call. = FALSE
    )
    interval <- "none"
  }

  if (CI > 1 | CI < 0 | !is.numeric(CI)) {
    stop("Invalid value for 'CI'. 'CI' must be numeric value between 0 and 1. ",
      call. = FALSE
    )
  }

  # estimated parameters
  PAR <- coef(object, item = ITEMS, SE = FALSE, simplify = FALSE, IRTpars = FALSE, CI = 0)
  # estimated parameters, adding missing parameters
  PAR_NEW <- lapply(PAR, function(x) {
    par_names <- names(x)
    par_tmp <- data.frame(
      b0 = 0, b1 = 0, b2 = 0, b3 = 0,
      c = 0, d = 1, cDif = 0, dDif = 0
    )
    par_tmp[par_names] <- x[par_names]
    return(par_tmp)
  })

  # predicted values
  PV <- lapply(1:m, function(i) {
    pars <- as.vector(PAR_NEW[[i]])
    .gNLR.is(
      x = as.vector(match), g = group,
      b0 = pars[["b0"]], b1 = pars[["b1"]], b2 = pars[["b2"]], b3 = pars[["b3"]],
      c = pars[["c"]], d = pars[["d"]], cDif = pars[["cDif"]], dDif = pars[["dDif"]]
    )
  })

  # confidence interval
  if (interval == "confidence") {
    DELTA_new <- as.list(rep(NA, m))
    DELTA_new <- lapply(1:m, function(i) {
      attr(.delta.gNLR.is(
        match, group,
        PAR_NEW[[i]][, "b0"], PAR_NEW[[i]][, "b1"], PAR_NEW[[i]][, "b2"], PAR_NEW[[i]][, "b3"],
        PAR_NEW[[i]][, "c"], PAR_NEW[[i]][, "d"], PAR_NEW[[i]][, "cDif"], PAR_NEW[[i]][, "dDif"]
      ), "gradient")
    })

    VCOV_par <- lapply(1:m, function(i) {
      matrix(
        0,
        ncol = 8, nrow = 8,
        dimnames = list(
          c("b0", "b1", "b2", "b3", "c", "d", "cDif", "dDif"),
          c("b0", "b1", "b2", "b3", "c", "d", "cDif", "dDif")
        )
      )
    })
    VCOV_par <- lapply(1:m, function(i) {
      if (ITEMS[i] %in% object$DIFitems) {
        tmp <- colnames(object$covM1[[ITEMS[i]]])
        VCOV_par[[i]][tmp, tmp] <- object$covM1[[ITEMS[i]]]
      } else {
        tmp <- colnames(object$covM0[[ITEMS[i]]])
        VCOV_par[[i]][tmp, tmp] <- object$covM0[[ITEMS[i]]]
      }
      return(VCOV_par[[i]])
    })

    SE_new <- const <- lwr <- upp <- lapply(1:m, function(i) c())
    SE_new <- lapply(1:m, function(i) {
      sqrt(diag(DELTA_new[[i]] %*% VCOV_par[[i]] %*% t(DELTA_new[[i]])))
    })

    alpha <- 1 - CI
    n <- dim(object$Data)[[1]]
    d <- sapply(ITEMS, function(i) {
      ifelse(i %in% object$DIFitems, length(object$parM1[[i]]), length(object$parM0[[i]]))
    })
    df <- n - d

    const <- lapply(1:m, function(i) SE_new[[i]] * qt(1 - alpha / 2, df = df)[i])

    lwr <- lapply(1:m, function(i) PV[[i]] - const[[i]])
    upp <- lapply(1:m, function(i) PV[[i]] + const[[i]])

    res <- lapply(1:m, function(i) {
      data.frame(
        item = nams[i],
        match = match,
        group = as.factor(group),
        prob = PV[[i]],
        lwr.conf = lwr[[i]],
        upr.conf = upp[[i]]
      )
    })
    res <- as.data.frame(do.call(rbind, res))
  } else {
    res <- lapply(1:m, function(i) {
      data.frame(
        item = nams[i],
        match = match,
        group = as.factor(group),
        prob = PV[[i]]
      )
    })
    res <- as.data.frame(do.call(rbind, res))
    res$item <- factor(res$item, levels = unique(res$item))
  }

  return(res)
}

#' Extract item parameter estimates from an object of the \code{"difNLR"} class.
#'
#' @aliases coefficients.difNLR
#'
#' @description
#' S3 method for extracting the item parameter estimates from an object of the \code{"difNLR"} class.
#'
#' @param object an object of the \code{"difNLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all
#'   converged items (default), or a vector of item names (column names of the
#'   \code{Data}), or item identifiers (integers specifying the column number).
#' @param SE logical: should the standard errors of the estimated item parameters
#'   be also returned? (the default is \code{FALSE}).
#' @param simplify logical: should the estimated item parameters be simplified to a
#'   matrix? (the default is \code{FALSE}).
#' @param IRTpars logical: should the estimated item parameters be returned in he
#'   IRT parameterization? (the default is \code{TRUE}).
#' @param CI numeric: a significance level for confidence intervals (CIs) of item
#'   parameter estimates (the default is \code{0.95} for 95\% CI).
#'   With 0 value, no CIs are displayed.
#' @param ... other generic parameters for the \code{coef()} method.
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
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using the generalized logistic regression model. \cr
#' \code{\link[stats]{coef}} for a generic function for extracting parameter estimates.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # estimated parameters
#' coef(x)
#' # includes standard errors
#' coef(x, SE = TRUE)
#' # includes standard errors and simplifies to matrix
#' coef(x, SE = TRUE, simplify = TRUE)
#' # intercept-slope parameterization
#' coef(x, IRTpars = FALSE)
#' # intercept-slope parameterization, simplifies to matrix, turn off confidence intervals
#' coef(x, IRTpars = FALSE, simplify = TRUE, CI = 0)
#' # for DIF items only
#' coef(x, item = x$DIFitems, IRTpars = FALSE, simplify = TRUE, CI = 0)
#' }
#' @export
coef.difNLR <- function(object, item = "all", SE = FALSE, simplify = FALSE, IRTpars = TRUE, CI = 0.95, ...) {
  # checking input
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (inherits(item, "character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item, "integer") & !inherits(item, "numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
          call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }
  nams <- nams[items]
  m <- length(items)

  if (!inherits(SE, "logical")) {
    stop("Invalid value for 'SE'. 'SE' needs to be logical. ",
      call. = FALSE
    )
  }
  if (!inherits(simplify, "logical")) {
    stop("Invalid value for 'simplify'. 'simplify' needs to be logical. ",
      call. = FALSE
    )
  }
  if (!inherits(IRTpars, "logical")) {
    stop("Invalid value for 'IRTpars'. 'IRTpars' needs to be logical. ",
      call. = FALSE
    )
  }
  if (CI > 1 | CI < 0 | !is.numeric(CI)) {
    stop("Invalid value for 'CI'. 'CI' must be numeric value between 0 and 1. ",
      call. = FALSE
    )
  }

  nlrPAR <- lapply(items, function(i) {
    if (i %in% object$DIFitems) {
      object$parM1[[i]]
    } else {
      object$parM0[[i]]
    }
  })
  nlrCOV <- lapply(items, function(i) {
    if (i %in% object$DIFitems) {
      object$covM1[[i]]
    } else {
      object$covM0[[i]]
    }
  })

  if (!IRTpars) {
    pars <- nlrPAR
    se <- lapply(1:m, function(i) {
      if (items[i] %in% object$conv.fail.which) {
        setNames(rep(NA, length(pars[[i]])), names(pars[[i]]))
      } else {
        sqrt(diag(nlrCOV[[i]]))
      }

    })
  } else {
    nlrDM <- lapply(1:m, function(i) {
      .deltamethod.NLR.is2irt(
        par = nlrPAR[[i]], cov = nlrCOV[[i]],
        conv = !(items[i] %in% object$conv.fail.which),
        cov_fail = is.null(nlrCOV[[i]])
      )
    })
    pars <- lapply(nlrDM, function(x) x$par)
    se <- lapply(nlrDM, function(x) x$se)
  }
  names(pars) <- nams
  names(se) <- nams

  if (SE) {
    coefs <- lapply(nams, function(i) rbind(pars[[i]], se[[i]]))
    coefs <- lapply(coefs, "rownames<-", c("estimate", "SE"))
    names(coefs) <- nams
  } else {
    coefs <- pars
  }

  if (CI > 0) {
    alpha <- (1 - CI) / 2
    CIlow <- lapply(nams, function(i) pars[[i]] - qnorm(1 - alpha) * se[[i]])
    CIupp <- lapply(nams, function(i) pars[[i]] + qnorm(1 - alpha) * se[[i]])
    names(CIlow) <- names(CIupp) <- nams
    coefs <- lapply(nams, function(i) rbind(coefs[[i]], CIlow[[i]], CIupp[[i]]))
    if (SE) {
      coefs <- lapply(coefs, "rownames<-", c("estimate", "SE", paste0("CI", 100 * alpha), paste0("CI", 100 * (1 - alpha))))
    } else {
      coefs <- lapply(coefs, "rownames<-", c("estimate", paste0("CI", 100 * alpha), paste0("CI", 100 * (1 - alpha))))
    }
    names(coefs) <- nams
  }

  if (simplify) {
    res <- as.data.frame(plyr::ldply(coefs, rbind))
    if (SE) {
      if (CI > 0) {
        resnams <- paste(res[, 1], c("estimate", "SE", paste0("CI", 100 * alpha), paste0("CI", 100 * (1 - alpha))))
      } else {
        resnams <- paste(res[, 1], c("estimate", "SE"))
      }
    } else {
      if (CI > 0) {
        resnams <- paste(res[, 1], c("estimate", paste0("CI", 100 * alpha), paste0("CI", 100 * (1 - alpha))))
      } else {
        resnams <- res[, 1]
      }
    }
    res <- res[, -1]
    rownames(res) <- resnams
    res[is.na(res)] <- 0
    if (object$conv.fail > 0) {
      res[grepl(nams[object$conv.fail.which], rownames(res)), ] <- NA
    }
  } else {
    res <- coefs
  }

  return(res)
}

#' Log-likelihood and information criteria for an object of the \code{"difNLR"} class.
#'
#' @aliases AIC.difNLR BIC.difNLR
#'
#' @rdname logLik.difNLR
#'
#' @description
#' S3 methods for extracting log-likelihood, Akaike's information criterion (AIC)
#' and Schwarz's Bayesian criterion (BIC) for an object of the \code{"difNLR"}
#' class.
#'
#' @param object an object of the \code{"difNLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all
#'   converged items (default), or a vector of item names (column names of the
#'   \code{Data}), or item identifiers (integers specifying the column number).
#' @param ... other generic parameters for S3 methods.
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
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using the generalized logistic regression model. \cr
#' \code{\link[stats]{logLik}} for a generic function extracting log-likelihood. \cr
#' \code{\link[stats]{AIC}} for a generic function calculating AIC and BIC.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # AIC, BIC, log-likelihood
#' AIC(x)
#' BIC(x)
#' logLik(x)
#'
#' # AIC, BIC, log-likelihood for the first item
#' AIC(x, item = 1)
#' BIC(x, item = 1)
#' logLik(x, item = 1)
#' }
#' @export
logLik.difNLR <- function(object, item = "all", ...) {
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (inherits(item, "character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item, "integer") & !inherits(item, "numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
          call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  if (any(object$conv.fail.which %in% items)) {
    if (length(setdiff(items, object$conv.fail.which)) == 0) {
      stop(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
          collapse = "\n"
        ),
        call. = FALSE
      )
    } else {
      warning(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
          collapse = "\n"
        ),
        call. = FALSE
      )
      ITEMS <- setdiff(items, object$conv.fail.which)
    }
  } else {
    ITEMS <- items
  }

  val <- df <- rep(NA, m)

  val[ITEMS] <- ifelse(ITEMS %in% object$DIFitems,
    object$llM1[ITEMS],
    object$llM0[ITEMS]
  )
  df[ITEMS] <- ifelse(ITEMS %in% object$DIFitems,
    sapply(object$parM1, length)[ITEMS],
    sapply(object$parM0, length)[ITEMS]
  )
  val <- val[items]
  df <- df[items]
  if (length(items) == 1) {
    attr(val, "df") <- df
    class(val) <- "logLik"
  }
  return(val)
}

#' @rdname logLik.difNLR
#' @aliases BIC.difNLR logLik.difNLR
#' @export
AIC.difNLR <- function(object, item = "all", ...) {
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (inherits(item, "character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item, "integer") & !inherits(item, "numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
          call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  if (any(object$conv.fail.which %in% items)) {
    if (length(setdiff(items, object$conv.fail.which)) == 0) {
      stop(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
          collapse = "\n"
        ),
        call. = FALSE
      )
    } else {
      warning(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
          collapse = "\n"
        ),
        call. = FALSE
      )
      ITEMS <- setdiff(items, object$conv.fail.which)
    }
  } else {
    ITEMS <- items
  }

  k <- AIC <- rep(NA, m)
  k[ITEMS] <- ifelse(ITEMS %in% object$DIFitems,
    sapply(object$parM1, length)[ITEMS],
    sapply(object$parM0, length)[ITEMS]
  )
  AIC[ITEMS] <- 2 * k[ITEMS] - 2 * logLik(object, item = ITEMS)
  AIC <- AIC[items]

  return(AIC)
}

#' @rdname logLik.difNLR
#' @aliases AIC.difNLR logLik.difNLR
#' @export
BIC.difNLR <- function(object, item = "all", ...) {
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (inherits(item, "character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item, "integer") & !inherits(item, "numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
          call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  if (any(object$conv.fail.which %in% items)) {
    if (length(setdiff(items, object$conv.fail.which)) == 0) {
      stop(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
          collapse = "\n"
        ),
        call. = FALSE
      )
    } else {
      warning(
        paste0("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
          collapse = "\n"
        ),
        call. = FALSE
      )
      ITEMS <- setdiff(items, object$conv.fail.which)
    }
  } else {
    ITEMS <- items
  }
  k <- BIC <- rep(NA, m)
  k[ITEMS] <- ifelse(ITEMS %in% object$DIFitems,
    sapply(object$parM1, length)[ITEMS],
    sapply(object$parM0, length)[ITEMS]
  )
  n <- dim(object$Data)[1]
  BIC[ITEMS] <- log(n) * k[ITEMS] - 2 * logLik(object, item = ITEMS)
  BIC <- BIC[items]

  return(BIC)
}

#' Fitted values and residuals for an object of the \code{"difNLR"} class.
#'
#' @aliases resid.difNLR fitted.difNLR
#' @rdname residuals.difNLR
#'
#' @description
#' S3 methods for extracting fitted values and residuals for an object of the
#' \code{"difNLR"} class.
#'
#' @param object an object of the \code{"difNLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all
#'   converged items (default), or a vector of item names (column names of the
#'   \code{Data}), or item identifiers (integers specifying the column number).
#' @param ... other generic parameters for S3 methods.
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
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using the generalized logistic regression model. \cr
#' \code{\link[stats]{fitted}} for a generic function extracting fitted values. \cr
#' \code{\link[stats]{residuals}} for a generic function extracting residuals.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # fitted values
#' fitted(x)
#' fitted(x, item = 1)
#' fitted(x, item = x$DIFitems)
#'
#' # residuals
#' residuals(x)
#' residuals(x, item = 1)
#' residuals(x, item = x$DIFitems)
#' }
#'
#' @export
residuals.difNLR <- function(object, item = "all", ...) {
  # checking input
  n <- dim(object$Data)[1]

  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (inherits(item, "character")) {
    if (any(item != "all") & !all(item %in% nams)) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (any(item == "all")) {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (!inherits(item, "integer") & !inherits(item, "numeric")) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    } else {
      if (!all(item %in% 1:m)) {
        stop("Invalid number for 'item'.",
          call. = FALSE
        )
      } else {
        items <- item
      }
    }
  }

  if (any(object$conv.fail.which %in% items)) {
    if (length(setdiff(items, object$conv.fail.which)) == 0) {
      stop(
        paste("Item", intersect(object$conv.fail.which, items), "does not converge.",
          collapse = "\n"
        ),
        call. = FALSE
      )
    } else {
      warning(
        paste("Item", intersect(object$conv.fail.which, items), "does not converge. NA produced.",
          collapse = "\n"
        ),
        call. = FALSE
      )
      ITEMS <- setdiff(items, object$conv.fail.which)
    }
  } else {
    ITEMS <- items
  }

  residuals <- matrix(NA, nrow = n, ncol = m)
  residuals[, ITEMS] <- as.matrix(object$Data[, ITEMS] - fitted(object, item = ITEMS))

  colnames(residuals) <- colnames(object$Data)
  rownames(residuals) <- rownames(object$Data)
  residuals <- residuals[, items]

  return(residuals)
}
