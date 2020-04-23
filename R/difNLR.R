#' DIF detection using non-linear regression method.
#'
#' @aliases difNLR
#'
#' @description Performs DIF detection procedure for dichotomous data based on non-linear
#' regression model (generalized logistic regression) and either likelihood-ratio or F test
#' of a submodel.
#'
#' @param Data data.frame or matrix: dataset which rows represent scored examinee answers (\code{"1"}
#' correct, \code{"0"} incorrect) and columns correspond to the items. In addition, \code{Data} can
#' hold the vector of group membership.
#' @param group numeric or character: a dichotomous vector of the same length as \code{nrow(Data)}
#' or a column identifier of \code{Data}.
#' @param focal.name numeric or character: indicates the level of \code{group} which corresponds to
#' focal group.
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param constraints character: which parameters should be the same for both groups. Possible values
#' are any combinations of parameters \code{"a"}, \code{"b"}, \code{"c"}, and \code{"d"}.
#' See \strong{Details}.
#' @param type character: type of DIF to be tested. Possible values are \code{"all"} for detecting
#' difference in any parameter (default), \code{"udif"} for uniform DIF only (i.e., difference in
#' difficulty parameter \code{"b"}), \code{"nudif"} for non-uniform DIF only (i.e., difference in
#' discrimination parameter \code{"a"}), \code{"both"} for uniform and non-uniform DIF (i.e.,
#' difference in parameters \code{"a"} and \code{"b"}), or combination of parameters \code{"a"},
#' \code{"b"}, \code{"c"}, and \code{"d"}. Can be specified as a single value (for all items) or as
#' an item-specific vector.
#' @param method character: method used to estimate parameters. Either \code{"nls"} for
#' non-linear least squares (default), or \code{"likelihood"} for maximum likelihood method.
#' @param match numeric or character: matching criterion to be used as an estimate of trait. Can be
#' either \code{"zscore"} (default, standardized total score), \code{"score"} (total test score),
#' or vector of the same length as number of observations in \code{Data}.
#' @param anchor numeric or character: specification of DIF free items. Either \code{NULL} (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number) determining which items are currently considered as anchor (DIF free) items.
#' Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' @param purify logical: should the item purification be applied? (default is \code{FALSE}).
#' @param nrIter numeric: the maximal number of iterations in the item purification (default is 10).
#' @param test character: test to be performed for DIF detection. Can be either \code{"LR"} for
#' likelihood ratio test of a submodel (default), or \code{"F"} for F-test of a submodel.
#' @param alpha numeric: significance level (default is 0.05).
#' @param p.adjust.method character: method for multiple comparison correction. Possible values are
#' \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"},
#' \code{"fdr"}, and \code{"none"} (default). For more details see \code{\link[stats]{p.adjust}}.
#' @param start numeric: initial values for estimation of parameters. If not specified, starting
#' values are calculated with \code{\link[difNLR]{startNLR}} function. Otherwise, list with as many
#' elements as a number of items. Each element is a named numeric vector of length 8 representing initial
#' values for parameter estimation. Specifically, parameters \code{"a"}, \code{"b"}, \code{"c"}, and
#' \code{"d"} are initial values for discrimination, difficulty, guessing, and inattention for reference
#' group. Parameters \code{"aDif"}, \code{"bDif"}, \code{"cDif"}, and \code{"dDif"} are then differences
#' in these parameters between reference and focal group.
#' @param initboot logical: in case of convergence issues, should be starting values re-calculated based
#' on bootstraped samples? (default is \code{TRUE}; newly calculated initial values are applied only to
#' items/models with convergence issues).
#' @param nrBo numeric: the maximal number of iterations for calculation of starting values using
#' bootstraped samples (default is 20).
#'
#' @usage
#' difNLR(Data, group, focal.name, model, constraints, type = "all", method = "nls",
#'        match = "zscore", anchor = NULL, purify = FALSE, nrIter = 10, test = "LR",
#'        alpha = 0.05, p.adjust.method = "none", start, initboot = T, nrBo = 20)
#'
#' @details
#' DIF detection procedure based on non-linear regression is the extension of logistic regression
#' procedure (Swaminathan and Rogers, 1990; Drabinova and Martinkova, 2017).
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
#' Missing values are allowed but discarded for item estimation. They must be coded as
#' \code{NA} for both, \code{Data} and \code{group} arguments.
#'
#' In case that model considers difference in guessing or inattention parameter, the different parameterization is
#' used and parameters with standard errors are re-calculated by delta method. However, covariance matrices stick
#' with alternative parameterization.
#'
#' @return The \code{difNLR()} function returns an object of class \code{"difNLR"}. The output
#' including values of the test statistics, p-values, and items detected as function differently
#' is displayed by the \code{print()} method.
#'
#' Object of class \code{"difNLR"} is a list with the following components:
#' \describe{
#'   \item{\code{Sval}}{the values of the test statistics.}
#'   \item{\code{nlrPAR}}{the estimates of final model.}
#'   \item{\code{nlrSE}}{the standard errors of estimates of final model.}
#'   \item{\code{parM0}}{the estimates of null model.}
#'   \item{\code{seM0}}{the standard errors of estimates of null model.}
#'   \item{\code{covM0}}{the covariance matrices of estimates of null model.}
#'   \item{\code{llM0}}{log-likelihood of null model.}
#'   \item{\code{parM1}}{the estimates of alternative model.}
#'   \item{\code{seM1}}{the standard errors of estimates of alternative model.}
#'   \item{\code{covM1}}{the covariance matrices of estimates of alternative model.}
#'   \item{\code{llM1}}{log-likelihood of alternative model.}
#'   \item{\code{DIFitems}}{either the column identifiers of the items which were detected as DIF, or
#'   \code{"No DIF item detected"} in case no item was detected as function differently.}
#'   \item{\code{model}}{fitted model.}
#'   \item{\code{constraints}}{constraints for the \code{model}.}
#'   \item{\code{type}}{character: type of DIF that was tested. If parameters were specified, the value is \code{"other"}.}
#'   \item{\code{types}}{character: the parameters (specified by user, \code{type} has value \code{"other"}) which were
#'   tested for difference.}
#'   \item{\code{p.adjust.method}}{character: method for multiple comparison correction which was applied.}
#'   \item{\code{pval}}{the p-values of the test.}
#'   \item{\code{adj.pval}}{the adjusted p-values of the test using \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of the test.}
#'   \item{\code{test}}{used test.}
#'   \item{\code{purification}}{\code{purify} value.}
#'   \item{\code{nrPur}}{number of iterations in item purification process. Returned only if \code{purify}
#'   is \code{TRUE}.}
#'   \item{\code{difPur}}{a binary matrix with one row per iteration of item purification and one column per item.
#'   \code{"1"} in i-th row and j-th column means that j-th item was identified as DIF in i-th iteration. Returned only
#'   if \code{purify} is \code{TRUE}.}
#'   \item{\code{conv.puri}}{logical: indicating whether item purification process converged before the maximal number
#'   \code{nrIter} of iterations. Returned only if \code{purify} is \code{TRUE}.}
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
#' For an object of class \code{"difNLR"} several methods are available (e.g. \code{methods(class = "difNLR")}).
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
#' Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27(4), 361-370,
#' \url{https://doi.org/10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso
#' \code{\link[difNLR]{plot.difNLR}} for graphical representation of item characteristic curves and DIF statistics. \cr
#' \code{\link[difNLR]{coef.difNLR}} for extraction of item parameters with their standard errors. \cr
#' \code{\link[difNLR]{predict.difNLR}} for prediction. \cr
#' \code{\link[difNLR]{fitted.difNLR}} and \code{\link[difNLR]{residuals.difNLR}} for extraction of fitted
#' values and residuals. \cr
#' \code{\link[difNLR]{logLik.difNLR}}, \code{\link[difNLR]{AIC.difNLR}}, \code{\link[difNLR]{BIC.difNLR}}
#' for extraction of loglikelihood and information criteria. \cr
#'
#' \code{\link[stats]{p.adjust}} for multiple comparison corrections. \cr
#' \code{\link[stats]{nls}} for nonlinear least squares estimation. \cr
#' \code{\link[difNLR]{startNLR}} for calculation of initial values of fitting algorithms in \code{difNLR()}.
#'
#' @examples
#' # Loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' \dontrun{
#' # Graphical devices
#' plot(x, item = x$DIFitems)
#' plot(x, item = "Item1")
#' plot(x, item = 1, group.names = c("Group 1", "Group 2"))
#' plot(x, plot.type = "stat")
#'
#' # Coefficients
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
#'
#' # Fitted values
#' fitted(x)
#' fitted(x, item = 1)
#'
#' # Residuals
#' residuals(x)
#' residuals(x, item = 1)
#'
#' # Predicted values
#' predict(x)
#' predict(x, item = 1)
#'
#' # Predicted values for new subjects
#' predict(x, item = 1, match = 0, group = 0)
#' predict(x, item = 1, match = 0, group = 1)
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
#' # Testing both DIF effects using F test and
#' # 3PL model with fixed guessing for groups
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "F")
#'
#' # Testing both DIF effects using LR test,
#' # 3PL model with fixed guessing for groups
#' # and Benjamini-Hochberg correction
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", p.adjust.method = "BH")
#'
#' # Testing both DIF effects using LR test,
#' # 3PL model with fixed guessing for groups
#' # and item purification
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", purify = TRUE)
#'
#' # Testing both DIF effects using 3PL model with fixed guessing for groups
#' # and total score as matching criterion
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "score")
#'
#' # Testing uniform DIF effects using 4PL model with the same
#' # guessing and inattention
#' difNLR(Data, group, focal.name = 1, model = "4PLcgdg", type = "udif")
#'
#' # Testing non-uniform DIF effects using 2PL model
#' difNLR(Data, group, focal.name = 1, model = "2PL", type = "nudif")
#'
#' # Testing difference in parameter b using 4PL model with fixed
#' # a and c parameters
#' difNLR(Data, group, focal.name = 1, model = "4PL", constraints = "ac", type = "b")
#'
#' # Testing both DIF effects using LR test,
#' # 3PL model with fixed guessing for groups
#' # with maximum likelihood estimation method
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "likelihood")
#' }
#'
#' @keywords DIF
#' @export
difNLR <- function(Data, group, focal.name, model, constraints, type = "all", method = "nls",
                   match = "zscore", anchor = NULL, purify = FALSE, nrIter = 10,
                   test = "LR", alpha = 0.05, p.adjust.method = "none", start,
                   initboot = T, nrBo = 20) {
  if (any(type == "nudif" & model == "1PL")) {
    stop("Detection of non-uniform DIF is not possible with 1PL model.", call. = FALSE)
  }
  if (any(type == "nudif" & model == "Rasch")) {
    stop("Detection of non-uniform DIF is not possible with Rasch model.", call. = FALSE)
  }
  # input check
  ### model
  if (missing(model)) {
    stop("'model' is missing", call. = FALSE)
  }
  ### constraints
  if (missing(constraints)) {
    constraints <- NULL
  }
  ### matching criterion
  if (!(match[1] %in% c("score", "zscore"))) {
    if (is.null(dim(Data))) {
      no <- length(Data)
    } else {
      no <- dim(Data)[1]
    }
    if (length(match) != no) {
      stop("Invalid value for 'match'. Possible values are 'zscore', 'score' or vector of the same length as number
           of observations in 'Data'.")
    }
  }
  ### purification
  if (purify & !(match[1] %in% c("score", "zscore"))) {
    stop("Purification not allowed when matching variable is not 'zscore' or 'score'.", call. = FALSE)
  }
  ### test
  if (!test %in% c("F", "LR") | !is.character(type)) {
    stop("Invalid value for 'test'. Test for DIF detection must be either 'F' or 'LR'.", call. = FALSE)
  }
  ### significance level
  if (alpha > 1 | alpha < 0) {
    stop("Argument 'alpha' must be between 0 and 1.", call. = FALSE)
  }
  ### starting values
  if (missing(start)) {
    start <- NULL
  }
  ### starting values with bootstraped samples
  if (initboot) {
    if (nrBo < 1) {
      stop("The maximal number of iterations for calculation of starting values using bootstraped
           samples 'nrBo' need to be greater than 1.", call. = FALSE)
    }
  }
  ### estimation method
  if (!(method %in% c("nls", "likelihood"))) {
    stop("Invalid value for 'method'. Estimation method must be either 'nls' or 'likelihood'.", call. = FALSE)
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
      stop("'group' must be binary vector", call. = FALSE)
    }
    if (is.matrix(DATA) | is.data.frame(DATA)) {
      if (!all(apply(DATA, 2, function(i) {
        length(levels(as.factor(i))) == 2
      }))) {
        stop("'Data' must be data frame or matrix of binary vectors.", call. = FALSE)
      }
      if (dim(DATA)[1] != length(GROUP)) {
        stop("'Data' must have the same number of rows as is length of vector 'group'.", call. = FALSE)
      }
    } else {
      if (is.vector(DATA)) {
        if (length(DATA) != length(GROUP)) {
          stop("'Data' must have the same number of rows as is length of vector 'group'.", call. = FALSE)
        }
        DATA <- as.data.frame(DATA)
      } else {
        stop("'Data' must be data frame or matrix of binary vectors.", call. = FALSE)
      }
    }
    group.names <- unique(GROUP)[!is.na(unique(GROUP))]
    if (group.names[1] == focal.name) {
      group.names <- rev(group.names)
    }
    GROUP <- as.numeric(as.factor(GROUP) == focal.name)

    if (length(match) == dim(DATA)[1]) {
      df <- data.frame(DATA, GROUP, match, check.names = F)
    } else {
      df <- data.frame(DATA, GROUP, check.names = F)
    }

    df <- df[complete.cases(df), ]

    if (dim(df)[1] == 0) {
      stop("It seems that your 'Data' does not include any subjects that are complete.",
        call. = FALSE
      )
    }

    GROUP <- df[, "GROUP"]
    DATA <- as.data.frame(df[, !(colnames(df) %in% c("GROUP", "match"))])
    colnames(DATA) <- colnames(df)[!(colnames(df) %in% c("GROUP", "match"))]

    if (length(match) > 1) {
      match <- df[, "match"]
    }

    ### model
    if (length(model) == 1) {
      model <- rep(model, dim(DATA)[2])
    } else {
      if (length(model) != dim(DATA)[2]) {
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

    ### type of DIF to be tested
    if (length(type) == 1) {
      type <- rep(type, dim(DATA)[2])
    } else {
      if (length(type) != dim(DATA)[2]) {
        stop("Invalid length of 'type'. Type of DIF need to be specified for each item or
             by single string. ", call. = FALSE)
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

    ### constraints
    if (!(is.null(constraints))) {
      if (length(constraints) == 1) {
        constraints <- rep(constraints, dim(DATA)[2])
      } else {
        if (length(constraints) != dim(DATA)[2]) {
          stop("Invalid length of 'constraints'. Constraints need to be specified for each item or
               by single string. ", call. = FALSE)
        }
      }
      constraints <- lapply(1:dim(DATA)[2], function(i) unique(unlist(strsplit(constraints[i], split = ""))))
      if (!all(sapply(1:dim(DATA)[2], function(i) {
        all(constraints[[i]] %in% letters[1:4]) | all(is.na(constraints[[i]]))
      }))) {
        stop("Constraints can be only combinations of parameters 'a', 'b', 'c', and 'd'. ", call. = FALSE)
      }
      if (any(!(type %in% c("udif", "nudif", "both", "all")))) {
        types <- as.list(rep(NA, dim(DATA)[2]))
        wht <- !(type %in% c("udif", "nudif", "both", "all"))
        types[wht] <- unlist(strsplit(type[wht], split = ""))
        if (any(sapply(1:dim(DATA)[2], function(i) (all(!is.na(constraints[[i]])) & (types[[i]] %in% constraints[[i]]))))) {
          stop("The difference in constrained parameters cannot be tested.", call. = FALSE)
        }
      }
    } else {
      constraints <- as.list(rep(NA, dim(DATA)[2]))
      types <- NULL
    }
    ### anchors
    if (!is.null(anchor)) {
      if (is.numeric(anchor)) {
        ANCHOR <- anchor
      } else {
        ANCHOR <- NULL
        for (i in 1:length(anchor)) {
          ANCHOR[i] <- (1:dim(DATA)[2])[colnames(DATA) == anchor[i]]
        }
      }
    } else {
      ANCHOR <- 1:dim(DATA)[2]
    }
    ### parameterization
    parameterization <- ifelse(model %in% c(
      "3PLc", "3PL", "3PLd", "4PLcgd", "4PLd",
      "4PLcdg", "4PLc", "4PL"
    ),
    "alternative",
    "classic"
    )
    if (!is.null(start)) {
      if (length(start) != dim(DATA)[2]) {
        stop("Invalid value for 'start'. Initial values must be a list with as many elements
             as number of items in Data.", call. = FALSE)
      }
      if (!all(unique(unlist(lapply(start, names))) %in% c(letters[1:4], paste(letters[1:4], "Dif", sep = "")))) {
        stop("Invalid names in 'start'. Each element of 'start' need to be a numeric vector
             with names 'a', 'b', 'c', 'd', 'aDif', 'bDif', 'cDif' and 'dDif'.", call. = FALSE)
      }
    }
    if (!purify | !(match[1] %in% c("zscore", "score")) | !is.null(anchor)) {
      PROV <- suppressWarnings(NLR(DATA, GROUP,
        model = model, constraints = constraints, type = type, method = method,
        match = match, anchor = ANCHOR, start = start, p.adjust.method = p.adjust.method,
        test = test, alpha = alpha, initboot = initboot, nrBo = nrBo
      ))
      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      significant <- which(ADJ.PVAL < alpha)

      nlrPAR <- nlrSE <- lapply(
        1:length(PROV$par.m1),
        function(i) {
          structure(rep(0, length(PROV$par.m1[[i]])),
            names = names(PROV$par.m1[[i]])
          )
        }
      )
      for (i in 1:length(PROV$par.m1)) {
        nlrPAR[[i]][names(PROV$par.m0[[i]])] <- PROV$par.m0[[i]]
        nlrSE[[i]][names(PROV$se.m0[[i]])] <- PROV$se.m0[[i]]
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
        conv.fail = PROV$conv.fail, conv.fail.which = PROV$conv.fail.which,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, match = match
      )
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(NLR(DATA, GROUP,
        model = model, constraints = constraints, type = type, method = method,
        match = match, start = start, p.adjust.method = p.adjust.method, test = test,
        alpha = alpha, initboot = initboot, nrBo = nrBo
      ))
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
              nodif <- 1:dim(DATA)[2]
            } else {
              for (i in 1:dim(DATA)[2]) {
                if (sum(i == dif) == 0) {
                  nodif <- c(nodif, i)
                }
              }
            }
            prov2 <- suppressWarnings(NLR(DATA, GROUP,
              model = model, constraints = constraints, type = type, method = method,
              match = match, anchor = nodif, start = start, p.adjust.method = p.adjust.method,
              test = test, alpha = alpha, initboot = initboot, nrBo = nrBo
            ))
            stats2 <- prov2$Sval
            pval2 <- prov2$pval
            significant2 <- which(pval2 < alpha)
            if (length(significant2) == 0) {
              dif2 <- NULL
            } else {
              dif2 <- significant2
            }
            difPur <- rbind(difPur, rep(0, dim(DATA)[2]))
            difPur[nrPur + 1, dif2] <- 1
            if (length(dif) != length(dif2)) {
              dif <- dif2
            } else {
              dif <- sort(dif)
              dif2 <- sort(dif2)
              if (sum(dif == dif2) == length(dif)) {
                noLoop <- TRUE
                break
              }
              else {
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
            structure(rep(0, length(PROV$par.m1[[i]])),
              names = names(PROV$par.m1[[i]])
            )
          }
        )
        for (i in 1:length(PROV$par.m1)) {
          nlrPAR[[i]][names(PROV$par.m0[[i]])] <- PROV$par.m0[[i]]
          nlrSE[[i]][names(PROV$se.m0[[i]])] <- PROV$se.m0[[i]]
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
        rownames(difPur) <- paste("Step", 0:(dim(difPur)[1] - 1), sep = "")
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
        conv.fail = PROV$conv.fail, conv.fail.which = PROV$conv.fail.which,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, match = match
      )
    }
    if (PROV$conv.fail > 0) {
      warning(paste("Convergence failure in item", PROV$conv.fail.which, "\n"), call. = FALSE)
    }
    if (purify) {
      if (!noLoop) {
        warning(paste("Item purification process not converged after ",
          nrPur, ifelse(nrPur <= 1, " iteration.", " iterations."), "\n",
          "Results are based on the last iteration of the item purification.\n",
          sep = ""
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
  cat(paste("Detection of ",
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
    "using generalized logistic regression model",
    sep = ""
  ))
  cat(
    paste("\n\nGeneralized logistic regression ",
      switch(x$test,
        "F" = "F-test",
        "LR" = "likelihood ratio chi-square"
      ),
      " statistics",
      ifelse(length(unique(x$model)) == 1,
        paste(
          "\nbased on",
          switch(unique(x$model),
            "Rasch" = "Rasch model", "1PL" = "1PL model", "2PL" = "2PL model",
            "3PL" = "3PL model", "3PLcg" = "3PL model with fixed guessing for groups",
            "3PLdg" = "3PL model with fixed inattention parameter for groups",
            "3PLc" = "3PL model", "3PLd" = "3PL model with inattention parameter",
            "4PLcgdg" = "4PL model with fixed guessing and inattention parameter for groups",
            "4PLcgd" = "4PL model with fixed guessing for groups",
            "4PLd" = "4PL model with fixed guessing for groups",
            "4PLcdg" = "4PL model with fixed inattention parameter for groups",
            "4PLc" = "4PL model with fixed inattention parameter for groups",
            "4PL" = "4PL model"
          )
        ),
        ""
      ),
      sep = ""
    ),
    ifelse((!all(is.na(x$constraints)) & length(unique(x$constraints)) == 1),
      paste(
        "with constraints on",
        ifelse(length(unique(unlist(x$constraints))) == 1, "parameter", "parameters"),
        paste(unique(unlist(x$constraints)), collapse = ", "), "\n"
      ),
      "\n"
    )
  )
  cat(paste("\nParameters were estimated with", ifelse(x$method == "nls",
    "non-linear least squares\n",
    "maximum likelihood method\n"
  )))
  cat(paste("\nItem purification was",
    ifelse(x$purification, " ", " not "), "applied",
    ifelse(x$purification, paste(" with ", x$nrPur,
      ifelse(x$nrPur <= 1, " iteration.", " iterations."),
      sep = ""
    ), ""), "\n",
    sep = ""
  ))
  if (x$p.adjust.method == "none") {
    cat("No p-value adjustment for multiple comparisons\n\n")
  } else {
    cat(paste(
      "Multiple comparisons made with",
      switch(x$p.adjust.method,
        holm = "Holm", hochberg = "Hochberg", hommel = "Hommel",
        bonferroni = "Bonferroni", BH = "Benjamini-Hochberg",
        BY = "Benjamini-Yekutieli", fdr = "FDR"
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
      "LR" = c("Chisq-value", "P-value", "")
    )
  } else {
    tab <- format(round(cbind(x$Sval, x$pval, x$adj.pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 4)
    colnames(tab) <- switch(x$test,
      "F" = c("F-value", "P-value", "Adj. P-value", ""),
      "LR" = c("Chisq-value", "P-value", "Adj. P-value", "")
    )
  }

  rownames(tab) <- colnames(x$Data)

  print(tab, quote = F, digits = 4, zero.print = F)
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
    cat(paste("\nDetection thresholds: ", round(critical, 4), " (significance level: ", x$alpha, ")", sep = ""))
  }

  if (is.character(x$DIFitems)) {
    cat("\nNone of items is detected as DIF \n")
  } else {
    cat("\n\nItems detected as DIF items:")
    cat("\n", paste(colnames(x$Data)[x$DIFitems], "\n", sep = ""))
  }
}

#' ICC and test statistics plots for an object of \code{"difNLR"} class.
#'
#' @description Plot method for an object of \code{"difNLR"} class using \pkg{ggplot2}.
#'
#' Two types of plots are available. The first one is obtained by setting \code{plot.type = "cc"}
#' (default). The characteristic curves for an item specified in \code{item} argument are plotted.
#' Plotted curves represent the best model.
#'
#' The second plot is obtained by setting \code{plot.type = "stat"}. The  test statistics
#' (either LR-test, or F-test, depends on argument \code{test}) are displayed on the Y axis,
#' for each converged item. The detection threshold is displayed by a horizontal line and items
#' detected as DIF are printed with the red color. Only parameters \code{size} and \code{title}
#' are used.
#'
#' @param x an object of \code{"difNLR"} class.
#' @param plot.type character: type of plot to be plotted (either \code{"cc"} for characteristic curve
#' (default), or \code{"stat"} for test statistics).
#' @param item numeric or character: either character \code{"all"} to apply for all converged items (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number).
#' @param col character: single value, or vector of two values representing colors for plot.
#' @param shape integer: shape parameter for plot.
#' @param size numeric: single number, or vector of two numbers representing line width in plot.
#' @param linetype numeric: single number, or vector of two numbers representing line type in plot for
#' reference and focal group.
#' @param title string: title of a plot.
#' @param group.names character: names of reference and focal group.
#' @param ... other generic parameters for \code{plot()} function.
#'
#' @return For an option \code{plot.type = "stat"}, returns object of class \code{"ggplot"}. In case of
#' \code{plot.type = "cc"}, returns list of objects of class \code{"ggplot"}.
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
#' Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27(4), 361-370,
#' \url{https://doi.org/10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using generalized logistic regression model. \cr
#' \code{\link[ggplot2]{ggplot}} for general function to plot a \code{"ggplot"} object.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # Graphical devices - characteristic curves
#' plot(x)
#' plot(x, item = x$DIFitems)
#' plot(x, item = 1)
#' plot(x, item = "Item1")
#'
#' # Graphical devices - test statistics
#' plot(x, plot.type = "stat")
#' }
#' @export
plot.difNLR <- function(x, plot.type = "cc", item = "all",
                        col = c("dodgerblue2", "goldenrod2"), shape = 21,
                        size = .8, linetype = c(2, 1), title, group.names, ...) {
  plotstat <- function(x, size = size, title = title) {
    if (x$conv.fail != 0) {
      if (length(x$conv.fail) == sum(!is.na(x$Sval))) {
        switch(x$test, "F" = stop("None of items does converge.
                                  F-statistic values not plotted", call. = FALSE),
          "LR" = stop("None of items does converge.
                           LR-statistic values not plotted", call. = FALSE)
        )
      } else {
        switch(x$test, "F" = warning(paste("Item ", x$conv.fail.which,
          " does not converge. F-statistic value not plotted",
          sep = "", collapse = "\n"
        ), call. = FALSE),
        "LR" = warning(paste("Item ", x$conv.fail.which,
          " does not converge. LR-statistic value not plotted",
          sep = "", collapse = "\n"
        ), call. = FALSE)
        )
      }
    }

    if (missing(title)) {
      title <- "Non-linear regression DIF detection with none multiple comparison correction"
    }
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
    plot_stat <- ggplot(hv, aes_string(
      x = "V1", y = "V2",
      label = "V1",
      col = "g"
    )) +
      ### points
      geom_text() +
      scale_color_manual(values = c("black", "red")) +
      ### critical value
      geom_hline(yintercept = Sval_critical, size = size) +
      ### theme
      ggtitle(title) +
      labs(x = "Item", y = switch(x$test,
        "F" = "F-statistic",
        "LR" = "Chisq-statistic"
      )) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", vjust = 1.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      )

    return(plot_stat)
  }

  if (missing(group.names)) {
    group.names <- x$group.names
    if (all(group.names %in% c(0, 1))) group.names <- c("Reference", "Focal")
  }

  plotCC <- function(x, item = item,
                     col = col, shape = shape, size = size,
                     linetype = linetype, title = title, group.names = group.names) {
    m <- length(x$nlrPAR)
    nams <- colnames(x$Data)
    if (class(item) == "character") {
      if (item != "all" & !item %in% nams) {
        stop("Invalid value for 'item'. Item must be either character 'all', or
             numeric vector corresponding to column identifiers, or name of the item.",
          call. = FALSE
        )
      }
      if (item[1] == "all") {
        items <- 1:m
      } else {
        items <- which(nams %in% item)
      }
    } else {
      if (class(item) != "integer" & class(item) != "numeric") {
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

    if (length(col) == 1) {
      col <- rep(col, 2)
    } else {
      if (length(col) > 2) {
        col <- col[1:2]
        warning("Length of 'col' is greater than 2. Only first two values are used.",
          call. = FALSE
        )
      }
    }
    if (any(x$conv.fail.which %in% items)) {
      if (length(setdiff(items, x$conv.fail.which)) == 0) {
        stop(paste("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted.",
          sep = "", collapse = "\n"
        ),
        call. = FALSE
        )
      } else {
        warning(paste("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted.",
          sep = "", collapse = "\n"
        ),
        call. = FALSE
        )
        items <- setdiff(items, x$conv.fail.which)
      }
    }
    if (length(linetype) != 2) {
      if (length(linetype) == 1) {
        linetype <- rep(linetype, 2)
      } else {
        linetype <- linetype[1:2]
        warning("Length of 'linetype' is greater than 2. Only first two values are used.",
          call. = FALSE
        )
      }
    }
    if (length(group.names) > 2) {
      group.names <- group.names[1:2]
      warning("Only first two values for 'group.names' argument are used.", call. = FALSE)
    } else {
      if (length(group.names) < 2) {
        group.names <- c("Reference", "Focal")
        warning("Argument 'group.names' need to have length of two. Default value is used.", call. = FALSE)
      }
    }


    if (x$purification) {
      anchor <- c(1:m)[!c(1:m) %in% x$DIFitems]
    } else {
      anchor <- 1:m
    }

    ### Data
    if (length(x$match) > 1) {
      xlab <- "Matching criterion"
      xR <- x$match[x$group == 0]
      xF <- x$match[x$group == 1]
    } else {
      if (x$match == "score") {
        xlab <- "Total score"
        xR <- c(apply(x$Data[x$group == 0, anchor], 1, sum))
        xF <- c(apply(x$Data[x$group == 1, anchor], 1, sum))
      } else {
        xlab <- "Standardized total score"
        xR <- c(scale(apply(x$Data[x$group == 0, anchor], 1, sum)))
        xF <- c(scale(apply(x$Data[x$group == 1, anchor], 1, sum)))
      }
    }

    max_sts <- max(
      as.numeric(levels(as.factor(xR))),
      as.numeric(levels(as.factor(xF)))
    )
    min_sts <- min(
      as.numeric(levels(as.factor(xR))),
      as.numeric(levels(as.factor(xF)))
    )
    alpha <- 0.5
    plot_CC <- list()

    for (i in items) {
      hv_R <- data.frame(cbind(
        as.numeric(levels(as.factor(xR))),
        tapply(
          x$Data[x$group == 0, i],
          as.factor(xR), mean
        )
      ))
      hv_F <- data.frame(cbind(
        as.numeric(levels(as.factor(xF))),
        tapply(
          x$Data[x$group == 1, i],
          as.factor(xF), mean
        )
      ))
      hv <- data.frame(rbind(
        cbind(hv_R, Group = "Reference"),
        cbind(hv_F, Group = "Focal")
      ))
      rownames(hv) <- 1:dim(hv)[1]
      hv$size <- c(table(xR), table(xF))

      if (!missing(title)) {
        TITLE <- title
      } else {
        TITLE <- colnames(x$Data)[i]
      }

      PAR <- data.frame(
        a = 1, b = 0, c = 0, d = 1,
        aDif = 0, bDif = 0, cDif = 0, dDif = 0
      )
      PAR[names(x$nlrPAR[[i]])] <- x$nlrPAR[[i]]
      PARR <- PAR[1:4]
      PARF <- PAR[1:4] + PAR[5:8]

      plot_CC[[i]] <- ggplot(hv, aes_string("X1", "X2")) +
        ### points
        geom_point(aes_string(
          colour = "Group",
          fill = "Group",
          size = "size"
        ),
        alpha = alpha, shape = shape
        ) +
        ### lines
        stat_function(aes(
          colour = "Reference",
          linetype = "Reference"
        ),
        fun = .gNLR_group,
        args = as.list(PARR),
        size = size,
        geom = "line"
        ) +
        stat_function(aes(colour = "Focal", linetype = "Focal"),
          fun = .gNLR_group,
          args = as.list(PARF),
          size = size,
          geom = "line"
        ) +
        ### style
        scale_colour_manual(breaks = levels(hv$Group), values = col, name = "Group", labels = group.names) +
        scale_fill_manual(breaks = levels(hv$Group), values = col, name = "Group", labels = group.names) +
        scale_linetype_manual(breaks = levels(hv$Group), values = linetype, name = "Group", labels = group.names) +
        ### theme
        ggtitle(TITLE) +
        labs(x = xlab, y = "Probability of correct answer") +
        scale_y_continuous(limits = c(0, 1)) +
        theme_bw() +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = "white", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)
        ) +
        ### legend
        theme(
          legend.box.just = "top",
          legend.justification = c("left", "top"),
          legend.position = c(0.02, 0.98),
          legend.box = "horizontal",
          legend.box.margin = margin(3, 3, 3, 3)
        ) +
        guides(
          size = guide_legend(title = "Count", order = 1),
          colour = guide_legend(title = "Group", order = 2),
          fill = guide_legend(title = "Group", order = 2),
          linetype = guide_legend(title = "Group", order = 2)
        )
    }
    plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
    return(plot_CC)
  }
  ### checking input
  if (!(plot.type %in% c("cc", "stat"))) {
    stop("Invalid value for 'plot.type'. Possible values of 'plot.type' is 'cc' or 'stat'.",
      call. = FALSE
    )
  } else {
    if (plot.type == "cc") {
      plotCC(x,
        item = item,
        col = col, shape = shape, size = size,
        linetype = linetype, title = title, group.names = group.names
      )
    } else {
      plotstat(x, size = size, title = title)
    }
  }
}

#' @rdname residuals.difNLR
#' @aliases residuals.difNLR
#' @export
fitted.difNLR <- function(object, item = "all", ...) {
  ### checking input
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
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
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
        sep = "", collapse = "\n"
      ),
      call. = FALSE
      )
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
        sep = "", collapse = "\n"
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

  ### data
  if (length(object$match) > 1) {
    match <- object$match
  } else {
    if (object$match == "score") {
      match <- c(apply(object$Data, 1, sum))
    } else {
      match <- c(scale(apply(object$Data, 1, sum)))
    }
  }

  ### fitted values
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

#' Predicted values for an object of \code{"difNLR"} class.
#'
#' @description S3 method for predictions from the model used in the object if \code{"difNLR"} class.
#'
#' @param object an object of \code{"difNLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all converged items (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number).
#' @param match numeric: matching criterion for new observations.
#' @param group numeric: group membership for new observations.
#' @param interval character: type of interval calculation, either \code{"none"} (default) or \code{"confidence"}
#' for confidence interval.
#' @param level numeric: confidence level.
#' @param ... other generic parameters for \code{predict()} function.
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
#' Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27(4), 361-370,
#' \url{https://doi.org/10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using generalized logistic regression model. \cr
#' \code{\link[stats]{predict}} for generic function for prediction.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # Predicted values
#' summary(predict(x))
#' predict(x, item = 1)
#' predict(x, item = "Item1")
#'
#' # Predicted values for new observations - average score
#' predict(x, item = 1, match = 0, group = 0) # reference group
#' predict(x, item = 1, match = 0, group = 1) # focal group
#'
#' # Predicted values for new observations - various z-scores and groups
#' new.match <- rep(c(-1, 0, 1), 2)
#' new.group <- rep(c(0, 1), each = 3)
#' predict(x, item = 1, match = new.match, group = new.group)
#'
#' # Predicted values for new observations with confidence intervals
#' predict(x, item = 1, match = new.match, group = new.group, interval = "confidence")
#' predict(x, item = c(1, 2), match = new.match, group = new.group, interval = "confidence")
#' }
#'
#' @export
predict.difNLR <- function(object, item = "all", match, group, interval = "none", level = 0.95, ...) {
  ### checking input
  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
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
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
        sep = "", collapse = "\n"
      ),
      call. = FALSE
      )
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
        sep = "", collapse = "\n"
      ),
      call. = FALSE
      )
      ITEMS <- setdiff(items, object$conv.fail.which)
    }
  } else {
    ITEMS <- items
  }

  if (!interval %in% c("none", "confidence")) {
    warning(
      "Only confidence interval is supported. ",
      call. = FALSE
    )
    interval <- "none"
  }

  PAR <- data.frame(
    a = rep(1, m), b = 0, c = 0, d = 1,
    aDif = 0, bDif = 0, cDif = 0, dDif = 0
  )
  for (i in ITEMS) {
    PAR[i, names(object$nlrPAR[[i]])] <- object$nlrPAR[[i]]
  }

  ### predicted values
  NEW <- lapply(1:m, function(i) {
    if (i %in% ITEMS) {
      .gNLR(
        match, group, PAR[i, "a"], PAR[i, "b"],
        PAR[i, "c"], PAR[i, "d"],
        PAR[i, "aDif"], PAR[i, "bDif"],
        PAR[i, "cDif"], PAR[i, "dDif"]
      )
    } else {
      NA
    }
  })

  if (interval == "confidence") {
    DELTA_new <- as.list(rep(NA, m))
    DELTA_new[ITEMS] <- lapply(ITEMS, function(i) {
      attr(.delta.gNLR(
        match, group, PAR[i, "a"], PAR[i, "b"],
        PAR[i, "c"], PAR[i, "d"],
        PAR[i, "aDif"], PAR[i, "bDif"],
        PAR[i, "cDif"], PAR[i, "dDif"]
      ), "gradient")
    })

    VCOV_par <- matrix(
      0,
      ncol = 8, nrow = 8,
      dimnames = list(
        c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif"),
        c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
      )
    )
    VCOV_par <- lapply(1:m, function(i) VCOV_par)
    VCOV_par <- lapply(ITEMS, function(i) {
      if (i %in% object$DIFitems) {
        tmp <- colnames(object$covM1[[i]])
        VCOV_par[[i]][tmp, tmp] <- object$covM1[[i]]
      } else {
        VCOV_par <- colnames(object$covM0[[i]])
        VCOV_par[[i]][tmp, tmp] <- object$covM0[[i]]
      }
      return(VCOV_par[[i]])
    })

    SE_new <- lapply(ITEMS, function(i) {
      sqrt(diag(DELTA_new[[i]] %*% VCOV_par[[i]] %*% t(DELTA_new[[i]])))
    })

    alpha <- 1 - level
    n <- dim(object$Data)[[1]]
    d <- sapply(1:m, function(i) {
      ifelse(i %in% object$DIFitems, length(object$parM1[[i]]), length(object$parM0[[i]]))
    })
    df <- n - d

    const <- lapply(ITEMS, function(i) SE_new[[i]] * qt(1 - alpha / 2, df = df)[ITEMS])

    lwr <- lapply(ITEMS, function(i) NEW[[i]] - const[[i]])
    upp <- lapply(ITEMS, function(i) NEW[[i]] + const[[i]])

    res <- lapply(ITEMS, function(i) {
      data.frame(cbind(
        item = colnames(object$Data)[i],
        match, group,
        prob = NEW[[i]],
        lwr.conf = lwr[[i]],
        upr.conf = upp[[i]]
      ))
    })
    res <- as.data.frame(do.call(rbind, res))
    res$match <- as.numeric(paste(res$match))
    res$prob <- as.numeric(paste(res$prob))
    res$lwr.conf <- as.numeric(paste(res$lwr.conf))
    res$upr.conf <- as.numeric(paste(res$upr.conf))
  } else {
    res <- lapply(ITEMS, function(i) {
      data.frame(cbind(
        item = colnames(object$Data)[i],
        match, group,
        prob = NEW[[i]]
      ))
    })
    res <- as.data.frame(do.call(rbind, res))
    res$match <- as.numeric(paste(res$match))
    res$prob <- as.numeric(paste(res$prob))
  }

  return(res)
}

#' Extract model coefficients from an object of \code{"difNLR"} class.
#'
#' @description S3 method for extracting model coefficients from an object of \code{"difNLR"} class.
#' @aliases coefficients.difNLR
#'
#' @param object an object of \code{"difNLR"} class.
#' @param SE logical: should the standard errors of estimated parameters be also returned? (default is \code{FALSE}).
#' @param simplify logical: should the estimated parameters be simplified to a matrix? (default is \code{FALSE}).
#' @param ... other generic parameters for \code{predict()} function.
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
#' Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27(4), 361-370,
#' \url{https://doi.org/10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using generalized logistic regression model. \cr
#' \code{\link[stats]{coef}} for generic function extracting model coefficients.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # Coefficients
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
#' }
#' @export
coef.difNLR <- function(object, SE = FALSE, simplify = FALSE, ...) {
  if (class(SE) != "logical") {
    stop("Invalid value for 'SE'. 'SE' need to be logical. ",
      call. = FALSE
    )
  }
  if (class(simplify) != "logical") {
    stop("Invalid value for 'simplify'. 'simplify' need to be logical. ",
      call. = FALSE
    )
  }

  m <- dim(object$Data)[2]
  nams <- colnames(object$Data)

  coefs <- object$nlrPAR
  names(coefs) <- nams

  if (SE) {
    se <- object$nlrSE
    names(se) <- nams
    coefs <- lapply(nams, function(i) rbind(coefs[[i]], se[[i]]))
    coefs <- lapply(coefs, "rownames<-", c("estimate", "SE"))
    names(coefs) <- nams
  }

  if (simplify) {
    res <- as.data.frame(plyr::ldply(coefs, rbind))
    if (SE) {
      resnams <- paste(res[, 1], c("estimate", "SE"))
    } else {
      resnams <- res[, 1]
    }
    res <- res[, -1]
    rownames(res) <- resnams
    res[is.na(res)] <- 0
  } else {
    res <- coefs
  }

  return(res)
}

#' Loglikelihood and information criteria for an object of \code{"difNLR"} class.
#'
#' @aliases AIC.difNLR BIC.difNLR
#' @rdname logLik.difNLR
#'
#' @description S3 methods for extracting loglikelihood, Akaike's information criterion (AIC) and
#' Schwarz's Bayesian criterion (BIC) for an object of \code{"difNLR"} class.
#'
#' @param object an object of \code{"difNLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all converged items (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number).
#' @param ... other generic parameters for S3 methods.
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
#' Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27(4), 361-370,
#' \url{https://doi.org/10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using generalized logistic regression model. \cr
#' \code{\link[stats]{logLik}} for generic function extracting loglikelihood. \cr
#' \code{\link[stats]{AIC}} for generic function calculating AIC and BIC.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using likelihood-ratio test and
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
  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
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
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
        sep = "", collapse = "\n"
      ),
      call. = FALSE
      )
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
        sep = "", collapse = "\n"
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

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
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
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
        sep = "", collapse = "\n"
      ),
      call. = FALSE
      )
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
        sep = "", collapse = "\n"
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

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
             numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
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
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. ",
        sep = "", collapse = "\n"
      ),
      call. = FALSE
      )
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. NA produced.",
        sep = "", collapse = "\n"
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

#' Fitted values and residuals for an object of \code{"difNLR"} class.
#'
#' @aliases resid.difNLR fitted.difNLR
#' @rdname residuals.difNLR
#'
#' @description S3 methods for extracting fitted values and residuals for an object of \code{"difNLR"} class.
#'
#' @param object an object of \code{"difNLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all converged items (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number).
#' @param ... other generic parameters for S3 methods.
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
#' Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27(4), 361-370,
#' \url{https://doi.org/10.1111/j.1745-3984.1990.tb00754.x}
#'
#' @seealso
#' \code{\link[difNLR]{difNLR}} for DIF detection among binary data using generalized logistic regression model. \cr
#' \code{\link[stats]{fitted}} for generic function extracting fitted values. \cr
#' \code{\link[stats]{residuals}} for generic function extracting residuals.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#'
#' # Fitted values
#' fitted(x)
#' fitted(x, item = 1)
#'
#' # Residuals
#' residuals(x)
#' residuals(x, item = 1)
#' }
#' @export
residuals.difNLR <- function(object, item = "all", ...) {
  ### checking input
  n <- dim(object$Data)[1]

  m <- length(object$nlrPAR)
  nams <- colnames(object$Data)

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either character 'all', or
           numeric vector corresponding to column identifiers, or name of the item.",
        call. = FALSE
      )
    }
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
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
      stop(paste("Item", intersect(object$conv.fail.which, items), "does not converge.",
        collapse = "\n"
      ),
      call. = FALSE
      )
    } else {
      warning(paste("Item", intersect(object$conv.fail.which, items), "does not converge. NA produced.",
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

  residuals <- residuals[, items]

  return(residuals)
}

# private functions
.gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif) {
  return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
}

.gNLR_group <- function(x, a, b, c, d) {
  return(c + (d - c) / (1 + exp(-(a * (x - b)))))
}

.delta.gNLR <- deriv(y ~ (c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))),
  namevec = c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif"),
  function(x, g, a, b, c, d, aDif, bDif, cDif, dDif) {}
)
