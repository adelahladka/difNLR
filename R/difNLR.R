#' Performs DIF detection using Non-Linear Regression method.
#'
#' @aliases difNLR print.difNLR plot.difNLR fitted.difNLR predict.difNLR coef.difNLR
#'
#' @description Performs DIF detection procedure based on Non-Linear Regression model (generalized logistic
#' regression) and either likelihood-ratio or F test of submodel.
#'
#' @param Data numeric: either the scored data matrix only, or the scored data
#' matrix plus the vector of group. See \strong{Details}.
#' @param group numeric or character: either the binary vector of group membership or
#' the column indicator (in \code{Data}) of group membership. See \strong{Details}.
#' @param focal.name numeric or character: indicates the level of \code{group} which corresponds to
#' focal group
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param constraints character: which parameters should be the same for both groups. See \strong{Details}.
#' @param type character: type of DIF to be tested. Possible values are \code{"both"} (default), \code{"udif"},
#' \code{"nudif"}, \code{"all"}, or combination of parameters 'a', 'b', 'c' and 'd'. See \strong{Details}.
#' @param match specifies matching criterion. Can be either \code{"zscore"} (default, standardized total score),
#' \code{"score"} (total test score), or vector of the same length as number of observations in "Data". See \strong{Details}.
#' @param anchor Either \code{NULL} (default) or a vector of item names or item identifiers specifying which items are
#' currently considered as anchor (DIF free) items. Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' See \strong{Details}.
#' @param purify logical: should the item purification be applied? (default is \code{FALSE}). See \strong{Details}.
#' @param nrIter numeric: the maximal number of iterations in the item purification (default is 10).
#' @param test character: test to be performed for DIF detection (either \code{"LR"} (default), or \code{"F"}).
#' See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#' @param p.adjust.method character: method for multiple comparison correction. See \strong{Details}.
#' @param start numeric: matrix with n rows (where n is the number of items) and 8 columns
#' containing initial item parameters estimates. See \strong{Details}.
#' @param x an object of 'difNLR' class
#' @param object an object of 'difNLR' class
#' @param plot.type character: type of plot to be plotted (either \code{"cc"} for characteristic curve
#' (default), or \code{"stat"} for test statistics). See \strong{Details}.
#' @param item either character (\code{"all"}), or numeric vector, or single number corresponding to column indicators. See \strong{Details}.
#' @param col character: single value, or vector of two values representing colors for plot.
#' @param shape integer: shape parameter for plot.
#' @param size numeric: single number, or vector of two numbers representing line width in plot.
#' @param linetype numeric: single number, or vector of two numbers representing line type in plot for reference and focal group.
#' @param title string: title of plot.
#' @param score numeric: standardized total score of subject.
#' @param ... other generic parameters for \code{print()}, \code{plot()}, \code{fitted()},
#' \code{predict()} or \code{coef()} functions.
#'
#' @usage difNLR(Data, group, focal.name, model, constraints, type = "both",
#' match = "zscore", anchor = NULL, purify = FALSE, nrIter = 10, test = "LR",
#' alpha = 0.05, p.adjust.method = "none", start)
#'
#' @details
#' DIF detection procedure based on Non-Linear Regression is the extension of Logistic Regression
#' procedure (Swaminathan and Rogers, 1990).
#'
#' The \code{Data} is a matrix whose rows represents scored examinee answers ("1" correct,
#' "0" incorrect) and columns correspond to the items. In addition, \code{Data} can hold
#' the vector of group membership. If so, \code{group} is a column indicator of \code{Data}.
#' Otherwise, \code{group} must be either a vector of the same length as \code{nrow(Data)}.
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
#' in \code{Data}. Matching criterion is used in \code{NLR()} function as a covariate in non-linear regression model.
#'
#' A set of anchor items (DIF free) can be specified through the \code{anchor} argument. It need to be a vector of either
#' item names (as specified in column names of \code{Data}) or item identifiers (integers specifying the column number).
#' In case anchor items are provided, only these items are used to compute matching criterion \code{match}. If the \code{match}
#' argument is not either \code{"zscore"} or \code{"score"}, \code{anchor} argument is ignored. When anchor items are
#' provided, purification is not applied.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust()} function from the \code{stats}
#' package. Possible values are \code{"holm"}, \code{"hochberg"}, \code{"hommel"},
#' \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}.
#'
#' The \code{start} is a matrix with a number of rows equal to number of items and with 8 columns.
#' First 4 columns represent parameters (a, b, c, d) of generalized logistic regression model
#' for reference group. Last 4 columns represent differences of parameters (aDif, bDif, cDif, dDif)
#' of generalized logistic regression model between reference and focal group. If not specified, starting
#' values are calculated with \code{startNLR()} function.
#'
#' The output of the difNLR is displayed by the \code{print.difNLR()} function.
#'
#' Two types of plots are available. The first one is obtained by setting \code{plot.type = "cc"}
#' (default). The characteristic curve for item specified in \code{item} option is plotted. For default
#' option \code{"all"} of item, characteristic curves of all converged items are plotted. The drawn
#' curves represent best model.
#' The second plot is obtained by setting \code{plot.type = "stat"}. The  test statistics
#' (either LR-test, or F-test, depends on argument \code{test}) are displayed on the Y axis,
#' for each coverged item. The detection threshold is displayed by a horizontal line and items
#' detected as DIF are printed with the red color. Only parameters \code{size} and \code{title}
#' are used.
#'
#' Fitted values are extracted by the \code{fitted.difNLR()} function for item(s) specified in
#' \code{item} argument.
#'
#' Predicted values are produced by the \code{predict.difNLR()} function for item(s) specified in
#' \code{item} argument. \code{score} represents standardized total score of new subject and
#' \code{group} argument represents group membership of new subject.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as
#' \code{NA} for both, \code{data} and \code{group} parameters.
#'
#' In case that model considers difference in guessing or inattention parameter, the different parameterization is
#' used and parameters with standard errors are recalculated by delta method. However, covariance matrices stick with
#' alternative parameterization.
#'
#' @return A list of class 'difNLR' with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{nlrPAR}}{the estimates of final model.}
#'   \item{\code{nlrSE}}{the standard errors of estimates of final model.}
#'   \item{\code{parM0}}{the estimates of null model.}
#'   \item{\code{seM0}}{the standard errors of estimates of null model.}
#'   \item{\code{covM0}}{the covariance matrices of estimates of null model.}
#'   \item{\code{parM1}}{the estimates of alternative model.}
#'   \item{\code{seM1}}{the standard errors of estimates of alternative model.}
#'   \item{\code{covM1}}{the covariance matrices of estimates of alternative model.}
#'   \item{\code{alpha}}{numeric: significance level.}
#'   \item{\code{DIFitems}}{either the column indicators of the items which were detected as DIF, or
#'   \code{"No DIF item detected"}.}
#'   \item{\code{match}}{matching criterion.}
#'   \item{\code{model}}{fitted model.}
#'   \item{\code{type}}{character: type of DIF that was tested. If parameters were specified, the value is \code{"other"}.}
#'   \item{\code{types}}{character: the parameters (specified by user, \code{type} has value \code{"other"}) which were
#'   tested for difference.}
#'   \item{\code{p.adjust.method}}{character: method for multiple comparison correction which was applied.}
#'   \item{\code{pval}}{the p-values by likelihood ratio test.}
#'   \item{\code{adj.pval}}{the adjusted p-values by likelihood ratio test using \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of likelihood ratio test.}
#'   \item{\code{test}}{used test.}
#'   \item{\code{purification}}{\code{purify} value.}
#'   \item{\code{nrPur}}{number of iterations in item purification process. Returned only if \code{purify}
#'   is \code{TRUE}.}
#'   \item{\code{difPur}}{a binary matrix with one row per iteration of item purification and one column per item.
#'   "1" in i-th row and j-th column means that j-th item was identified as DIF in i-1-th iteration. Returned only
#'   if \code{purify} is \code{TRUE}.}
#'   \item{\code{conv.puri}}{logical indicating whether item purification process converged before the maximal number
#'   \code{nrIter} of iterations. Returned only if \code{purify} is \code{TRUE}.}
#'   \item{\code{group}}{the vector of group membership.}
#'   \item{\code{Data}}{the data matrix.}
#'   \item{\code{conv.fail}}{numeric: number of convergence issues.}
#'   \item{\code{conv.fail.which}}{the indicators of the items which did not converge.}
#'   \item{\code{llM0}}{log-likelihood of null model.}
#'   \item{\code{llM1}}{log-likelihood of alternative model.}
#' }
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
#' # Testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' (x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
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
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", purify = T)
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
#' # Graphical devices
#' plot(x)
#' plot(x, item = x$DIFitems)
#' plot(x, plot.type = "stat")
#'
#' # Fitted values
#' fitted(x)
#' fitted(x, item = 1)
#' # Predicted values
#' predict(x)
#' predict(x, item = 1)
#'
#' # Predicted values for new subjects
#' predict(x, item = 1, score = 0, group = 1)
#' predict(x, item = 1, score = 0, group = 0)
#'
#' # AIC, BIC, logLik
#' AIC(x)
#' BIC(x)
#' logLik(x)
#' }
#'
#' @keywords DIF
#' @export


difNLR <- function(Data, group, focal.name, model, constraints, type = "both",
                   match = "zscore", anchor = NULL, purify = FALSE, nrIter = 10,
                   test = "LR", alpha = 0.05, p.adjust.method = "none", start)
{
  if (type == "nudif" & model == "1PL")
    stop("Detection of non-uniform DIF is not possible with 1PL model!", call. = FALSE)
  if (type == "nudif" & model == "Rasch")
    stop("Detection of non-uniform DIF is not possible with Rasch model!", call. = FALSE)
  # input check
  ### model
  if (missing(model)) {
    stop("'model' is missing", call. = FALSE)
  } else {
    if (!(model %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLdg", "3PLc", "3PL", "3PLd", "4PLcgdg", "4PLcgd",
                       "4PLd", "4PLcdg", "4PLc", "4PL"))){
      stop("Invalid value for 'model'", call. = FALSE)
    }
  }
  ### constraints
  if (!(missing(constraints))){
    constraints <- unique(unlist(strsplit(constraints, split = "")))
    if (!all(constraints %in% letters[1:4])){
      stop("Constraints can be only 'a', 'b', 'c' or 'd'!")
    }
    if (!(type %in% c("udif", "nudif", "both", "all"))){
      types <- unlist(strsplit(type, split = ""))
      if (length(intersect(types, constraints)) > 0){
        stop("The difference in constrained parameters cannot be tested!")
      }
    }
  } else {
    constraints <- NULL
  }
  ### type of DIF to be tested
  if (!(type %in% c("udif", "nudif", "both", "all"))){
    types <- unique(unlist(strsplit(type, split = "")))
    if (!all(types %in% letters[1:4])){
      stop("Type of DIF to be tested not recognized. Only parameters 'a', 'b', 'c' or 'd' can be tested
           or 'type' must be one of predefined options: either 'udif', 'nudif', 'both', or 'all'")
    }
  }
  ### matching criterion
  if (!(match[1] %in% c("score", "zscore"))){
    if (length(match) != dim(Data)[1]){
      stop("Invalid value for 'match'. Possible values are 'zscore', 'score' or vector of the same length as number
           of observations in 'Data'!")
    }
  }
  ### purification
  if (purify & !(match[1] %in% c("score", "zscore")))
    stop("purification not allowed when matching variable is not 'zscore' or 'score'",  call. = FALSE)
  ### test
  if (!test %in% c("F", "LR") | !is.character(type))
    stop("'test' must be either 'F' or 'LR'", call. = FALSE)
  ### significance level
  if (alpha > 1 | alpha < 0)
    stop("'alpha' must be between 0 and 1", call. = FALSE)
  ### starting values
  if (missing(start)) {
    start <- NULL
  }
  # internal NLR function
  internalNLR <- function() {
    if (length(group) == 1) {
      if (is.numeric(group)) {
        GROUP <- Data[, group]
        DATA <- Data[, (1:ncol(Data)) != group]
        colnames(DATA) <- colnames(Data)[(1:ncol(Data)) !=  group]
      } else {
        GROUP <- Data[, colnames(Data) == group]
        DATA <- Data[, colnames(Data) != group]
        colnames(DATA) <- colnames(Data)[colnames(Data) !=  group]
      }
    } else {
      GROUP <- group
      DATA <- Data
    }
    if (length(levels(as.factor(GROUP))) != 2)
      stop("'group' must be binary vector", call. = FALSE)
    if (is.matrix(DATA) | is.data.frame(DATA)) {
      if (!all(apply(DATA, 2, function(i) { length(levels(as.factor(i))) == 2 })))
        stop("'Data' must be data frame or matrix of binary vectors", call. = FALSE)
      if (nrow(DATA) != length(GROUP))
        stop("'Data' must have the same number of rows as is length of vector 'group'", call. = FALSE)
    } else {
      stop("'Data' must be data frame or matrix of binary vectors", call. = FALSE)
    }

    GROUP <- as.numeric(as.factor(GROUP) == focal.name)

    df <- data.frame(DATA, GROUP, check.names = F)
    df <- df[complete.cases(df), ]

    if (dim(df)[1] == 0){
      stop("It seems that your 'Data' does not include any subjects that are complete. ", call. = FALSE)
    }

    GROUP <- df[, "GROUP"]
    DATA <- data.frame(df[, colnames(df) != "GROUP"])

    if (!is.null(anchor)) {
      if (is.numeric(anchor)){
        ANCHOR <- anchor
        } else {
        ANCHOR <- NULL
        for (i in 1:length(anchor))
          ANCHOR[i] <- (1:ncol(DATA))[colnames(DATA) == anchor[i]]
      }
    } else {
      ANCHOR <- 1:ncol(DATA)
    }
    if (is.null(start)){
      if (model %in% c("3PLc", "3PL", "3PLd", "4PLcgd", "4PLd", "4PLcdg", "4PLc", "4PL")){
        parameterization <- "alternative"
        start <- startNLR(DATA, GROUP, model, match = match, parameterization = parameterization)
        } else {
          parameterization <- "classic"
          start <- startNLR(DATA, GROUP, model, match = match, parameterization = parameterization)
        }
    } else {
      if (ncol(start) != 8 | nrow(start) != ncol(DATA)){
        stop("'start' must be data frame or matrix with 8 columns and the number of its rows need
             to correspond to number of items!", call. = FALSE)
      }
      colnames(start) <- c(letters[1:4], paste(letters[1:4], "Dif", sep = ""))
    }
    if (!purify | !(match[1] %in% c("zscore", "score")) | !is.null(anchor)) {
      PROV <- suppressWarnings(NLR(DATA, GROUP, model = model, constraints = constraints, type = type, match = match,
                                 anchor = ANCHOR, start = start, p.adjust.method = p.adjust.method, test = test,
                                 alpha = alpha))
      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      significant <- which(ADJ.PVAL < alpha)

      nlrPAR <- nlrSE <- structure(data.frame(matrix(0, ncol = ncol(PROV$par.m0), nrow = nrow(PROV$par.m0))),
                                   .Names = colnames(PROV$par.m0))
      nlrPAR[, colnames(PROV$par.m1)] <- PROV$par.m1
      nlrSE[, colnames(PROV$par.m1)] <- PROV$se.m1

      if (length(significant) > 0) {
        DIFitems <- significant
        for (idif in 1:length(DIFitems)) {
          nlrPAR[DIFitems[idif], ] <- PROV$par.m0[DIFitems[idif], ]
          nlrSE[DIFitems[idif], ] <- PROV$se.m0[DIFitems[idif], ]
        }
      } else {
        DIFitems <- "No DIF item detected"
      }
      types <- NULL
      if (!(type %in% c("udif", "nudif", "both", "all"))){
        types <- unlist(strsplit(type, split = ""))
        type <- "other"
      }
      RES <- list(Sval = STATS,
                  nlrPAR = nlrPAR, nlrSE = nlrSE,
                  parM0 = PROV$par.m0, seM0 = PROV$se.m0, covM0 = PROV$cov.m0,
                  parM1 = PROV$par.m1, seM1 = PROV$se.m1, covM1 = PROV$cov.m1,
                  alpha = alpha, DIFitems = DIFitems, match = match,
                  model = model, constraints = constraints,
                  type = type, types = types, p.adjust.method = p.adjust.method,
                  pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df, test = test,
                  purification = purify,
                  group = GROUP, Data = DATA,
                  conv.fail = PROV$conv.fail, conv.fail.which = PROV$conv.fail.which,
                  llM0 = PROV$ll.m0, llM1 = PROV$ll.m1)
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(NLR(DATA, GROUP, model = model, constraints = constraints, type = type, match = match,
                                    start = start, p.adjust.method = p.adjust.method, test = test,
                                    alpha = alpha))
      stats1 <- prov1$Sval
      adj.pval1 <- prov1$adjusted.pval
      significant1 <- which(adj.pval1 < alpha)

      if (length(significant1) == 0) {
        PROV <- prov1
        STATS <- stats1
        DIFitems <- "No DIF item detected"
        nlrPAR <- nlrSE <- structure(data.frame(matrix(0, ncol = ncol(PROV$par.m0), nrow = nrow(PROV$par.m0))),
                                     .Names = colnames(PROV$par.m0))
        nlrPAR[, colnames(PROV$par.m1)] <- PROV$par.m1
        nlrSE[, colnames(PROV$par.m1)] <- PROV$se.m1
        noLoop <- TRUE
      } else {
        dif <- significant1
        difPur <- rep(0, length(stats1))
        difPur[dif] <- 1
        repeat {
          if (nrPur >= nrIter){
            break
            } else {
              nrPur <- nrPur + 1
              nodif <- NULL
              if (is.null(dif)) {
                nodif <- 1:ncol(DATA)
                } else {
                  for (i in 1:ncol(DATA)) {
                    if (sum(i == dif) == 0)
                      nodif <- c(nodif, i)
                  }
                }
              prov2 <- suppressWarnings(NLR(DATA, GROUP, model = model, constraints = constraints, type = type, match = match,
                                            anchor = nodif, start = start, p.adjust.method = p.adjust.method, test = test,
                                            alpha = alpha))
              stats2 <- prov2$Sval
              adj.pval2 <- prov2$adjusted.pval
              significant2 <- which(adj.pval2 < alpha)
              if (length(significant2) == 0)
                dif2 <- NULL
              else dif2 <- significant2
              difPur <- rbind(difPur, rep(0, ncol(DATA)))
              difPur[nrPur + 1, dif2] <- 1
              if (length(dif) != length(dif2))
                dif <- dif2
              else {
                dif <- sort(dif)
                dif2 <- sort(dif2)
                if (sum(dif == dif2) == length(dif)) {
                  noLoop <- TRUE
                  break
                  }
                else dif <- dif2
            }
          }
        }
        PROV <- prov2
        STATS <- stats2
        significant1 <- significant2
        nlrPAR <- nlrSE <- structure(data.frame(matrix(0, ncol = ncol(PROV$par.m0), nrow = nrow(PROV$par.m0))),
                                     .Names = colnames(PROV$par.m0))
        nlrPAR[, colnames(PROV$par.m1)] <- PROV$par.m1
        nlrSE[, colnames(PROV$par.m1)] <- PROV$se.m1
        if (length(significant1) > 0) {
          DIFitems <- significant1
          for (idif in 1:length(DIFitems)) {
            nlrPAR[DIFitems[idif], ] <- PROV$par.m0[DIFitems[idif], ]
            nlrSE[DIFitems[idif], ] <- PROV$se.m0[DIFitems[idif], ]
          }
        } else {
          DIFitems <- "No DIF item detected"
        }
      }
      if (is.null(difPur) == FALSE) {
        ro <- co <- NULL
        for (ir in 1:nrow(difPur)) ro[ir] <- paste("Step", ir - 1, sep = "")
        for (ic in 1:ncol(difPur)) co[ic] <- paste("Item", ic, sep = "")
        rownames(difPur) <- ro
        colnames(difPur) <- co
      }
      types <- NULL
      if (!(type %in% c("udif", "nudif", "both", "all"))){
        types <- unlist(strsplit(type, split = ""))
        type <- "other"
      }
      RES <- list(Sval = STATS,
                  nlrPAR = nlrPAR, nlrSE = nlrSE,
                  parM0 = PROV$par.m0, seM0 = PROV$se.m0, covM0 = PROV$cov.m0,
                  parM1 = PROV$par.m1, seM1 = PROV$se.m1, covM1 = PROV$cov.m1,
                  alpha = alpha, DIFitems = DIFitems, match = match,
                  model = model, constraints = constraints,
                  type = type, types = types, p.adjust.method = p.adjust.method,
                  pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df, test = test,
                  purification = purify, nrPur = nrPur, difPur = difPur, conv.puri = noLoop,
                  group = GROUP, Data = DATA,
                  conv.fail = PROV$conv.fail, conv.fail.which = PROV$conv.fail.which,
                  llM0 = PROV$ll.m0, llM1 = PROV$ll.m1)
    }
    class(RES) <- "difNLR"
    return(RES)
  }


  resToReturn <- internalNLR()
  return(resToReturn)
}


#' @rdname difNLR
#' @export
print.difNLR <- function (x, ...){
  cat(paste("Detection of",
            switch(x$type,
                   both = " both types of ", udif = " uniform ", nudif = " non-uniform ", all = " all types of ",
                   other = " "),
            "Differential Item Functioning\n",
            switch(x$type,
                   both = "", udif = "", nudif = "", all = "",
                   other = paste("caused by difference in", x$types, "parameter\n")),
            "using Generalized Logistic Regression Model", sep = ""))
  cat(paste("\n\nGeneralized Logistic Regression", switch(x$test, "F" = " F-test ", "LR" = " Likelihood Ratio chi-square "),
            "statistics \nbased on ",
            switch(x$model,
                   "Rasch" = "Rasch model", "1PL" = "1PL model", "2PL" = "2PL model",
                   "3PL" = "3PL model", "3PLcg" = "3PL model with fixed guessing for groups",
                   "3PLdg" = "3PL model with fixed inattention parameter for groups",
                   "3PLc" = "3PL model", "3PLd" = "3PL model with inattention parameter",
                   "4PLcgdg" = "4PL model with fixed guessing and inattention parameter for groups",
                   "4PLcgd" = "4PL model with fixed guessing for groups",
                   "4PLd" = "4PL model with fixed guessing for groups",
                   "4PLcdg" = "4PL model with fixed inattention parameter for groups",
                   "4PLc" = "4PL model with fixed inattention parameter for groups",
                   "4PL" = "4PL model"),
            ifelse(is.null(x$constraints), "",
                   paste(" with constraints on",
                         paste(unique(unlist(strsplit(x$constraints, split = ""))), collapse = ", "),
                         "parameter")), sep = ""), "\n")
  if (x$purification) word.iteration <- ifelse(x$nrPur <= 1, " iteration", " iterations")
  cat(paste("\nItem purification was", ifelse(x$purification, " ", " not "), "applied",
      ifelse(x$purification, paste(" with ", x$nrPur, word.iteration, sep = ""), ""), "\n", sep = ""))
  if (x$purification){
    if (!x$conv.puri){
      cat(paste("WARNING: Item purification process not converged after "), x$nrPur, word.iteration, "\n",
          "         Results are based on last iteration of the item purification.\n", sep = "")
    }
  }
  if (x$p.adjust.method == "none") {
    cat("No p-value adjustment for multiple comparisons\n\n")
  } else {
    cat(paste("\nMultiple comparisons made with",
              switch(x$p.adjust.method,
                     holm = "Holm", hochberg = "Hochberg", hommel = "Hommel",
                     bonferroni = "Bonferroni", BH = "Benjamini-Hochberg",
                     BY = "Benjamini-Yekutieli", fdr = "FDR"), "adjustment of p-values\n\n"))
  }
  sign <- ifelse(is.na(x$adj.pval), " ",
                 ifelse(x$adj.pval < 0.001, "***",
                        ifelse(x$adj.pval < 0.01, "**",
                               ifelse(x$adj.pval < 0.05, "*",
                                      ifelse(x$adj.pval < 0.1, ".", " ")))))
  if (x$p.adjust.method == "none"){
    tab <- format(round(cbind(x$Sval, x$pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 3)
    colnames(tab) <- switch(x$test,
                            "F" = c("F-value", "P-value", ""),
                            "LR" = c("Chisq-value", "P-value", ""))
  } else {
    tab <- format(round(cbind(x$Sval, x$pval, x$adj.pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 4)
    colnames(tab) <- switch(x$test,
                            "F" = c("F-value", "P-value", "Adj. P-value", ""),
                            "LR" = c("Chisq-value", "P-value", "Adj. P-value", ""))
  }

  rownames(tab) <- colnames(x$Data)

  print(tab, quote = F, digits = 4, zero.print = F)
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  critical <- ifelse(x$test == "F", qf(1 - x$alpha, x$df[1], x$df[2]), qchisq(1 - x$alpha, x$df))
  cat(paste("\nDetection thresholds: ", round(critical, 4), " (significance level: ", x$alpha, ")", sep = ""))
  if (is.character(x$DIFitems)) {
    cat("\nNone of items is detected as DIF")
  }
  else {
    cat("\n\nItems detected as DIF items:")
    cat("\n", paste(colnames(x$Data)[x$DIFitems], "\n", sep = ""))
  }
}

#' @rdname difNLR
#' @export
plot.difNLR <- function(x, plot.type = "cc", item = "all",
                        col = c("dodgerblue2", "goldenrod2"),
                        shape = 21, size = .8,
                        linetype = c(2, 1), title,
                        ...){

  plotstat <- function(x, size = size, title = title){
    if (x$conv.fail != 0){
      if (length(x$conv.fail) == length(x$Sval)){
        switch(x$test, "F" = stop("None of items does converge.
                                  F-statistic values not plotted", call. = FALSE),
                       "LR" = stop("None of items does converge.
                                   LR-statistic values not plotted", call. = FALSE))
      } else {
        switch(x$test, "F" = warning(paste("Item ", x$conv.fail.which,
                                           " does not converge. F-statistic value not plotted",
                                           sep = "", collapse = "\n"), call. = FALSE),
                       "LR" = warning(paste("Item ", x$conv.fail.which,
                                            " does not converge. LR-statistic value not plotted",
                                           sep = "", collapse = "\n"), call. = FALSE))
      }
    }

    if(missing(title)){
      title <- "Non-linear regression DIF detection with none multiple comparison correction"
    }
    n <- nrow(x$Data)
    Sval_critical <- switch(x$test,
                            "F" = qf(1 - x$alpha, x$df[1], x$df[2]),
                            "LR" = qchisq(1 - x$alpha, x$df))
    g <- as.factor(ifelse(x$Sval > Sval_critical, 1, 0))
    items <- setdiff(1:length(x$Sval), x$conv.fail.which)
    hv <- na.omit(as.data.frame(cbind(1:length(x$Sval), x$Sval, g)))
    plot_stat <- ggplot(hv, aes_string(x = "V1", y = "V2",
                                       label = as.character("V1"),
                                       col = as.factor(g))) +
                  ### points
                  geom_text() +
                  scale_color_manual(values = c("black", "red")) +
                  ### critical value
                  geom_hline(yintercept = Sval_critical, size = size) +
                  ### theme
                  ggtitle(title) +
                  labs(x = "Item", y = switch(x$test,
                                              "F" = "F-statistic",
                                              "LR" = "Chisq-statistic")) +
                  theme_bw() +
                  theme(plot.title = element_text(face = "bold", vjust = 1.5),
                        axis.line  = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.background = element_rect(fill = "transparent", colour = NA),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        legend.position = "none")

    return(plot_stat)
  }

  plotCC <- function(x, item = item,
                     col = col, shape = shape, size = size,
                     linetype = linetype, title = title){
    m <- nrow(x$nlrPAR)
    if (class(item) == "character"){
      if (item != "all")
        stop("'item' must be either numeric vector or character string 'all' ",
             call. = FALSE)
    } else {
      if (class(item) != "integer" & class(item) != "numeric")
        stop("'item' must be either numeric vector or character string 'all' ",
             call. = FALSE)
    }
    if (class(item) == "numeric" & !all(item %in% 1:m))
      stop("invalid number of 'item'",
           call. = FALSE)
    if (class(item) == "integer" & !all(item %in% 1:m))
      stop("'item' must be either numeric vector or character string 'all' ",
           call. = FALSE)
    if (length(col) == 1){
      col <- rep(col, 2)
    } else {
      if (length(col) > 2){
        col <- col[1:2]
        warning("Length of 'col' is greater than 2. Only first two values are used",
                call. = FALSE)
      }
    }
    if (class(item) == "character"){
      items <- 1:m
    } else {
      items <- item
    }
    if (any(x$conv.fail.which %in% items)){
      if (length(setdiff(items, x$conv.fail.which)) == 0){
        stop(paste("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted",
                   sep = "", collapse = "\n"),
             call. = FALSE)
      } else {
        warning(paste("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted",
                      sep = "", collapse = "\n"),
                call. = FALSE)
        items <- setdiff(items, x$conv.fail.which)
      }
    }
    if (length(linetype) != 2){
      if (length(linetype) == 1){
        linetype <- rep(linetype, 2)
      } else {
        linetype <- linetype[1:2]
        warning("Length of 'linetype' is greater than 2. Only first two values are used",
                call. = FALSE)
      }
    }


    ### functions
    gNLR <- deriv3( ~ (c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) /
                      (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))),
                    namevec = c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif"),
                    function.arg = function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){})

    ### Data
    if (length(x$match) > 1){
      xlab <- "Matching criterion"
      xR <- x$match[x$group == 0]
      xF <- x$match[x$group == 1]
    } else {
      if (x$match == "score"){
        xlab <- "Total score"
        xR <- c(apply(x$Data[x$group == 0, ], 1, sum))
        xF <- c(apply(x$Data[x$group == 1, ], 1, sum))
      } else {
        xlab <- "Standardized total score"
        xR <- c(scale(apply(x$Data[x$group == 0, ], 1, sum)))
        xF <- c(scale(apply(x$Data[x$group == 1, ], 1, sum)))
      }
    }

    max_sts <- max(as.numeric(levels(as.factor(xR))),
                   as.numeric(levels(as.factor(xF))))
    min_sts <- min(as.numeric(levels(as.factor(xR))),
                   as.numeric(levels(as.factor(xF))))
    alpha <- 0.5
    plot_CC <- list()

    for (i in items){
      hv_R <- data.frame(cbind(as.numeric(levels(as.factor(xR))),
                               tapply(x$Data[x$group == 0, i],
                                      as.factor(xR), mean)))
      hv_F <- data.frame(cbind(as.numeric(levels(as.factor(xF))),
                               tapply(x$Data[x$group == 1, i],
                                      as.factor(xF), mean)))
      hv   <- data.frame(rbind(cbind(hv_R, Group = "Reference"),
                               cbind(hv_F, Group = "Focal")))
      rownames(hv) <- 1:dim(hv)[1]
      hv$size <- c(table(xR), table(xF))

      if (!missing(title)){
        TITLE <- title
      } else {
        TITLE <- colnames(x$Data)[i]
      }

      if (dim(x$nlrPAR)[2] != 8){
        PAR <- data.frame(a = rep(1, m), b = 0, c = 0, d = 1,
                          aDif = 0, bDif = 0, cDif = 0, dDif = 0)
        PAR[, colnames(x$nlrPAR)] <- x$nlrPAR
      } else {
        PAR <- x$nlrPAR
      }

      plot_CC[[i]] <- ggplot(hv, aes_string("X1", "X2")) +
                      ### points
                      geom_point(aes_string(colour = "Group",
                                            fill = "Group",
                                            size = "size"),
                                 alpha = alpha, shape = shape) +
                      ### lines
                      stat_function(aes(colour = "Reference",
                                        linetype = "Reference"),
                                    fun = gNLR,
                                    args = list(g = 0,
                                                a = PAR[i, "a"], b = PAR[i, "b"],
                                                c = PAR[i, "c"], d = PAR[i, "d"],
                                                aDif = PAR[i, "aDif"], bDif = PAR[i, "bDif"],
                                                cDif = PAR[i, "cDif"], dDif = PAR[i, "dDif"]),
                                    size = size,
                                    geom = "line") +
                      stat_function(aes(colour = "Focal", linetype = "Focal"),
                                    fun = gNLR,
                                    args = list(g = 1,
                                                a = PAR[i, "a"], b = PAR[i, "b"],
                                                c = PAR[i, "c"], d = PAR[i, "d"],
                                                aDif = PAR[i, "aDif"], bDif = PAR[i, "bDif"],
                                                cDif = PAR[i, "cDif"], dDif = PAR[i, "dDif"]),
                                    size = size,
                                    geom = "line") +
                      ### style
                      scale_size_continuous(name = "Counts")  +
                      scale_colour_manual(breaks = hv$Group, values = col, name = "Group") +
                      scale_fill_manual(breaks = hv$Group, values = col, name = "Group") +
                      scale_linetype_manual(breaks = hv$Group, values = linetype, name = "Group") +
                      guides(colour = guide_legend(title = "Group", order = 1)) +
                      guides(fill = guide_legend(title = "Group", order = 1)) +
                      guides(linetype = guide_legend(title = "Group", order = 1)) +
                      guides(size = guide_legend(title = "Counts", order = 2)) +
                      ### theme
                      ggtitle(TITLE) +
                      labs(x = xlab, y = "Probability of correct answer") +
                      scale_y_continuous(limits = c(0, 1)) +
                      theme_bw() +
                      theme(plot.title = element_text(face = "bold", vjust = 1.5),
                            axis.line  = element_line(colour = "black"),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            plot.background = element_rect(fill = "transparent", colour = NA)) +
                      ### legend
                      theme(legend.box.just = "top",
                            legend.justification = c(1, 0),
                            legend.position = c(1, 0),
                            legend.box = "horizontal")
    }
    plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
    return(plot_CC)
  }
  ### checking input
  if (!(plot.type %in% c("cc", "stat"))){
    stop("Possible values of 'plot.type' is 'cc' or 'stat'.",
         call. = FALSE)
  } else {
    if (plot.type == "cc"){
      plotCC(x, item = item,
             col = col, shape = shape, size = size,
             linetype = linetype, title = title)
    } else {
      plotstat(x, size = size, title = title)
    }
  }
}

#' @rdname difNLR
#' @export
fitted.difNLR <- function(object, item = "all", ...){

  ### checking input
  m <- nrow(object$nlrPAR)
  if (class(item) == "character"){
    if (item != "all")
      stop("'item' must be either numeric vector or character string 'all' ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("'item' must be either numeric vector or character string 'all' ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("invalid number of 'item'",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("'item' must be either numeric vector or character string 'all' ",
         call. = FALSE)
  if (class(item) == "character"){
    items <- 1:m
  } else {
    items <- item
  }
  if (any(object$conv.fail.which %in% items)){
    if (length(setdiff(items, object$conv.fail.which)) == 0){
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                 sep = "", collapse = "\n"),
           call. = FALSE)
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                    sep = "", collapse = "\n"),
              call. = FALSE)
      items <- setdiff(items, object$conv.fail.which)
    }
  }


  ### functions
  gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){
    return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
  }

  if (dim(object$nlrPAR)[2] != 8){
    PAR <- data.frame(a = rep(1, m), b = 0, c = 0, d = 1, aDif = 0, bDif = 0, cDif = 0, dDif = 0)
    PAR[, colnames(object$nlrPAR)] <- object$nlrPAR
  } else {
    PAR <- object$nlrPAR
  }
  ### data
  if (length(object$match) > 1){
    match <- object$match
  } else {
    if (object$match == "score"){
      match <- c(apply(object$Data, 1, sum))
    } else {
      match <- c(scale(apply(object$Data, 1, sum)))
    }
  }

  ### fitted values
  FV <- lapply(items, function(i) gNLR(match, object$group,
                                       PAR[i, "a"], PAR[i, "b"],
                                       PAR[i, "c"], PAR[i, "d"],
                                       PAR[i, "aDif"], PAR[i, "bDif"],
                                       PAR[i, "cDif"], PAR[i, "dDif"]))
  FV <- lapply(FV, setNames, NULL)
  names(FV) <- paste("Item", items)
  return(FV)

}

#' @rdname difNLR
#' @export
predict.difNLR <- function(object, item = "all",
                           score, group, ...){

  ### checking input
  m <- nrow(object$nlrPAR)
  if (class(item) == "character"){
    if (item != "all")
      stop("'item' must be either numeric vector or character string 'all' ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("'item' must be either numeric vector or character string 'all' ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("invalid number of 'item'",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("'item' must be either numeric vector or character string 'all' ",
         call. = FALSE)
  if (missing(score)){
    if (length(object$match) > 1){
      match <- object$match
    } else {
      if (object$match == "score"){
        match <- c(apply(object$Data, 1, sum))
      } else {
        match <- c(scale(apply(object$Data, 1, sum)))
      }
    }
  } else {
    match <- score
  }
  if (missing(group)){
    group <- object$group
  }
  if(length(score) != length(group)){
    stop("'score' and 'group' must be the same length",
         call. = FALSE)
  }
  if (class(item) == "character"){
    items <- 1:m
  } else {
    items <- item
  }
  if (any(object$conv.fail.which %in% items)){
    if (length(setdiff(items, object$conv.fail.which)) == 0){
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                 sep = "", collapse = "\n"),
           call. = FALSE)
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                    sep = "", collapse = "\n"),
              call. = FALSE)
      items <- setdiff(items, object$conv.fail.which)
    }
  }

  ### functions
  gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){
    return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
  }


  if (dim(object$nlrPAR)[2] != 8){
    PAR <- data.frame(a = rep(1, m), b = 0, c = 0, d = 1, aDif = 0, bDif = 0, cDif = 0, dDif = 0)
    PAR[, colnames(object$nlrPAR)] <- object$nlrPAR
  } else {
    PAR <- object$nlrPAR
  }


  ### predicted values
  PV <- lapply(items, function(i) gNLR(match, group, PAR[i, "a"], PAR[i, "b"],
                                      PAR[i, "c"], PAR[i, "d"],
                                      PAR[i, "aDif"], PAR[i, "bDif"],
                                      PAR[i, "cDif"], PAR[i, "dDif"]))
  PV <- lapply(PV, setNames, NULL)
  names(PV) <- paste("Item", items)
  return(predict = PV)
}

#' @rdname difNLR
#' @export
coef.difNLR <- function(object, ...){
  return(object$nlrPAR)
}

#' @rdname difNLR
#' @export
logLik.difNLR <- function(object, ...){
  m <- nrow(object$nlrPAR)
  LL <- ifelse(1:m %in% object$DIFitems, object$llM0, object$llM1)
  return(LL)
}

#' @rdname difNLR
#' @export
AIC.difNLR <- function(object, ...){
  m <- nrow(object$nlrPAR)
  k <- ifelse(1:m %in% object$DIFitems, ncol(object$parM0), ncol(object$parM1))
  AIC <- 2*k - 2*unlist(logLik.difNLR(object))
  return(AIC)
}

#' @rdname difNLR
#' @export
BIC.difNLR <- function(object, ...){
  m <- nrow(object$nlrPAR)
  k <- ifelse(1:m %in% object$DIFitems, ncol(object$parM0), ncol(object$parM1))
  n <- nrow(object$Data)
  BIC <- log(n)*k - 2*unlist(logLik.difNLR(object))
  return(BIC)
}

#' @rdname difNLR
#' @aliases resid.difNLR
#' @export
residuals.difNLR <- function(object, ...){
  m <- nrow(object$nlrPAR)
  residuals <- object$Data - fitted.difNLR(object)
  return(residuals)
}
