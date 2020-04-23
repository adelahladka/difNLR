#' DIF detection among ordinal data.
#'
#' @aliases difORD
#'
#' @description Performs DIF detection procedure for ordinal data based either on adjacent category logit
#' model or on cumulative logit model and likelihood ratio test of a submodel.
#'
#' @param Data data.frame or matrix: dataset which rows represent ordinaly scored examinee answers and
#' columns correspond to the items. In addition, \code{Data} can hold the vector of group membership.
#' @param group numeric or character: a dichotomous vector of the same length as \code{nrow(Data)}
#' or a column identifier of \code{Data}.
#' @param focal.name numeric or character: indicates the level of \code{group} which corresponds to
#' focal group.
#' @param model character: logistic regression model for ordinal data (either \code{"adjacent"} (default)
#' or \code{"cumulative"}). See \strong{Details}.
#' @param type character: type of DIF to be tested. Either \code{"both"} for uniform and non-uniform
#' DIF (i.e., difference in parameters \code{"a"} and \code{"b"}) (default), or \code{"udif"} for
#' uniform DIF only (i.e., difference in difficulty parameter \code{"b"}), or \code{"nudif"} for
#' non-uniform DIF only (i.e., difference in discrimination parameter \code{"a"}). Can be specified
#' as a single value (for all items) or as an item-specific vector.
#' @param match numeric or character: matching criterion to be used as an estimate of trait. Can be
#' either \code{"zscore"} (default, standardized total score), \code{"score"} (total test score),
#' or vector of the same length as number of observations in \code{Data}.
#' @param anchor numeric or character: specification of DIF free items. Either \code{NULL} (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number) determining which items are currently considered as anchor (DIF free) items.
#' Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' @param purify logical: should the item purification be applied? (default is \code{FALSE}).
#' @param nrIter numeric: the maximal number of iterations in the item purification (default is 10).
#' @param p.adjust.method character: method for multiple comparison correction. Possible values are
#' \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"},
#' \code{"fdr"}, and \code{"none"} (default). For more details see \code{\link[stats]{p.adjust}}.
#' @param parametrization character: parametrization of regression coefficients. Possible options are
#' \code{"irt"} for difficulty-discrimination parametrization (default) and \code{"classic"} for
#' intercept-slope parametrization. See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#'
#' @usage
#' difORD(Data, group, focal.name, model = "adjacent", type = "both", match = "zscore",
#'        anchor = NULL, purify = FALSE, nrIter = 10, p.adjust.method = "none",
#'        parametrization = "irt", alpha = 0.05)
#'
#' @details
#' Performs DIF detection procedure for ordinal data based either on adjacent category logit model
#' or on cumulative logit model.
#'
#' Using adjacent category logit model, logarithm of ratio of probabilities of two adjacent
#' categories is
#' \deqn{log(P(y = k)/P(y = k-1)) = (a + aDif*g)*(x - b_k - b_kDif*g),}
#' where \eqn{x} is by default standardized total score (also called Z-score) and \eqn{g} is a group
#' membership. Parameter \eqn{a} is a discrimination of the item and parameter \eqn{b_k} is difficulty
#' for the \eqn{k}-th category of the item. Terms \eqn{a_Dif} and \eqn{b_kDif} then represent differences
#' between two groups (reference and focal) in relevant parameters.
#'
#' Using cumulative logit model, probability of gaining at least \eqn{k} points is given by
#' 2PL model, i.e.,
#' \deqn{P(y >= k) = exp((a + aDif*g)*(x - b_k - b_kDif*g))/(1 + exp((a + aDif*g)*(x - b_k - b_kDif*g))).}
#' The category probability (i.e., probability of gaining exactly \eqn{k} points) is then
#' \eqn{P(Y = k) = P(Y >= k) - P(Y >= k + 1)}.
#'
#' Both models are estimated by iteratively reweighted least squares. For more details see \code{\link[VGAM]{vglm}}.
#'
#' Argument \code{parametrization} is a character which specifies parametrization of regression parameters.
#' Default option is \code{"irt"} which returns IRT parametrization (difficulty-discrimination, see above).
#' Option \code{"classic"} returns intercept-slope parametrization with effect of group membership and
#' interaction with matching criterion, i.e. \eqn{b_0k + b_1*x + b_2k*g + b_3*x*g} instead of
#' \eqn{(a + a_Dif*g)*(x - b_k - b_kDif*g))}.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as \code{NA}
#' for both, \code{Data} and \code{group} parameters.
#'
#' @return The \code{difORD()} function returns an object of class \code{"difORD"}. The output
#' including values of the test statistics, p-values, and items marked as DIF is displayed by the
#' \code{print()} method.
#'
#' A list of class \code{"difORD"} with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{ordPAR}}{the estimates of the final model.}
#'   \item{\code{ordSE}}{standard errors of the estimates of the final model.}
#'   \item{\code{parM0}}{the estimates of null model.}
#'   \item{\code{parM1}}{the estimates of alternative model.}
#'   \item{\code{llM0}}{log-likelihood of null model.}
#'   \item{\code{llM1}}{log-likelihood of alternative model.}
#'   \item{\code{AICM0}}{AIC of null model.}
#'   \item{\code{AICM1}}{AIC of alternative model.}
#'   \item{\code{BICM0}}{BIC of null model.}
#'   \item{\code{BICM1}}{BIC of alternative model.}
#'   \item{\code{DIFitems}}{either the column identifiers of the items which were detected as DIF, or
#'   \code{"No DIF item detected"} in case no item was detected as DIF.}
#'   \item{\code{model}}{model used for DIF detection.}
#'   \item{\code{type}}{character: type of DIF that was tested.}
#'   \item{\code{parametrization}}{Parameters' parametrization.}
#'   \item{\code{purification}}{\code{purify} value.}
#'   \item{\code{nrPur}}{number of iterations in item purification process. Returned only if \code{purify}
#'   is \code{TRUE}.}
#'   \item{\code{difPur}}{a binary matrix with one row per iteration of item purification and one column per item.
#'   \code{"1"} in i-th row and j-th column means that j-th item was identified as DIF in i-th iteration. Returned only
#'   if \code{purify} is \code{TRUE}.}
#'   \item{\code{conv.puri}}{logical indicating whether item purification process converged before the maximal number
#'   \code{nrIter} of iterations. Returned only if \code{purify} is \code{TRUE}.}
#'   \item{\code{p.adjust.method}}{character: method for multiple comparison correction which was applied.}
#'   \item{\code{pval}}{the p-values by likelihood ratio test.}
#'   \item{\code{adj.pval}}{the adjusted p-values by likelihood ratio test using \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of likelihood ratio test.}
#'   \item{\code{alpha}}{numeric: significance level.}
#'   \item{\code{Data}}{the data matrix.}
#'   \item{\code{group}}{the vector of group membership.}
#'   \item{\code{group.names}}{levels of grouping variable.}
#'   \item{\code{match}}{matching criterion.}
#'   }
#'
#' For an object of class \code{"difORD"} several methods are available (e.g. \code{methods(class = "difORD")}).
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
#' Agresti, A. (2010). Analysis of ordinal categorical data. Second edition. John Wiley & Sons.
#'
#' @seealso
#' \code{\link[difNLR]{plot.difORD}} for graphical representation of item characteristic curves. \cr
#' \code{\link[difNLR]{coef.difORD}} for extraction of item parameters with their standard errors. \cr
#' \code{\link[difNLR]{logLik.difORD}}, \code{\link[difNLR]{AIC.difORD}}, \code{\link[difNLR]{BIC.difORD}}
#' for extraction of loglikelihood and information criteria. \cr
#'
#' \code{\link[stats]{p.adjust}} for multiple comparison corrections. \cr
#' \code{\link[VGAM]{vglm}} for estimation function using iteratively reweighted least squares.
#'
#'
#' @examples
#' # Loading data
#' data(dataMedicalgraded, package = "ShinyItemAnalysis")
#' Data <- dataMedicalgraded[, 1:5]
#' group <- dataMedicalgraded[, 101]
#'
#' # Testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
#'
#' \dontrun{
#' # Graphical devices
#' plot(x, item = 3)
#' plot(x, item = "X2003")
#' plot(x, item = "X2003", group.names = c("Group 1", "Group 2"))
#'
#' # Estimated parameters
#' coef(x)
#' coef(x, SE = TRUE) # with SE
#' coef(x, SE = TRUE, simplify = TRUE) # with SE, simplified
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
#' # Testing both DIF effects with Benjamini-Hochberg adjustment method
#' difORD(Data, group, focal.name = 1, model = "adjacent", p.adjust.method = "BH")
#'
#' # Testing both DIF effects with item purification
#' difORD(Data, group, focal.name = 1, model = "adjacent", purify = TRUE)
#'
#' # Testing uniform DIF effects
#' difORD(Data, group, focal.name = 1, model = "adjacent", type = "udif")
#' # Testing non-uniform DIF effects
#' difORD(Data, group, focal.name = 1, model = "adjacent", type = "nudif")
#'
#' # Testing both DIF effects with total score as matching criterion
#' difORD(Data, group, focal.name = 1, model = "adjacent", match = "score")
#'
#' # Testing both DIF effects with cumulative logit model
#' # using IRT parametrization
#' (x <- difORD(Data, group, focal.name = 1, model = "cumulative", parametrization = "irt"))
#'
#' # Graphical devices
#' plot(x, item = 3, plot.type = "cumulative")
#' plot(x, item = 3, plot.type = "category")
#'
#' # Estimated parameters in IRT parametrization
#' coef(x, simplify = TRUE)
#' }
#' @keywords DIF
#' @export
difORD <- function(Data, group, focal.name, model = "adjacent", type = "both", match = "zscore",
                   anchor = NULL, purify = FALSE, nrIter = 10, p.adjust.method = "none",
                   parametrization = "irt", alpha = 0.05) {
  if (!type %in% c("udif", "nudif", "both") | !is.character(type)) {
    stop("'type' must be either 'udif', 'nudif', or 'both'.",
      call. = FALSE
    )
  }
  if (!model %in% c("adjacent", "cumulative") | !is.character(model)) {
    stop("'model' must be either 'adjacent' or 'cumulative'.",
      call. = FALSE
    )
  }
  if (alpha > 1 | alpha < 0) {
    stop("'alpha' must be between 0 and 1.",
      call. = FALSE
    )
  }
  if (!parametrization %in% c("classic", "irt")) {
    stop("Invalid value for 'parametrization'. Possible values are 'classic' and 'irt'.",
      call. = FALSE
    )
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
           of observations in 'Data'.", call. = FALSE)
    }
  }
  ### purification
  if (purify & !(match[1] %in% c("score", "zscore"))) {
    stop("Purification not allowed when matching variable is not 'zscore' or 'score'.", call. = FALSE)
  }

  internalORD <- function() {
    if (length(group) == 1) {
      if (is.numeric(group)) {
        GROUP <- Data[, group]
        DATA <- Data[, (1:dim(Data)[2]) != group]
        colnames(DATA) <- colnames(Data)[(1:dim(Data)[2]) != group]
      } else {
        GROUP <- Data[, colnames(Data) == group]
        DATA <- Data[, colnames(Data) != group]
        colnames(DATA) <- colnames(Data)[colnames(Data) != group]
      }
    } else {
      GROUP <- group
      DATA <- Data
    }
    if (length(levels(as.factor(GROUP))) != 2) {
      stop("'group' must be binary vector", call. = FALSE)
    }
    if (is.matrix(DATA) | is.data.frame(DATA)) {
      if (dim(DATA)[1] != length(GROUP)) {
        stop("'Data' must have the same number of rows as is length of vector 'group'.",
          call. = FALSE
        )
      }
    } else {
      stop("'Data' must be data frame or matrix of ordinal vectors.",
        call. = FALSE
      )
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
      stop("It seems that your 'Data' does not include any subjects that are complete.",
        call. = FALSE
      )
    }

    GROUP <- df[, "GROUP"]
    DATA <- data.frame(df[, !(colnames(df) %in% c("GROUP", "match"))])

    if (length(match) > 1) {
      match <- df[, "match"]
    }

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
    if (!purify | !(match[1] %in% c("zscore", "score")) | !is.null(anchor)) {
      PROV <- suppressWarnings(ORD(DATA, GROUP,
        model = model, match = match, anchor = ANCHOR,
        type = type, p.adjust.method = p.adjust.method, parametrization = parametrization,
        alpha = alpha
      ))

      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      se.m1 <- PROV$se.m1
      se.m0 <- PROV$se.m0
      significant <- which(ADJ.PVAL < alpha)

      if (length(significant) > 0) {
        DIFitems <- significant
        ordPAR <- PROV$par.m0
        ordSE <- se.m0
        for (idif in 1:length(DIFitems)) {
          ordPAR[[DIFitems[idif]]] <- PROV$par.m1[[DIFitems[idif]]]
          ordSE[[DIFitems[idif]]] <- se.m1[[DIFitems[idif]]]
        }
      } else {
        DIFitems <- "No DIF item detected"
        ordPAR <- PROV$par.m0
        ordSE <- se.m0
      }

      RES <- list(
        Sval = STATS,
        ordPAR = ordPAR,
        ordSE = ordSE,
        parM0 = PROV$par.m0,
        parM1 = PROV$par.m1,
        llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
        AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
        BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
        DIFitems = DIFitems,
        model = model,
        type = type,
        parametrization = parametrization,
        purification = purify,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval,
        df = PROV$df,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, match = match
      )
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(ORD(DATA, GROUP,
        model = model, type = type, match = match,
        p.adjust.method = p.adjust.method, parametrization = parametrization,
        alpha = alpha
      ))
      stats1 <- prov1$Sval
      pval1 <- prov1$pval
      significant1 <- which(pval1 < alpha)
      if (length(significant1) == 0) {
        PROV <- prov1
        STATS <- stats1
        DIFitems <- "No DIF item detected"
        se.m1 <- PROV$se.m1
        se.m0 <- PROV$se.m0
        ordPAR <- PROV$par.m0
        ordSE <- se.m0
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
            prov2 <- suppressWarnings(ORD(DATA, GROUP,
              model = model, anchor = nodif, type = type, match = match,
              p.adjust.method = p.adjust.method, parametrization = parametrization,
              alpha = alpha
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
        se.m1 <- PROV$se.m1
        se.m0 <- PROV$se.m0
        ordPAR <- PROV$par.m0
        ordSE <- se.m0
        if (length(significant1) > 0) {
          DIFitems <- significant1
          for (idif in 1:length(DIFitems)) {
            ordPAR[[DIFitems[idif]]] <- PROV$par.m1[[DIFitems[idif]]]
            ordSE[[DIFitems[idif]]] <- se.m1[[DIFitems[idif]]]
          }
        } else {
          DIFitems <- "No DIF item detected"
        }
      }
      if (!is.null(difPur)) {
        rownames(difPur) <- paste0("Step", 0:(dim(difPur)[1] - 1))
        colnames(difPur) <- colnames(DATA)
      }
      RES <- list(
        Sval = STATS,
        ordPAR = ordPAR,
        ordSE = ordSE,
        parM0 = PROV$par.m0,
        parM1 = PROV$par.m1,
        llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
        AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
        BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
        DIFitems = DIFitems,
        model = model,
        type = type,
        parametrization = parametrization,
        purification = purify, nrPur = nrPur, difPur = difPur, conv.puri = noLoop,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval,
        df = PROV$df,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, match = match
      )
    }
    class(RES) <- "difORD"
    return(RES)
  }
  resToReturn <- internalORD()
  return(resToReturn)
}

#' @export
print.difORD <- function(x, ...) {
  title <- paste0(
    "Detection of ",
    switch(x$type,
      both = "both types of ",
      udif = "uniform ",
      nudif = "non-uniform"
    ),
    "Differential Item Functioning for ordinal data using ",
    ifelse(x$model == "adjacent", "adjacent category", "cumulative"),
    " logit regression model "
  )
  cat(paste(strwrap(title, width = 60), collapse = "\n"))
  cat("\n\nLikelihood-ratio Chi-square statistics\n")
  if (x$purification) word.iteration <- ifelse(x$nrPur <= 1, " iteration", " iterations")
  cat(paste("\nItem purification was", ifelse(x$purification, " ", " not "), "applied",
    ifelse(x$purification, paste0(" with ", x$nrPur, word.iteration), ""), "\n",
    sep = ""
  ))
  if (x$purification) {
    if (!x$conv.puri) {
      cat(paste("WARNING: Item purification process not converged after "), x$nrPur, word.iteration, "\n",
        "         Results are based on last iteration of the item purification. \n",
        sep = ""
      )
    }
  }
  if (x$p.adjust.method == "none") {
    cat("No p-value adjustment for multiple comparisons\n\n")
  }
  else {
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
    colnames(tab) <- c("Chisq-value", "P-value", "")
  } else {
    tab <- format(round(cbind(x$Sval, x$pval, x$adj.pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 4)
    colnames(tab) <- c("Chisq-value", "P-value", "Adj. P-value", "")
  }

  rownames(tab) <- colnames(x$Data)

  print(tab, quote = F, digits = 4, zero.print = F)
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  if (is.character(x$DIFitems)) {
    switch(x$type,
      both = cat("\nNone of items is detected as DIF"),
      udif = cat("\nNone of items is detected as uniform DIF"),
      nudif = cat("\nNone of items is detected as non-uniform DIF")
    )
  } else {
    switch(x$type,
      both = cat("\nItems detected as DIF items:"),
      udif = cat("\nItems detected as uniform DIF items:"),
      nudif = cat("\nItems detected as non-uniform DIF items:")
    )
    cat("\n", paste(colnames(x$Data)[x$DIFitems], "\n", sep = ""))
  }
}

#' Extract model coefficients from an object of \code{"difORD"} class.
#'
#' @description S3 method for extracting estimated model coefficients from an object of \code{"difORD"} class.
#' @aliases coefficients.difORD
#'
#' @param object an object of \code{"difORD"} class.
#' @param SE logical: should the standard errors of estimated parameters be also returned? (default is \code{FALSE}).
#' @param simplify logical: should the estimated parameters be simplified to a matrix? (default is \code{FALSE}).
#' @param ... other generic parameters for \code{coef()} method.
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
#' @seealso
#' \code{\link[difNLR]{difORD}} for DIF detection among ordinal data. \cr
#' \code{\link[stats]{coef}} for generic function extracting model coefficients.
#'
#' @examples
#' \dontrun{
#' # Loading data
#' data(dataMedicalgraded, package = "ShinyItemAnalysis")
#' Data <- dataMedicalgraded[, 1:5]
#' group <- dataMedicalgraded[, 101]
#'
#' # Testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
#'
#' # Estimated parameters
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, simplify = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
#' }
#' @export
coef.difORD <- function(object, SE = FALSE, simplify = FALSE, ...) {
  if (class(SE) != "logical") {
    stop("Invalid value for 'SE'. 'SE' need to be logical.",
      call. = FALSE
    )
  }
  if (class(simplify) != "logical") {
    stop("Invalid value for 'simplify'. 'simplify' need to be logical.",
      call. = FALSE
    )
  }

  m <- dim(object$Data)[2]
  nams <- colnames(object$Data)

  coefs <- object$ordPAR
  names(coefs) <- nams

  if (SE) {
    se <- object$ordSE
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

    cat.est <- sort(unique(unlist(lapply(object$Data, function(x) sort(unique(x))[-1]))))
    if (object$parametrization == "irt") {
      res <- res[, c(paste0("b", cat.est), "a", paste0("bDIF", cat.est), "aDIF")]
    } else {
      res <- res[, c(paste0("(Intercept):", cat.est), "x", "group", "x:group")[1:ncol(res)]]
    }
  } else {
    res <- coefs
  }

  return(res)
}

#' Loglikelihood and information criteria for an object of \code{"difORD"} class.
#'
#' @aliases AIC.difORD BIC.difORD
#' @rdname logLik.difORD
#'
#' @description S3 methods for extracting loglikelihood, Akaike's information criterion (AIC) and
#' Schwarz's Bayesian criterion (BIC) for an object of \code{"difORD"} class.
#'
#' @param object an object of \code{"difORD"} class.
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
#' @seealso
#' \code{\link[difNLR]{difORD}} for DIF detection among ordinal data. \cr
#' \code{\link[stats]{logLik}} for generic function extracting loglikelihood. \cr
#' \code{\link[stats]{AIC}} for generic function calculating AIC and BIC.
#'
#' @examples
#' \dontrun{
#' # Loading data
#' data(dataMedicalgraded, package = "ShinyItemAnalysis")
#' Data <- dataMedicalgraded[, 1:5]
#' group <- dataMedicalgraded[, 101]
#'
#' # Testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
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
logLik.difORD <- function(object, item = "all", ...) {
  m <- length(object$ordPAR)
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

  val <- ifelse(items %in% object$DIFitems,
    object$llM1[items],
    object$llM0[items]
  )
  df <- ifelse(items %in% object$DIFitems,
    sapply(object$parM1, length)[items],
    sapply(object$parM0, length)[items]
  )
  if (length(items) == 1) {
    attr(val, "df") <- df
    class(val) <- "logLik"
  }
  return(val)
}

#' @rdname logLik.difORD
#' @aliases BIC.difORD logLik.difORD
#' @export
AIC.difORD <- function(object, item = "all", ...) {
  m <- length(object$ordPAR)
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

  AIC <- ifelse(items %in% object$DIFitems, object$AICM1[items], object$AICM0[items])
  return(AIC)
}

#' @rdname logLik.difORD
#' @aliases AIC.difORD logLik.difORD
#' @export
BIC.difORD <- function(object, item = "all", ...) {
  m <- length(object$ordPAR)
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

  BIC <- ifelse(items %in% object$DIFitems, object$BICM1[items], object$BICM0[items])
  return(BIC)
}

#' ICC plots for an object of \code{"difORD"} class.
#'
#' @description Plot method for an object of \code{"difORD"} class using \pkg{ggplot2}.
#'
#' The characteristic curves (category probabilities) for an item specified in \code{item}
#' argument are plotted. Plotted curves represent the best model. For cumulative logit model,
#' also cumulative probabilities may be plotted.
#'
#' @param x an object of \code{"difORD"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all converged items (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number).
#' @param plot.type character: which plot should be displayed for cumulative logit regression model. Either
#' \code{"category"} (default) for category probabilities or \code{"cumulative"} for cumulative probabilities.
#' @param title string: title of a plot.
#' @param group.names character: names of reference and focal group.
#' @param ... other generic parameters for \code{plot()} function.
#'
#' @return Returns list of objects of class \code{"ggplot"}.
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
#' @seealso
#' \code{\link[difNLR]{difORD}} for DIF detection among ordinal data. \cr
#' \code{\link[ggplot2]{ggplot}} for general function to plot a \code{"ggplot"} object.
#'
#' @examples
#' \dontrun{
#' # Loading data
#' data(dataMedicalgraded, package = "ShinyItemAnalysis")
#' Data <- dataMedicalgraded[, 1:5]
#' group <- dataMedicalgraded[, 101]
#'
#' # Testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
#'
#' # Graphical devices
#' plot(x, item = 3)
#' plot(x, item = "X2003", group.names = c("Group 1", "Group 2"))
#'
#' # Testing both DIF effects with cumulative logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "cumulative"))
#' plot(x, item = 3, plot.type = "cumulative", title = "Cumulative probabilities")
#' plot(x, item = 3, plot.type = "category", title = "Category probabilities")
#' }
#' @export
plot.difORD <- function(x, item = "all", title, plot.type, group.names, ...) {
  m <- length(x$ordPAR)
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

  if (missing(plot.type)) {
    if (x$model == "cumulative") {
      plot.type <- "category"
    } else {
      plot.type <- NULL
    }
  }

  if (x$model == "adjacent" & !is.null(plot.type)) {
    warning("Argument 'plot.type' is ignored for adjacent category logit model. ")
  }
  if (!is.null(plot.type)) {
    if (!plot.type %in% c("category", "cumulative")) {
      stop("'plot.type' can be either 'category' or 'cumulative'.  ")
    }
  }

  if (missing(group.names)) {
    group.names <- x$group.names
    if (all(group.names %in% c(0, 1))) group.names <- c("Reference", "Focal")
  }
  if (length(group.names) > 2) {
    group.names <- group.names[1:2]
    warning("Only first two values for 'group.names' argument are used. ")
  } else {
    if (length(group.names) < 2) {
      group.names <- c("Reference", "Focal")
      warning("Argument 'group.names' need to have length of two. Default value is used.")
    }
  }

  if (x$purification) {
    anchor <- c(1:m)[!c(1:m) %in% x$DIFitems]
  } else {
    anchor <- 1:m
  }

  if (x$match[1] == "zscore") {
    Data <- sapply(1:m, function(i) as.numeric(paste(x$Data[, i])))
    matching <- c(unlist(scale(rowSums(as.data.frame(Data[, anchor])))))
    xlab <- "Standardized total score"
  } else {
    if (x$match[1] == "score") {
      Data <- sapply(1:m, function(i) as.numeric(paste(x$Data[, i])))
      matching <- rowSums(as.data.frame(Data[, anchor]))
      xlab <- "Total score"
    } else {
      if (length(x$match) == dim(x$Data)[1]) {
        matching <- x$match
        xlab <- "Matching criterion"
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  match <- seq(min(matching, na.rm = T), max(matching, na.rm = T), length.out = 300)

  mat0 <- switch(x$type,
    "both"  = cbind("intercept" = 1, "match" = match, "group" = 0, "x:match" = 0),
    "udif"  = cbind("intercept" = 1, "match" = match, "group" = 0, "x:match" = 0),
    "nudif" = cbind("intercept" = 1, "match" = match, "group" = 1, "x:match" = 0)
  )
  mat1 <- switch(x$type,
    "both"  = cbind("intercept" = 1, "match" = match, "group" = 1, "x:match" = match),
    "udif"  = cbind("intercept" = 1, "match" = match, "group" = 1, "x:match" = 0),
    "nudif" = cbind("intercept" = 1, "match" = match, "group" = 1, "x:match" = match)
  )

  I <- length(items)
  plot_CC <- as.list(rep(NA, I))
  if (x$model == "adjacent") {
    for (k in 1:I) {
      i <- items[k]
      if (!missing(title)) {
        TITLE <- title
      } else {
        TITLE <- colnames(x$Data)[i]
      }

      cat <- unique(sort(x$Data[, i]))
      num.cat <- length(cat)
      num.cat.est <- num.cat - 1
      cat.est <- cat[-1]

      coefs <- x$ordPAR[[i]]
      if (x$parametrization == "classic") {
        coefs <- c(coefs, rep(0, num.cat.est + 3 - length(coefs)))
      } else {
        a <- coefs["a"]
        b <- coefs[paste0("b", cat.est)]
        aDIF <- coefs["aDIF"]
        bDIF <- coefs[paste0("bDIF", cat.est)]
        coefs <- c(-a * b, a, unique(round(-a * bDIF - aDIF * b - aDIF * bDIF, 7)), aDIF)
      }
      coefs <- sapply(1:num.cat.est, function(j) coefs[c(j, (num.cat.est + 1):length(coefs))])

      # calculation probabilities on formula exp(\sum_{t = 0}^{k} b_{0t} + b1X)/(\sum_{r = 0}^{K}exp(\sum_{t=0}^{r}b_{0t} + b1X))
      df.probs.cat <- matrix(0,
        nrow = 2 * length(match), ncol = num.cat,
        dimnames = list(NULL, cat)
      )
      df.probs.cat[, as.character(cat.est)] <- rbind(
        mat0 %*% coefs,
        mat1 %*% coefs
      )
      # cumulative sum
      df.probs.cat <- t(apply(df.probs.cat, 1, cumsum))
      # exponential
      df.probs.cat <- exp(df.probs.cat)
      # norming
      df.probs.cat <- df.probs.cat / rowSums(df.probs.cat)
      summary(df.probs.cat)

      # melting data
      df.probs.cat <- data.frame(match, group = rep(c(0, 1), each = length(match)), df.probs.cat)
      colnames(df.probs.cat) <- c("matching", "group", paste0("P(Y = ", cat, ")"))
      df.probs.cat <- reshape2::melt(df.probs.cat,
        id.vars = c("matching", "group"),
        variable.name = "category", value.name = "probability"
      )
      df.probs.cat$group <- as.factor(df.probs.cat$group)

      # empirical category values
      df.emp.cat0 <- data.frame(table(matching[x$group == 0], x$Data[x$group == 0, i]),
        prop.table(table(matching[x$group == 0], x$Data[x$group == 0, i]), 1),
        group = 0
      )[, c(1, 2, 3, 6, 7)]
      df.emp.cat1 <- data.frame(table(matching[x$group == 1], x$Data[x$group == 1, i]),
        prop.table(table(matching[x$group == 1], x$Data[x$group == 1, i]), 1),
        group = 1
      )[, c(1, 2, 3, 6, 7)]
      df.emp.cat <- rbind(df.emp.cat0, df.emp.cat1)
      colnames(df.emp.cat) <- c("matching", "category", "size", "probability", "group")

      df.emp.cat$matching <- as.numeric(paste(df.emp.cat$matching))
      df.emp.cat$category <- as.factor(df.emp.cat$category)
      df.emp.cat$group <- as.factor(df.emp.cat$group)
      levels(df.emp.cat$category) <- paste0("P(Y = ", levels(df.emp.cat$category), ")")

      cbPalette <- c("#ffbe33", "#34a4e5", "#ce7eaa", "#00805e", "#737373", "#f4eb71", "#0072B2", "#D55E00")
      num.col <- ceiling(length(levels(df.probs.cat$category)) / 8)
      cols <- rep(cbPalette, num.col)[1:length(levels(df.probs.cat$category))]

      plot_CC[[k]] <- ggplot() +
        geom_point(
          data = df.emp.cat,
          aes_string(
            x = "matching", y = "probability", group = "category",
            size = "size", col = "category", fill = "category"
          ),
          shape = 21, alpha = 0.5
        ) +
        geom_line(
          data = df.probs.cat,
          aes_string(
            x = "matching", y = "probability",
            col = "category", linetype = "group"
          ),
          size = 0.8
        ) +
        xlab(xlab) +
        ylab("Category probability") +
        ylim(0, 1) +
        ggtitle(TITLE) +
        scale_linetype_manual(
          breaks = c(0, 1),
          labels = group.names,
          values = c("solid", "dashed")
        ) +
        scale_fill_manual(values = cols) +
        scale_color_manual(values = cols) +
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
          legend.box.margin = margin(3, 3, 3, 3),
          legend.key = element_rect(fill = "white", colour = NA)
        ) +
        guides(
          size = guide_legend(title = "Count", order = 1),
          colour = guide_legend(title = "Score", order = 2),
          fill = guide_legend(title = "Score", order = 2),
          linetype = guide_legend(title = "Group", order = 3)
        )
    }
  } else {
    for (k in 1:I) {
      i <- items[k]
      if (!missing(title)) {
        TITLE <- title
      } else {
        TITLE <- colnames(x$Data)[i]
      }

      cat <- sort(unique(x$Data[, i]))
      num.cat <- length(cat)

      num.cat.est <- num.cat - 1
      cat.est <- cat[-1]

      coefs <- x$ordPAR[[i]]
      if (x$parametrization == "classic") {
        coefs <- c(coefs, rep(0, num.cat.est + 3 - length(coefs)))
      } else {
        a <- coefs["a"]
        b <- coefs[paste0("b", cat.est)]
        aDIF <- coefs["aDIF"]
        bDIF <- coefs[paste0("bDIF", cat.est)]
        coefs <- c(-a * b, a, unique(round(-a * bDIF - aDIF * b - aDIF * bDIF, 7)), aDIF)
      }
      coefs <- sapply(1:num.cat.est, function(j) coefs[c(j, (num.cat.est + 1):length(coefs))])

      # cumulative probabilities
      df.probs.cum <- matrix(1,
        nrow = 2 * length(match), ncol = num.cat,
        dimnames = list(NULL, cat)
      )

      # calculation of cumulative probabilities based on formula P(Y >= k) = exp(b0 + b1*x)/(1 + exp(b0 + b1*x))
      df.probs.cum[, as.character(cat.est)] <- exp(rbind(mat0 %*% coefs, mat1 %*% coefs)) / (1 + exp(rbind(mat0 %*% coefs, mat1 %*% coefs)))

      # if column between non-ones valued columns consist of ones, it has to be changed to value on the left side
      need.correction <- which(sapply(2:num.cat, function(i) (all(df.probs.cum[, i] == 1) & all(df.probs.cum[, i - 1] != 1))))
      df.probs.cum[, need.correction + 1] <- df.probs.cum[, need.correction]

      # category probabilities
      df.probs.cat <- data.frame(
        sapply(1:(num.cat - 1), function(i) df.probs.cum[, i] - df.probs.cum[, i + 1]),
        df.probs.cum[, num.cat]
      )

      # melting data
      df.probs.cum <- data.frame(match, group = rep(c(0, 1), each = length(match)), df.probs.cum)
      colnames(df.probs.cum) <- c("matching", "group", paste0("P(Y >= ", cat, ")"))
      df.probs.cum <- reshape2::melt(df.probs.cum,
        id.vars = c("matching", "group"),
        variable.name = "category", value.name = "probability"
      )
      df.probs.cum$group <- as.factor(df.probs.cum$group)

      df.probs.cat <- data.frame(match, group = rep(c(0, 1), each = length(match)), df.probs.cat)
      colnames(df.probs.cat) <- c("matching", "group", paste0("P(Y = ", cat, ")"))
      df.probs.cat <- reshape2::melt(df.probs.cat,
        id.vars = c("matching", "group"),
        variable.name = "category", value.name = "probability"
      )
      df.probs.cat$group <- as.factor(df.probs.cat$group)

      # empirical category values
      tmp <- table(matching, x$Data[, i], x$group)
      tmp0 <- tmp[, , 1]
      tmp1 <- tmp[, , 2]

      df.emp.cat0 <- data.frame(tmp0,
        prop.table(tmp0, 1),
        group = 0
      )[, c(1, 2, 3, 6, 7)]
      df.emp.cat1 <- data.frame(tmp1,
        prop.table(tmp1, 1),
        group = 1
      )[, c(1, 2, 3, 6, 7)]

      df.emp.cat <- rbind(df.emp.cat0, df.emp.cat1)
      colnames(df.emp.cat) <- c("matching", "category", "size", "probability", "group")

      df.emp.cat$matching <- as.numeric(paste(df.emp.cat$matching))
      df.emp.cat$category <- as.factor(df.emp.cat$category)
      df.emp.cat$group <- as.factor(df.emp.cat$group)
      levels(df.emp.cat$category) <- paste0("P(Y = ", levels(df.emp.cat$category), ")")


      # empirical cumulative values
      df.emp.cum0 <- prop.table(tmp0, 1)
      df.emp.cum0 <- cbind(
        t(apply(tmp0, 1, function(x) sum(x) - cumsum(x) + x)),
        t(apply(prop.table(tmp0, 1), 1, function(x) sum(x) - cumsum(x) + x))
      )
      df.emp.cum0 <- data.frame(matching = as.numeric(rownames(tmp0)), group = 0, df.emp.cum0)

      df.emp.cum1 <- prop.table(tmp1, 1)
      df.emp.cum1 <- cbind(
        t(apply(tmp1, 1, function(x) sum(x) - cumsum(x) + x)),
        t(apply(prop.table(tmp1, 1), 1, function(x) sum(x) - cumsum(x) + x))
      )
      df.emp.cum1 <- data.frame(matching = as.numeric(rownames(tmp1)), group = 1, df.emp.cum1)

      df.emp.cum <- rbind(df.emp.cum0, df.emp.cum1)
      df.emp.cum <- df.emp.cum[complete.cases(df.emp.cum), ]

      df.emp.cum.count <- reshape2::melt(df.emp.cum[, c(1:2, 3:(2 + num.cat))],
        id.vars = c("matching", "group"),
        variable.name = "category", value.name = "size"
      )
      levels(df.emp.cum.count$category) <- paste0("P(Y >= ", cat, ")")

      df.emp.cum.prob <- reshape2::melt(df.emp.cum[, c(1:2, (3 + num.cat):dim(df.emp.cum)[2])],
        id.vars = c("matching", "group"),
        variable.name = "category", value.name = "probability"
      )
      levels(df.emp.cum.prob$category) <- paste0("P(Y >= ", cat, ")")
      df.emp.cum <- merge(df.emp.cum.count, df.emp.cum.prob, by = c("matching", "group", "category"))

      # colours
      cbPalette <- c("#ffbe33", "#34a4e5", "#ce7eaa", "#00805e", "#737373", "#f4eb71", "#0072B2", "#D55E00")
      num.col <- ceiling(num.cat / 8)
      cols <- c("black", rep(cbPalette, num.col)[1:(num.cat - 1)])

      # hues <- seq(15, 375, length = num.cat)
      # cols <- c("black", hcl(h = hues, l = 65, c = 100)[1:(num.cat - 1)])

      if (plot.type == "cumulative") {
        df.emp.cum <- df.emp.cum[df.emp.cum$category != paste0("P(Y >= ", cat[1], ")"), ]
        df.probs.cum <- df.probs.cum[df.probs.cum$category != paste0("P(Y >= ", cat[1], ")"), ]
        cols <- cols[-1]

        df.emp.cum <- df.emp.cum[complete.cases(df.emp.cum), ]
        df.probs.cum <- df.probs.cum[complete.cases(df.probs.cum), ]

        plot_CC[[k]] <- ggplot() +
          geom_point(
            data = df.emp.cum,
            aes_string(
              x = "matching", y = "probability",
              size = "size", colour = "category", fill = "category"
            ),
            shape = 21, alpha = 0.5
          ) +
          geom_line(
            data = df.probs.cum,
            aes_string(
              x = "matching", y = "probability",
              col = "category", linetype = "group"
            ),
            size = 0.8
          ) +
          scale_fill_manual(values = cols) +
          scale_colour_manual(values = cols) +
          xlab(xlab) +
          ylab("Cumulative probability") +
          ggtitle(TITLE) +
          ylim(0, 1) +
          scale_linetype_manual(
            breaks = c(0, 1), labels = group.names,
            values = c("solid", "dashed")
          ) +
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
            legend.justification = c("right", "bottom"),
            legend.position = c(0.98, 0.02),
            legend.box = "horizontal",
            legend.box.margin = margin(3, 3, 3, 3)
          ) +
          guides(
            size = guide_legend(title = "Count", order = 3),
            colour = guide_legend(title = "Score", order = 2),
            fill = guide_legend(title = "Score", order = 2),
            linetype = guide_legend(title = "Group", order = 1)
          )
      } else {
        df.emp.cat <- df.emp.cat[complete.cases(df.emp.cat), ]
        df.probs.cat <- df.probs.cat[complete.cases(df.probs.cat), ]

        plot_CC[[k]] <- ggplot() +
          geom_point(
            data = df.emp.cat,
            aes_string(
              x = "matching", y = "probability",
              size = "size", col = "category", fill = "category"
            ),
            shape = 21, alpha = 0.5
          ) +
          geom_line(
            data = df.probs.cat,
            aes_string(
              x = "matching", y = "probability",
              col = "category", linetype = "group"
            ),
            size = 0.8
          ) +
          scale_fill_manual(values = cols) +
          scale_colour_manual(values = cols) +
          xlab(xlab) +
          ggtitle(TITLE) +
          ylab("Category probability") +
          ylim(0, 1) +
          scale_linetype_manual(
            breaks = c(0, 1), labels = group.names,
            values = c("solid", "dashed")
          ) +
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
          theme(
            legend.box.just = "top",
            legend.justification = c("left", "top"),
            legend.position = c(0.02, 0.98),
            legend.box = "horizontal",
            legend.box.margin = margin(3, 3, 3, 3)
          ) +
          guides(
            size = guide_legend(title = "Count", order = 1),
            colour = guide_legend(title = "Score", order = 2),
            fill = guide_legend(title = "Score", order = 2),
            linetype = guide_legend(title = "Group", order = 3)
          )
      }
    }
  }
  plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
  return(plot_CC)
}
