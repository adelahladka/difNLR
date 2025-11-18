#' DIF detection among ordinal data.
#'
#' @aliases difORD
#'
#' @description Performs DIF detection procedure for ordinal data
#'   based either on adjacent category logit model or on cumulative
#'   logit model and likelihood ratio test of a submodel.
#'
#' @param Data data.frame or matrix: dataset which rows represent
#'   ordinally scored examinee answers and columns correspond to the
#'   items. In addition, \code{Data} can hold the vector of group
#'   membership.
#' @param group numeric or character: a dichotomous vector of the same
#'   length as \code{nrow(Data)} or a column identifier of
#'   \code{Data}.
#' @param focal.name numeric or character: indicates the level of
#'   \code{group} which corresponds to focal group.
#' @param model character: logistic regression model for ordinal data
#'   (either \code{"adjacent"} (default) or \code{"cumulative"}). See
#'   \strong{Details}.
#' @param type character: type of DIF to be tested. Either
#'   \code{"both"} for uniform and non-uniform DIF (default), or
#'   \code{"udif"} for uniform DIF only, or \code{"nudif"} for
#'   non-uniform DIF only. Can be specified as a single value (for all
#'   items) or as an item-specific vector.
#' @param match character or numeric: matching criterion to be used as an
#'   estimate of the trait. It can be either \code{"zscore"} (default;
#'   standardized total score), \code{"score"} (total test score),
#'   \code{"restscore"} (total score without the tested item),
#'   \code{"zrestscore"} (standardized total score without the tested item), a
#'   numeric vector of the same length as a number of observations in the
#'   \code{Data}, or a numeric matrix of the same dimensions as \code{Data}
#'   (each column represents matching criterion for one item).
#' @param anchor character or numeric: specification of DIF-free (anchor) items
#'   used to compute the matching criterion (\code{match}). Can be either
#'   \code{NULL} (default; all items are used for the calculation), or a vector
#'   of item identifiers (integers indicating column numbers or item names in
#'   `Data`) specifying which items are currently considered as anchor items.
#'   This argument is ignored if the \code{match} is not \code{"zscore"},
#'   \code{"score"}, \code{"restscore"}, or \code{"zrestscore"}. For \code{match =
#'   "score"} or \code{match = "zscore"}, the matching criterion is computed
#'   from the items specified in the anchor set. For \code{match = "restscore"} or
#'   \code{match = "zrestscore"}, the same anchor items are used, except that the
#'   item currently under test is excluded from the computation.
#' @param purify logical: should the item purification be applied? (default is
#'   \code{FALSE}). Item purification is not applied when set of anchor items in
#'   \code{anchor} is specified or when \code{match} is not \code{"zscore"},
#'   \code{"score"}, \code{"restscore"}, or \code{"zrestscore"}.
#' @param nrIter numeric: the maximal number of iterations in the item
#'   purification (default is 10).
#' @param p.adjust.method character: method for multiple comparison
#'   correction. Possible values are \code{"holm"}, \code{"hochberg"},
#'   \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"},
#'   \code{"fdr"}, and \code{"none"} (default). For more details see
#'   \code{\link[stats]{p.adjust}}.
#' @param alpha numeric: significance level (default is 0.05).
#' @param parametrization deprecated. Use
#'   \code{\link[difNLR]{coef.difORD}} for different
#'   parameterizations.
#'
#' @usage
#' difORD(Data, group, focal.name, model = "adjacent", type = "both", match = "zscore",
#'        anchor = NULL, purify = FALSE, nrIter = 10, p.adjust.method = "none",
#'        alpha = 0.05, parametrization)
#'
#' @details
#' Calculates DIF likelihood ratio statistics based either on adjacent
#' category logit model or on cumulative logit model for ordinal data.
#'
#' Using adjacent category logit model, logarithm of ratio of
#' probabilities of two adjacent categories is
#' \deqn{log(P(y = k) / P(y = k - 1)) = b_0k + b_1 * x + b_2k * g + b_3 * x:g,}
#' where \eqn{x} is by default standardized total score (also called
#' Z-score) and \eqn{g} is a group membership.
#'
#' Using cumulative logit model, probability of gaining at least
#' \eqn{k} points is given by 2PL model, i.e.,
#' \deqn{P(y >= k) = exp(b_0k + b_1 * x + b_2k * g + b_3 * x:g) / (1 + exp(b_0k + b_1 * x + b_2k * g + b_3 * x:g)).}
#' The category probability (i.e., probability of gaining exactly
#' \eqn{k} points) is then \eqn{P(y = k) = P(y >= k) - P(y >= k + 1)}.
#'
#' Both models are estimated by iteratively reweighted least squares.
#' For more details see \code{\link[VGAM]{vglm}}.
#'
#' Missing values are allowed but discarded for item estimation. They
#' must be coded as \code{NA} for both, \code{Data} and \code{group}
#' parameters.
#'
#' @return The \code{difORD()} function returns an object of class
#'   \code{"difORD"}. The output including values of the test
#'   statistics, p-values, and items marked as DIF is displayed by the
#'   \code{print()} method.
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
#'   \item{\code{anchor}}{DIF free items specified by the \code{anchor} and \code{purify}.}
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
#'   \item{\code{match.name}}{Name of the matching criterion.}
#'   }
#'
#' For an object of class \code{"difORD"} several methods are available (e.g., \code{methods(class = "difORD")}).
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
#' Hladka, A. (2021). Statistical models for detection of differential item functioning. Dissertation thesis.
#' Faculty of Mathematics and Physics, Charles University.
#'
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression models for DIF and DDF detection.
#' The R Journal, 12(1), 300--323, \doi{10.32614/RJ-2020-014}.
#'
#' @seealso
#' \code{\link[difNLR]{plot.difORD}} for graphical representation of item characteristic curves. \cr
#' \code{\link[difNLR]{coef.difORD}} for extraction of item parameters with their standard errors. \cr
#' \code{\link[difNLR]{predict.difORD}} for calculation of predicted values. \cr
#' \code{\link[difNLR]{logLik.difORD}}, \code{\link[difNLR]{AIC.difORD}}, \code{\link[difNLR]{BIC.difORD}}
#' for extraction of log-likelihood and information criteria. \cr
#'
#' \code{\link[stats]{p.adjust}} for multiple comparison corrections. \cr
#' \code{\link[VGAM]{vglm}} for estimation function using iteratively reweighted least squares.
#'
#'
#' @examples
#' # loading data
#' data(Anxiety, package = "ShinyItemAnalysis")
#' Data <- Anxiety[, paste0("R", 1:29)] # items
#' group <- Anxiety[, "gender"] # group membership variable
#'
#' # testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
#' \dontrun{
#' # graphical devices
#' plot(x, item = 6)
#' plot(x, item = "R6")
#' plot(x, item = "R6", group.names = c("Males", "Females"))
#'
#' # estimated parameters
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
#' # testing both DIF effects with Benjamini-Hochberg adjustment method
#' difORD(Data, group, focal.name = 1, model = "adjacent", p.adjust.method = "BH")
#'
#' # testing both DIF effects with item purification
#' difORD(Data, group, focal.name = 1, model = "adjacent", purify = TRUE)
#'
#' # testing uniform DIF effects
#' difORD(Data, group, focal.name = 1, model = "adjacent", type = "udif")
#' # testing non-uniform DIF effects
#' difORD(Data, group, focal.name = 1, model = "adjacent", type = "nudif")
#'
#' # testing both DIF effects with different matching criteria
#' ddfMLR(Data, group, focal.name = 1, model = "adjacent", match = "score")
#' difORD(Data, group, focal.name = 1, model = "adjacent", match = "restscore")
#' difORD(Data, group, focal.name = 1, key, match = "zrestscore")
#' match <- rowSums(GMAT[, 1:20])
#' difORD(Data, group, focal.name = 1, key, match = match)
#' match <- replicate(ncol(Data), GMAT$criterion)
#' difORD(Data, group, focal.name = 1, key, match = match)
#' match <- as.data.frame(match)
#' difORD(Data, group, focal.name = 1, key, match = match)
#'
#' difORD(Data, group, focal.name = 1, model = "adjacent", match = "score")
#'
#' # testing both DIF effects with cumulative logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "cumulative"))
#' # graphical devices
#' plot(x, item = 7, plot.type = "cumulative")
#' plot(x, item = 7, plot.type = "category")
#'
#' # estimated parameters
#' coef(x, simplify = TRUE)
#' }
#' @keywords DIF
#' @export
difORD <- function(Data, group, focal.name, model = "adjacent", type = "both", match = "zscore",
                   anchor = NULL, purify = FALSE, nrIter = 10, p.adjust.method = "none",
                   alpha = 0.05, parametrization) {
  # === INPUTS CHECK and PROCESSING ===
  # === 0. Deprecation warnings ===
  if (!missing(parametrization)) {
    warning("The 'parametrization' argument is deprecated. Use 'coef.difORD()' instead.", call. = FALSE)
  }

  # === 1. Data and group check and processing ===
  group_result <- .resolve_group(Data, group, focal.name)
  GROUP <- group_result$GROUP
  DATA <- group_result$DATA
  group.names <- group_result$group_levels

  # checking structure of Data - ordinal items required
  DATA[] <- suppressWarnings(as.data.frame(sapply(DATA, function(x) as.numeric(paste(x)))))

  n <- nrow(DATA)
  m <- ncol(DATA)

  # === 2. Anchor items check and processing ===
  ANCHOR <- .resolve_anchor(anchor, DATA)

  # === 3. Matching criterion check and computation ===
  match_result <- .resolve_match(match = match, Data = DATA, anchor = ANCHOR)
  MATCH <- match_result$MATCH
  match.name <- match_result$match.name

  # === 4. Removing missing data ===
  df <- .resolve_missing(Data = DATA, group = GROUP, match = MATCH)
  DATA <- df[["DATA"]]
  GROUP <- df[["GROUP"]]
  MATCH <- df[["MATCH"]]

  # === 5. Model and type ===
  .check_character(model, "model", c("adjacent", "cumulative"))
  .check_character(type, "type", c("udif", "nudif", "both"))

  # === 6. Item purification and p.adjustment check ===
  .check_logical(purify, "purify")
  # deactivating item purification when anchors are specified
  if (!is.null(anchor) & purify) {
    purify <- FALSE
    warning("Item purification not activated as the set of anchor items is specified. ", call. = FALSE)
  }
  # purify only allowed when matching criterion is zscore or score
  if (purify && !any(match[1] %in% c("zscore", "score", "restscore", "zrestscore"))) {
    stop("'purify' is only allowed when 'match' is 'zscore' or 'score'.", call. = FALSE)
  }
  if (purify && (!is.numeric(nrIter) || nrIter <= 0 || nrIter %% 1 != 0)) {
    stop("'nrIter' must be a positive integer.", call. = FALSE)
  }
  .check_character(p.adjust.method, "p.adjust.method", p.adjust.methods)

  # === 7. alpha check ===
  .check_numeric(alpha, "alpha", 0, 1)

  # === DIF DETECTION ===
  if (!purify | !(match[1] %in% c("zscore", "score", "restscore", "zrestscore")) | !is.null(anchor)) {
    PROV <- suppressWarnings(ORD(
      Data = DATA, group = GROUP,
      model = model, match = MATCH, anchor = ANCHOR,
      type = type, p.adjust.method = p.adjust.method,
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
      parM0 = PROV$par.m0, parM1 = PROV$par.m1,
      covM0 = PROV$cov.m0, covM1 = PROV$cov.m1,
      llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
      AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
      BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
      DIFitems = DIFitems,
      model = model,
      type = type,
      anchor = ANCHOR,
      purification = purify,
      p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval,
      df = PROV$df,
      alpha = alpha,
      Data = DATA, group = GROUP, group.names = group.names, match = MATCH, match.name = match.name
    )
  } else {
    nrPur <- 0
    difPur <- NULL
    noLoop <- FALSE
    prov1 <- suppressWarnings(ORD(
      Data = DATA, group = GROUP,
      model = model, match = MATCH, anchor = ANCHOR, type = type,
      p.adjust.method = p.adjust.method,
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
          nodif <- if (is.null(dif)) {
            1:m # all items if no DIF
          } else {
            setdiff(1:m, dif) # items not in dif
          }
          match_result <- .resolve_match(match = match, Data = DATA, anchor = nodif)
          MATCH <- match_result$MATCH
          prov2 <- suppressWarnings(ORD(
            Data = DATA, group = GROUP,
            model = model, match = MATCH, anchor = nodif, type = type,
            p.adjust.method = p.adjust.method,
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
            } else {
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
    if (purify) {
      ANCHOR <- ANCHOR[!(ANCHOR %in% DIFitems)]
    }
    RES <- list(
      Sval = STATS,
      ordPAR = ordPAR,
      ordSE = ordSE,
      parM0 = PROV$par.m0, parM1 = PROV$par.m1,
      covM0 = PROV$cov.m0, covM1 = PROV$cov.m1,
      llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
      AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
      BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
      DIFitems = DIFitems,
      model = model,
      type = type,
      anchor = ANCHOR,
      purification = purify, nrPur = nrPur, difPur = difPur, conv.puri = noLoop,
      p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval,
      df = PROV$df,
      alpha = alpha,
      Data = DATA, group = GROUP, group.names = group.names, match = MATCH, match.name = match.name
    )
  }
  class(RES) <- "difORD"
  return(RES)
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
#' @description S3 method for extracting estimated model coefficients
#'   from an object of \code{"difORD"} class.
#' @aliases coefficients.difORD
#'
#' @param object an object of \code{"difORD"} class.
#' @param SE logical: should the standard errors of estimated
#'   parameters be also returned? (default is \code{FALSE}).
#' @param simplify logical: should the estimated parameters be
#'   simplified to a matrix? (default is \code{FALSE}).
#' @param IRTpars logical: should the estimated parameters be returned
#'   in IRT parameterization? (default is \code{TRUE}).
#' @param CI numeric: level of confidence interval for parameters,
#'   default is \code{0.95} for 95\% confidence interval.
#' @param ... other generic parameters for \code{coef()} function.
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
#' # loading data
#' data(Anxiety, package = "ShinyItemAnalysis")
#' Data <- Anxiety[, paste0("R", 1:29)] # items
#' group <- Anxiety[, "gender"] # group membership variable
#'
#' # testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
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
#' }
#' @export
coef.difORD <- function(object, SE = FALSE, simplify = FALSE, IRTpars = TRUE, CI = 0.95, ...) {
  .check_logical(SE, "SE")
  .check_logical(simplify, "simplify")
  .check_logical(IRTpars, "IRTpars")
  .check_numeric(CI, "CI", 0, 1)

  m <- dim(object$Data)[2]
  nams <- colnames(object$Data)

  if (!IRTpars) {
    pars <- object$ordPAR
    se <- object$ordSE
  } else {
    ordPAR <- object$ordPAR
    ordCOV <- ifelse(1:m %in% object$DIFitems, object$covM1, object$covM0)
    ordDM <- lapply(1:m, function(i) {
      .deltamethod.ORD.log2irt(
        par = ordPAR[[i]], cov = ordCOV[[i]]
      )
    })
    pars <- lapply(ordDM, function(x) x$par)
    se <- lapply(ordDM, function(x) x$se)
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
  } else {
    res <- coefs
  }

  return(res)
}

#' Log-likelihood and information criteria for an object of
#' \code{"difORD"} class.
#'
#' @aliases AIC.difORD BIC.difORD
#' @rdname logLik.difORD
#'
#' @description S3 methods for extracting log-likelihood, Akaike's
#'   information criterion (AIC) and Schwarz's Bayesian criterion
#'   (BIC) for an object of \code{"difORD"} class.
#'
#' @param object an object of \code{"difORD"} class.
#' @param item numeric or character: either character \code{"all"} to
#'   apply for all converged items (default), or a vector of item
#'   names (column names of \code{Data}), or item identifiers
#'   (integers specifying the column number).
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
#' @seealso \code{\link[difNLR]{difORD}} for DIF detection among
#' ordinal data. \cr \code{\link[stats]{logLik}} for generic function
#' extracting log-likelihood. \cr \code{\link[stats]{AIC}} for generic
#' function calculating AIC and BIC.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(Anxiety, package = "ShinyItemAnalysis")
#' Data <- Anxiety[, paste0("R", 1:29)] # items
#' group <- Anxiety[, "gender"] # group membership variable
#'
#' # testing both DIF effects with adjacent category logit model
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
  nams <- colnames(object$Data)
  m <- length(nams)

  # check item input, resolve item indices/names to numeric indices
  items <- .resolve_items(item = item, colnames_data = nams)

  val <- ifelse(items %in% object$DIFitems,
    object$llM1[items],
    object$llM0[items]
  )
  val <- val[items]

  if (length(items) == 1) {
    df <- ifelse(items %in% object$DIFitems,
      sapply(object$parM1, length)[items],
      sapply(object$parM0, length)[items]
    )

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
  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

  AIC <- ifelse(items %in% object$DIFitems, object$AICM1[items], object$AICM0[items])
  return(AIC)
}

#' @rdname logLik.difORD
#' @aliases AIC.difORD logLik.difORD
#' @export
BIC.difORD <- function(object, item = "all", ...) {
  m <- length(object$ordPAR)
  nams <- colnames(object$Data)
  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

  BIC <- ifelse(items %in% object$DIFitems, object$BICM1[items], object$BICM0[items])
  return(BIC)
}

#' ICC plots for an object of \code{"difORD"} class.
#'
#' @description Plot method for an object of \code{"difORD"} class
#'   using \pkg{ggplot2}.
#'
#'   The characteristic curves (category probabilities) for an item
#'   specified in \code{item} argument are plotted. Plotted curves
#'   represent the best model. For cumulative logit model, also
#'   cumulative probabilities may be plotted.
#'
#' @param x an object of \code{"difORD"} class.
#' @param item numeric or character: either character \code{"all"} to
#'   apply for all converged items (default), or a vector of item
#'   names (column names of \code{Data}), or item identifiers
#'   (integers specifying the column number).
#' @param plot.type character: which plot should be displayed for
#'   cumulative logit regression model. Either \code{"category"}
#'   (default) for category probabilities or \code{"cumulative"} for
#'   cumulative probabilities.
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
#' # loading data
#' data(Anxiety, package = "ShinyItemAnalysis")
#' Data <- Anxiety[, paste0("R", 1:29)] # items
#' group <- Anxiety[, "gender"] # group membership variable
#'
#' # testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
#'
#' # graphical devices
#' plot(x, item = 6)
#' plot(x, item = "R6", group.names = c("Males", "Females"))
#'
#' # testing both DIF effects with cumulative logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "cumulative"))
#' plot(x, item = 7, plot.type = "cumulative")
#' plot(x, item = 7, plot.type = "category")
#' }
#' @export
plot.difORD <- function(x, item = "all", plot.type, group.names, ...) {
  m <- length(x$ordPAR)
  nams <- colnames(x$Data)
  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

  if (missing(plot.type)) {
    plot.type <- "category"
  }
  if (x$model == "adjacent" & plot.type != "category") {
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

  xlab <- x$match.name
  matching <- x$match
  match <- sapply(matching, function(x) seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 300))

  plot_CC <- list()
  ylab <- ifelse(plot.type == "category", "Category probability", "Cumulative probability")

  for (i in items) {
    TITLE <- colnames(x$Data)[i]

    df.fitted <- data.frame(rbind(
      cbind(Group = "0", Match = match[, i], predict(x, item = i, match = match[, i], group = 0, type = plot.type)),
      cbind(Group = "1", Match = match[, i], predict(x, item = i, match = match[, i], group = 1, type = plot.type))
    ))
    if (plot.type == "cumulative") {
      # removing lowest category consisted of ones
      df.fitted <- df.fitted[, -3]
    }
    df.fitted <- reshape(
      data = df.fitted, direction = "long",
      varying = list(colnames(df.fitted)[-c(1:2)]), v.names = "Probability",
      idvar = c("Group", "Match"),
      timevar = "Category", times = colnames(df.fitted)[-c(1:2)]
    )
    df.fitted$Category <- factor(df.fitted$Category)
    levels(df.fitted$Category) <- gsub("PY", "", gsub("\\.", "", levels(df.fitted$Category)))
    if (plot.type == "cumulative") {
      levels(df.fitted$Category) <- paste0("P(Y >= ", levels(df.fitted$Category), ")")
    } else {
      levels(df.fitted$Category) <- paste0("P(Y = ", levels(df.fitted$Category), ")")
    }

    if (plot.type == "cumulative") {
      df.empirical <- table(matching[, i], x$Data[, i], x$group)
      tmp0 <- df.empirical[, , 1]
      tmp1 <- df.empirical[, , 2]

      # empirical cumulative values
      df.empirical.cum0 <- prop.table(tmp0, 1)
      df.empirical.cum0 <- cbind(
        t(apply(tmp0, 1, function(x) sum(x) - cumsum(x) + x)),
        t(apply(prop.table(tmp0, 1), 1, function(x) sum(x) - cumsum(x) + x))
      )
      df.empirical.cum0 <- data.frame(Match = as.numeric(rownames(tmp0)), Group = 0, df.empirical.cum0)

      df.empirical.cum1 <- prop.table(tmp1, 1)
      df.empirical.cum1 <- cbind(
        t(apply(tmp1, 1, function(x) sum(x) - cumsum(x) + x)),
        t(apply(prop.table(tmp1, 1), 1, function(x) sum(x) - cumsum(x) + x))
      )
      df.empirical.cum1 <- data.frame(Match = as.numeric(rownames(tmp1)), Group = 1, df.empirical.cum1)

      df.empirical.cum <- rbind(df.empirical.cum0, df.empirical.cum1)
      df.empirical.cum <- df.empirical.cum[complete.cases(df.empirical.cum), ]
      num.cat <- (ncol(df.empirical.cum) - 2) / 2

      df.empirical.cum.count <- reshape(
        data = df.empirical.cum[, c(1:2, 4:(2 + num.cat))], direction = "long",
        varying = list(colnames(df.empirical.cum)[4:(2 + num.cat)]), v.names = "Count",
        idvar = c("Group", "Match"),
        timevar = "Category"
      )
      df.empirical.cum.count$Category <- factor(df.empirical.cum.count$Category)
      levels(df.empirical.cum.count$Category) <- colnames(df.empirical.cum)[4:(2 + num.cat)]
      levels(df.empirical.cum.count$Category) <- paste0("P(Y >= ", gsub("X", "", levels(df.empirical.cum.count$Category)), ")")

      df.empirical.cum.prob <- reshape(
        data = df.empirical.cum[, c(1:2, (4 + num.cat):dim(df.empirical.cum)[2])], direction = "long",
        varying = list(colnames(df.empirical.cum)[(4 + num.cat):dim(df.empirical.cum)[2]]), v.names = "Probability",
        idvar = c("Group", "Match"),
        timevar = "Category"
      )
      df.empirical.cum.prob$Category <- factor(df.empirical.cum.prob$Category)
      levels(df.empirical.cum.prob$Category) <- colnames(df.empirical.cum)[(4 + num.cat):dim(df.empirical.cum)[2]]
      levels(df.empirical.cum.prob$Category) <- paste0(
        "P(Y >= ",
        gsub(
          "\\.1", "",
          gsub("X", "", levels(df.empirical.cum.prob$Category))
        ), ")"
      )

      df.empirical <- merge(df.empirical.cum.count, df.empirical.cum.prob, by = c("Match", "Group", "Category"))
    } else {
      df.empirical <- data.frame(Group = x$group, Match = matching[, i], Category = x$Data[, i])
      df.empirical.table <- as.data.frame(table(df.empirical[, c("Group", "Match")]))
      df.empirical.table2 <- as.data.frame(table(df.empirical))

      colnames(df.empirical.table2)[4] <- "Count"
      df.empirical <- merge(df.empirical.table, df.empirical.table2)
      df.empirical$Probability <- ifelse(df.empirical$Count == 0, 0, df.empirical$Count / df.empirical$Freq)
      df.empirical$Match <- as.numeric(paste(df.empirical$Match))
      levels(df.empirical$Category) <- paste0("P(Y = ", levels(df.empirical$Category), ")")
    }

    cbPalette <- c("black", "#ffbe33", "#34a4e5", "#ce7eaa", "#00805e", "#737373", "#f4eb71", "#0072B2", "#D55E00")
    if (plot.type == "cumulative") {
      cbPalette <- cbPalette[-1]
    }
    num.col <- ceiling(length(levels(df.fitted$Category)) / 8)
    cols <- rep(cbPalette, num.col)[1:length(levels(df.fitted$Category))]

    plot_CC[[i]] <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df.empirical,
        ggplot2::aes(
          x = .data$Match, y = .data$Probability, group = .data$Category,
          size = .data$Count, col = .data$Category, fill = .data$Category
        ),
        shape = 21, alpha = 0.5
      ) +
      ggplot2::geom_line(
        data = df.fitted,
        ggplot2::aes(
          x = .data$Match, y = .data$Probability,
          col = .data$Category, linetype = .data$Group
        ),
        linewidth = 0.8
      ) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::ylim(0, 1) +
      ggplot2::ggtitle(TITLE) +
      ggplot2::scale_linetype_manual(
        breaks = c(0, 1),
        labels = group.names,
        values = c("solid", "dashed")
      ) +
      ggplot2::scale_fill_manual(values = cols) +
      ggplot2::scale_color_manual(values = cols) +
      .plot.theme() +
      # legend
      .plot.theme.legend() +
      ggplot2::guides(
        size = ggplot2::guide_legend(title = "Count", order = 1),
        colour = ggplot2::guide_legend(title = "Score", order = 2),
        fill = ggplot2::guide_legend(title = "Score", order = 2),
        linetype = ggplot2::guide_legend(title = "Group", order = 3)
      )
  }
  plot_CC <- plot_CC[items]
  names(plot_CC) <- nams[items]
  return(plot_CC)
}

#' Predicted values for an object of \code{"difORD"} class.
#'
#' @description S3 method for predictions from the model used in the
#'   object of \code{"difORD"} class.
#'
#' @param object an object of \code{"difORD"} class.
#' @param item numeric or character: either character \code{"all"} to
#'   apply for all converged items (default), or a vector of item
#'   names (column names of \code{Data}), or item identifiers
#'   (integers specifying the column number).
#' @param match numeric: matching criterion for new observations.
#' @param group numeric: group membership for new observations.
#' @param type character: type of probability to be computed. Either
#'   \code{"category"} for category probabilities or
#'   \code{"cumulative"} for cumulative probabilities. Cumulative
#'   probabilities are available only for cumulative logit model.
#' @param ... other generic parameters for \code{predict()} function.
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
#' @references
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression models for DIF and DDF detection.
#' The R Journal, 12(1), 300--323, \doi{10.32614/RJ-2020-014}.
#'
#' @seealso
#' \code{\link[difNLR]{difORD}} for DIF detection among ordinal data using either cumulative logit or adjacent category logit model. \cr
#' \code{\link[stats]{predict}} for generic function for prediction.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(Anxiety, package = "ShinyItemAnalysis")
#' Data <- Anxiety[, paste0("R", 1:29)] # items
#' group <- Anxiety[, "gender"] # group membership variable
#'
#' # testing both DIF effects with cumulative logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "cumulative"))
#'
#' # fitted values
#' predict(x, item = "R6")
#'
#' # predicted values
#' predict(x, item = "R6", match = 0, group = c(0, 1))
#' predict(x, item = "R6", match = 0, group = c(0, 1), type = "cumulative")
#' predict(x, item = c("R6", "R7"), match = 0, group = c(0, 1))
#'
#' # testing both DIF effects with adjacent category logit model
#' (x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
#'
#' # fitted values
#' predict(x, item = "R6")
#'
#' # predicted values
#' predict(x, item = "R6", match = 0, group = c(0, 1))
#' predict(x, item = c("R6", "R7"), match = 0, group = c(0, 1))
#' }
#'
#' @export
predict.difORD <- function(object, item = "all", match, group, type = "category", ...) {
  m <- ncol(object$Data)
  nams <- colnames(object$Data)

  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

  # extracting matching variable
  if (missing(match)) {
    match <- object$match
  }

  if (missing(group)) {
    group <- object$group
  } else {
    group_levels <- na.omit(unique(group))
    if (!all(group_levels %in% unique(object$group))) {
      stop("Invalid value for 'group'.", call. = FALSE)
    }
  }

  if (is.matrix(match) | is.data.frame(match)) {
    if (nrow(match) != length(group)) {
      stop("'group' must be of the same length as number of observations in 'match'. ", call. = FALSE)
    }
    if (length(group) == 1) {
      group <- rep(group, nrow(match))
    }
  } else {
    if (length(match) != length(group)) {
      if (length(match) == 1) {
        match <- rep(match, length(group))
      } else if (length(group) == 1) {
        group <- rep(group, length(match))
      } else {
        stop("Arguments 'match' and 'group' must be of the same length.", call. = FALSE)
      }
    }
    match <- replicate(ncol(object$Data), match)
  }

  if (object$model == "adjacent" & type != "category") {
    warning("Argument 'type' is ignored for adjacent category logit model. Category probabilities are returned.", call. = FALSE)
  }
  if (!is.character(type)) {
    stop("'type' must be a character vector.", call. = FALSE)
  }
  if (!type %in% c("category", "cumulative")) {
    stop("'type' must be either 'category' or 'cumulative'.", call. = FALSE)
  }

  coefs_all <- coef(object, IRTpars = FALSE, CI = 0)
  res <- list()

  for (i in items) {
    mat <- cbind("intercept" = 1, "match" = match[, i], "group" = group, "x:match" = match[, i] * group)
    cat <- sort(unique(object$Data[, i]))
    num.cat <- length(cat)
    num.cat.est <- num.cat - 1
    cat.est <- cat[-1]

    coefs <- coefs_all[[i]]
    coefs <- c(coefs, rep(0, num.cat.est + 3 - length(coefs)))
    coefs <- sapply(1:num.cat.est, function(j) coefs[c(j, (num.cat.est + 1):length(coefs))])

    if (object$model == "adjacent") {
      # calculation probabilities on formula exp(\sum_{t = 0}^{k} b_{0t} + b1X)/(\sum_{r = 0}^{K}exp(\sum_{t = 0}^{r}b_{0t} + b1X))
      df.probs.cat <- matrix(0,
        nrow = nrow(mat), ncol = num.cat,
        dimnames = list(NULL, cat)
      )
      df.probs.cat[, as.character(cat.est)] <- mat %*% coefs
      # cumulative sum
      df.probs.cat <- t(apply(df.probs.cat, 1, cumsum))
      # exponential
      df.probs.cat <- exp(df.probs.cat)
      # norming
      df.probs.cat <- df.probs.cat / rowSums(df.probs.cat)
      colnames(df.probs.cat) <- paste0("P(Y = ", cat, ")")
      df.probs.cat <- as.data.frame(df.probs.cat)
    } else {
      # cumulative probabilities
      df.probs.cum <- matrix(1,
        nrow = nrow(mat), ncol = num.cat,
        dimnames = list(NULL, cat)
      )

      # calculation of cumulative probabilities based on formula P(Y >= k) = exp(b0 + b1*x)/(1 + exp(b0 + b1*x))
      df.probs.cum[, as.character(cat.est)] <- exp(mat %*% coefs) / (1 + exp(mat %*% coefs))

      # if column between non-ones valued columns consist of ones, it has to be changed to value on the left side
      need.correction <- which(sapply(2:num.cat, function(i) (all(df.probs.cum[, i] == 1) & all(df.probs.cum[, i - 1] != 1))))
      df.probs.cum[, need.correction + 1] <- df.probs.cum[, need.correction]
      colnames(df.probs.cum) <- paste0("P(Y >= ", cat, ")")
      df.probs.cum <- as.data.frame(df.probs.cum)

      # category probabilities
      df.probs.cat <- data.frame(
        sapply(1:(num.cat - 1), function(i) df.probs.cum[, i] - df.probs.cum[, i + 1]),
        df.probs.cum[, num.cat]
      )
      colnames(df.probs.cat) <- paste0("P(Y = ", cat, ")")
      df.probs.cat <- as.data.frame(df.probs.cat)
    }

    if (type == "category") {
      prob <- df.probs.cat
    } else {
      prob <- df.probs.cum
    }
    res[[i]] <- prob
  }

  res <- res[items]
  names(res) <- nams[items]
  if (length(res) == 1) {
    res <- res[[1]]
  } else {
    names(res) <- nams[items]
  }
  return(res)
}
