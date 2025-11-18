#' DDF detection for nominal data.
#'
#' @aliases ddfMLR
#'
#' @description Performs DDF detection procedure for nominal data
#'   based on multinomial log-linear regression model and likelihood
#'   ratio test of a submodel.
#'
#' @param Data data.frame or matrix: dataset which rows represent
#'   unscored examinee answers (nominal) and columns correspond to the
#'   items. In addition, \code{Data} can hold the vector of group
#'   membership.
#' @param group numeric or character: a dichotomous vector of the same
#'   length as \code{nrow(Data)} or a column identifier of
#'   \code{Data}.
#' @param focal.name numeric or character: indicates the level of
#'   \code{group} which corresponds to focal group.
#' @param key character: the answer key. Each element corresponds to
#'   the correct answer of one item.
#' @param type character: type of DDF to be tested. Either
#'   \code{"both"} for uniform and non-uniform DDF (i.e., difference
#'   in parameters \code{"a"} and \code{"b"}) (default), or
#'   \code{"udif"} for uniform DDF only (i.e., difference in
#'   difficulty parameter \code{"b"}), or \code{"nudif"} for
#'   non-uniform DDF only (i.e., difference in discrimination
#'   parameter \code{"a"}). Can be specified as a single value (for
#'   all items) or as an item-specific vector.
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
#'   \code{\link[difNLR]{coef.ddfMLR}} for different
#'   parameterizations.
#'
#' @usage
#' ddfMLR(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
#'        purify = FALSE, nrIter = 10, p.adjust.method = "none",
#'        alpha = 0.05, parametrization)
#'
#' @details
#' Performs DDF detection procedure for nominal data based on
#' multinomial log-linear regression model and likelihood ratio test
#' of submodel. Probability of selection the \eqn{k}-th category
#' (distractor) is
#' \deqn{P(y = k) = exp((a_k + a_kDif * g) * (x - b_k - b_kDif * g))) / (1 + \sum exp((a_l + a_lDif * g) * (x - b_l - b_lDif * g))), }
#' where \eqn{x} is by default standardized total score (also called
#' Z-score) and \eqn{g} is a group membership. Parameters \eqn{a_k}
#' and \eqn{b_k} are discrimination and difficulty for the \eqn{k}-th
#' category. Terms \eqn{a_kDif} and \eqn{b_kDif} then represent
#' differences between two groups (reference and focal) in relevant
#' parameters. Probability of correct answer (specified in argument
#' \code{key}) is
#' \deqn{P(y = k) = 1/(1 + \sum exp((a_l + a_lDif * g)*(x - b_l - b_lDif * g))). }
#' Parameters are estimated via neural networks. For more details see
#' \code{\link[nnet]{multinom}}.
#'
#' Missing values are allowed but discarded for item estimation. They
#' must be coded as \code{NA} for both, \code{Data} and \code{group}
#' arguments.
#'
#' @return The \code{ddfMLR()} function returns an object of class
#'   \code{"ddfMLR"}. The output including values of the test
#'   statistics, p-values, and items marked as DDF is displayed by the
#'   \code{print()} method.
#'
#' A list of class \code{"ddfMLR"} with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{mlrPAR}}{the estimates of final model.}
#'   \item{\code{mlrSE}}{standard errors of the estimates of final model.}
#'   \item{\code{parM0}}{the estimates of null model.}
#'   \item{\code{parM1}}{the estimates of alternative model.}
#'   \item{\code{llM0}}{log-likelihood of null model.}
#'   \item{\code{llM1}}{log-likelihood of alternative model.}
#'   \item{\code{AIC0}}{AIC of null model.}
#'   \item{\code{AIC1}}{AIC of alternative model.}
#'   \item{\code{BIC0}}{BIC of null model.}
#'   \item{\code{BIC1}}{BIC of alternative model.}
#'   \item{\code{DDFitems}}{either the column identifiers of the items which were detected as DDF, or
#'   \code{"No DDF item detected"} in case no item was detected as DDF.}
#'   \item{\code{type}}{character: type of DDF that was tested.}
#'   \item{\code{anchor}}{DIF free items specified by the \code{anchor} and \code{purify}.}
#'   \item{\code{purification}}{\code{purify} value.}
#'   \item{\code{nrPur}}{number of iterations in item purification process. Returned only if \code{purify}
#'   is \code{TRUE}.}
#'   \item{\code{ddfPur}}{a binary matrix with one row per iteration of item purification and one column per item.
#'   \code{"1"} in i-th row and j-th column means that j-th item was identified as DDF in i-th iteration. Returned only
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
#'   \item{\code{key}}{key of correct answers.}
#'   \item{\code{match}}{matching criterion.}
#'   \item{\code{match.name}}{Name of the matching criterion.}
#'   }
#'
#' For an object of class \code{"ddfMLR"} several methods are available (e.g. \code{methods(class = "ddfMLR")}).
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
#' \code{\link[difNLR]{plot.ddfMLR}} for graphical representation of item characteristic curves. \cr
#' \code{\link[difNLR]{coef.ddfMLR}} for extraction of item parameters with their standard errors. \cr
#' \code{\link[difNLR]{logLik.ddfMLR}}, \code{\link[difNLR]{AIC.ddfMLR}}, \code{\link[difNLR]{BIC.ddfMLR}}
#' for extraction of log-likelihood and information criteria. \cr
#'
#' \code{\link[stats]{p.adjust}} for multiple comparison corrections. \cr
#' \code{\link[nnet]{multinom}} for estimation function using neural networks.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMATtest, GMATkey)
#' Data <- GMATtest[, 1:20] # items
#' group <- GMATtest[, "group"] # group membership variable
#' key <- GMATkey # correct answers
#'
#' # testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
#'
#' # graphical devices
#' plot(x, item = "Item1", group.names = c("Group 1", "Group 2"))
#' plot(x, item = x$DDFitems)
#' plot(x, item = 1)
#'
#' # estimated parameters
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
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
#' # testing both DDF effects with Benjamini-Hochberg adjustment method
#' ddfMLR(Data, group, focal.name = 1, key, p.adjust.method = "BH")
#'
#' # testing both DDF effects with item purification
#' ddfMLR(Data, group, focal.name = 1, key, purify = TRUE)
#'
#' # testing uniform DDF effects
#' ddfMLR(Data, group, focal.name = 1, key, type = "udif")
#' # testing non-uniform DDF effects
#' ddfMLR(Data, group, focal.name = 1, key, type = "nudif")
#'
#' # testing both DDF effects with different matching criteria
#' ddfMLR(Data, group, focal.name = 1, key, match = "score")
#' ddfMLR(Data, group, focal.name = 1, key, match = "restscore")
#' ddfMLR(Data, group, focal.name = 1, key, match = "zrestscore")
#' match <- rowSums(GMAT[, 1:20])
#' ddfMLR(Data, group, focal.name = 1, key, match = match)
#' match <- replicate(ncol(Data), GMAT$criterion)
#' ddfMLR(Data, group, focal.name = 1, key, match = match)
#' match <- as.data.frame(match)
#' ddfMLR(Data, group, focal.name = 1, key, match = match)
#' }
#'
#' @keywords DDF
#' @export
ddfMLR <- function(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
                   purify = FALSE, nrIter = 10, p.adjust.method = "none",
                   alpha = 0.05, parametrization) {
  # === INPUTS CHECK and PROCESSING ===
  # === 0. Deprecation warnings ===
  if (!missing(parametrization)) {
    warning("Argument 'parametrization' is deprecated; please use 'coef.difORD()'.", call. = FALSE)
  }

  # === 1. Data and group check and processing ===
  group_result <- .resolve_group(Data, group, focal.name)
  GROUP <- group_result$GROUP
  DATA <- group_result$DATA
  group.names <- group_result$group_levels

  n <- nrow(DATA)
  m <- ncol(DATA)

  # checking structure of Data - nominal items required
  DATA[] <- lapply(DATA[], as.factor)
  DATA[] <- lapply(DATA[], droplevels)

  # === 2. Key check ===
  if (is.matrix(key) | is.data.frame(key)) {
    KEY <- as.vector(t(key))
  } else {
    KEY <- key
  }
  if (length(KEY) != ncol(DATA)) {
    stop("'key' must be of length equal to the number of columns (items) in 'Data'.", call. = FALSE)
  }
  if (any(is.na(KEY))) {
    stop("Missing values not allowed for 'key'. All item keys must be defined.", call. = FALSE)
  }
  KEY <- paste(KEY)

  # === 3. Anchor items check and processing ===
  ANCHOR <- .resolve_anchor(anchor, DATA)

  # === 4. Matching criterion check and computation ===
  match_result <- .resolve_match(match = match, Data = DATA, anchor = ANCHOR, key = KEY)
  MATCH <- match_result$MATCH
  match.name <- match_result$match.name

  # === 5. Removing missing data ===
  df <- .resolve_missing(Data = DATA, group = GROUP, match = MATCH)
  DATA <- df[["DATA"]]
  GROUP <- df[["GROUP"]]
  MATCH <- df[["MATCH"]]

  # === 6. Type ===
  .check_character(type, "type", c("udif", "nudif", "both"))

  # === 7. Item purification and p.adjustment check ===
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

  # === 8. alpha check ===
  .check_numeric(alpha, "alpha", 0, 1)

  # === DIF DETECTION ===
    if (!purify | !(match[1] %in% c("zscore", "score")) | !is.null(anchor)) {
      PROV <- suppressWarnings(MLR(
        Data = DATA, group = GROUP,
        key = KEY, match = match, anchor = ANCHOR,
        type = type, p.adjust.method = p.adjust.method,
        alpha = alpha
      ))

      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      se.m1 <- PROV$se.m1
      se.m0 <- PROV$se.m0
      significant <- which(ADJ.PVAL < alpha)

      if (length(significant) > 0) {
        DDFitems <- significant
        mlrPAR <- PROV$par.m0
        mlrSE <- se.m0
        for (idif in 1:length(DDFitems)) {
          mlrPAR[[DDFitems[idif]]] <- PROV$par.m1[[DDFitems[idif]]]
          mlrSE[[DDFitems[idif]]] <- se.m1[[DDFitems[idif]]]
        }
      } else {
        DDFitems <- "No DDF item detected"
        mlrPAR <- PROV$par.m0
        mlrSE <- se.m0
      }

      nrow <- sapply(mlrPAR, nrow)
      colnams <- lapply(mlrPAR, colnames)
      rownams <- lapply(mlrPAR, rownames)
      mlrSE <- lapply(1:length(mlrSE), function(x) {
        matrix(mlrSE[[x]],
          nrow = nrow[[x]],
          dimnames = list(rownams[[x]], colnams[[x]])
        )
      })
      RES <- list(
        Sval = STATS,
        mlrPAR = mlrPAR,
        mlrSE = mlrSE,
        parM0 = PROV$par.m0, parM1 = PROV$par.m1,
        covM0 = PROV$cov.m0, covM1 = PROV$cov.m1,
        llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
        AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
        BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
        DDFitems = DDFitems,
        anchor = ANCHOR,
        type = type,
        purification = purify,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, key = c(KEY), match = MATCH, match.name = match.name
      )
    } else {
      nrPur <- 0
      ddfPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(MLR(
        Data = DATA, group = GROUP,
        key = KEY, type = type,
        p.adjust.method = p.adjust.method,
        alpha = alpha
      ))
      stats1 <- prov1$Sval
      pval1 <- prov1$pval
      significant1 <- which(pval1 < alpha)
      if (length(significant1) == 0) {
        PROV <- prov1
        STATS <- stats1
        DDFitems <- "No DDF item detected"
        se.m1 <- PROV$se.m1
        se.m0 <- PROV$se.m0
        mlrPAR <- PROV$par.m0
        mlrSE <- se.m0
        noLoop <- TRUE
      } else {
        dif <- significant1
        ddfPur <- rep(0, length(stats1))
        ddfPur[dif] <- 1
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
            match_result <- .resolve_match(match = match, Data = DATA, anchor = nodif, key = KEY)
            MATCH <- match_result$MATCH
            prov2 <- suppressWarnings(MLR(
              Data = DATA, group = GROUP,
              key = KEY, match = MATCH, anchor = nodif, type = type,
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
            ddfPur <- rbind(ddfPur, rep(0, dim(DATA)[2]))
            ddfPur[nrPur + 1, dif2] <- 1
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
        mlrPAR <- PROV$par.m0
        mlrSE <- se.m0
        if (length(significant1) > 0) {
          DDFitems <- significant1
          for (idif in 1:length(DDFitems)) {
            mlrPAR[[DDFitems[idif]]] <- PROV$par.m1[[DDFitems[idif]]]
            mlrSE[[DDFitems[idif]]] <- se.m1[[DDFitems[idif]]]
          }
        } else {
          DDFitems <- "No DDF item detected"
        }
      }
      if (!is.null(ddfPur)) {
        rownames(ddfPur) <- paste("Step", 0:(dim(ddfPur)[1] - 1), sep = "")
        colnames(ddfPur) <- colnames(DATA)
      }
      if (purify) {
        ANCHOR <- ANCHOR[!(ANCHOR %in% DDFitems)]
      }
      RES <- list(
        Sval = STATS,
        mlrPAR = mlrPAR,
        mlrSE = mlrSE,
        parM0 = PROV$par.m0, parM1 = PROV$par.m1,
        covM0 = PROV$cov.m0, covM1 = PROV$cov.m1,
        llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
        AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
        BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
        DDFitems = DDFitems,
        type = type,
        anchor = ANCHOR,
        purification = purify, nrPur = nrPur, ddfPur = ddfPur, conv.puri = noLoop,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, key = c(KEY), match = MATCH, match.name = match.name
      )
    }
    class(RES) <- "ddfMLR"
    return(RES)

}

#' @export
print.ddfMLR <- function(x, ...) {
  title <- switch(x$type,
    both = "Detection of both types of Differential Distractor Functioning using multinomial log-linear regression model",
    udif = "Detection of uniform Differential Distractor Functioning using multinomial log-linear regression model",
    nudif = "Detection of non-uniform Differential Distractor Functioning using multinomial log-linear regression model"
  )
  cat(paste(strwrap(title, width = 60), collapse = "\n"))
  cat("\n\nLikelihood-ratio chi-square statistics\n")
  if (x$purification) word.iteration <- ifelse(x$nrPur <= 1, " iteration", " iterations")
  cat(paste("\nItem purification was", ifelse(x$purification, " ", " not "), "applied",
    ifelse(x$purification, paste(" with ", x$nrPur, word.iteration, sep = ""), ""), "\n",
    sep = ""
  ))
  if (x$purification) {
    if (!x$conv.puri) {
      cat(paste("WARNING: Item purification process not converged after "), x$nrPur, word.iteration, "\n",
        "         Results are based on last iteration of the item purification.\n",
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

  if (is.character(x$DDFitems)) {
    switch(x$type,
      both = cat("\nNone of items is detected as DDF"),
      udif = cat("\nNone of items is detected as uniform DDF"),
      nudif = cat("\nNone of items is detected as non-uniform DDF")
    )
  } else {
    switch(x$type,
      both = cat("\nItems detected as DDF items:"),
      udif = cat("\nItems detected as uniform DDF items:"),
      nudif = cat("\nItems detected as non-uniform DDF items:")
    )
    cat("\n", paste(colnames(x$Data)[x$DDFitems], "\n", sep = ""))
  }
}

#' ICC plots for an object of \code{"ddfMLR"} class.
#'
#' @description Plot method for an object of \code{"ddfMLR"} class
#'   using \pkg{ggplot2}.
#'
#'   The characteristic curves for an item specified in \code{item}
#'   argument are plotted. Plotted curves represent the best model.
#'
#' @param x an object of \code{"ddfMLR"} class.
#' @param item numeric or character: either character \code{"all"} to
#'   apply for all items (default), or a vector of item names (column
#'   names of \code{Data}), or item identifiers (integers specifying
#'   the column number).
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
#' \code{\link[difNLR]{ddfMLR}} for DDF detection. \cr
#' \code{\link[ggplot2]{ggplot}} for general function to plot a \code{"ggplot"} object.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMATtest, GMATkey)
#' Data <- GMATtest[, 1:20] # items
#' group <- GMATtest[, "group"] # group membership variable
#' key <- GMATkey # correct answers
#'
#' # testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
#'
#' # graphical devices
#' plot(x, item = "Item1", group.names = c("Group 1", "Group 2"))
#' plot(x, item = x$DDFitems)
#' plot(x, item = c(3, 1, 5))
#' }
#' @export
plot.ddfMLR <- function(x, item = "all", group.names, ...) {
  m <- length(x$mlrPAR)
  nams <- colnames(x$Data)
  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

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

  for (i in items) {
    TITLE <- colnames(x$Data)[i]

    df.fitted <- data.frame(rbind(
      cbind(Group = "0", Match = match[, i], predict(x, item = i, match = match[, i], group = 0)),
      cbind(Group = "1", Match = match[, i], predict(x, item = i, match = match[, i], group = 1))
    ), check.names = FALSE)
    df.fitted <- reshape(
      data = df.fitted, direction = "long",
      varying = list(colnames(df.fitted)[-c(1:2)]), v.names = "Probability",
      idvar = c("Group", "Match"),
      timevar = "Category", times = colnames(df.fitted)[-c(1:2)]
    )
    df.fitted$Category <- factor(df.fitted$Category)
    df.fitted$Category <- relevel(df.fitted$Category, ref = paste(x$key[i]))
    levels(df.fitted$Category) <- paste0("P(Y = ", levels(df.fitted$Category), ")")
    n_cats <- length(levels(df.fitted$Category))

    df.empirical <- rbind(
      data.frame(prop.table(table(x$Data[x$group == 1, i], matching[x$group == 1, i]), 2),
        table(x$Data[x$group == 1, i], matching[x$group == 1, i]),
        group = "1"
      ),
      data.frame(prop.table(table(x$Data[x$group == 0, i], matching[x$group == 0, i]), 2),
        table(x$Data[x$group == 0, i], matching[x$group == 0, i]),
        group = "0"
      )
    )
    df.empirical <- data.frame(
      Group = as.factor(df.empirical$group),
      Match = as.numeric(paste(df.empirical$Var2)),
      Category = df.empirical$Var1,
      Count = as.numeric(paste(df.empirical$Freq.1)),
      Probability = as.numeric(paste(df.empirical$Freq))
    )
    df.empirical$Category <- paste0("P(Y = ", df.empirical$Category, ")")
    df.empirical$Category <- factor(df.empirical$Category, levels = levels(df.fitted$Category))

    cbPalette <- c("#ffbe33", "#34a4e5", "#ce7eaa", "#00805e", "#737373", "#f4eb71", "#0072B2", "#D55E00")
    n_col <- ceiling(n_cats / 8)
    cols <- rep(cbPalette, n_col)[1:n_cats]

    plot_CC[[i]] <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = df.fitted,
        ggplot2::aes(
          x = .data$Match, y = .data$Probability,
          colour = .data$Category, linetype = .data$Group
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = df.empirical,
        ggplot2::aes(
          x = .data$Match, y = .data$Probability,
          colour = .data$Category, fill = .data$Category,
          size = .data$Count
        ),
        alpha = 0.5, shape = 21
      ) +
      ggplot2::ylim(0, 1) +
      ggplot2::ggtitle(TITLE) +
      ggplot2::labs(
        x = xlab,
        y = "Probability of answer"
      ) +
      ggplot2::scale_linetype_manual(
        breaks = c(0, 1), labels = group.names,
        values = c("solid", "dashed")
      ) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::scale_fill_manual(values = cols) +
      .plot.theme() +
      # legend
      .plot.theme.legend() +
      ggplot2::guides(
        size = ggplot2::guide_legend(title = "Count", order = 1),
        colour = ggplot2::guide_legend(title = "Answer", order = 2),
        fill = ggplot2::guide_legend(title = "Answer", order = 2),
        linetype = ggplot2::guide_legend(title = "Group", order = 3)
      )
  }
  plot_CC <- plot_CC[items]
  names(plot_CC) <- nams[items]
  return(plot_CC)
}

#' Extract model coefficients from an object of \code{"ddfMLR"} class.
#'
#' @description S3 method for extracting estimated model coefficients
#'   from an object of \code{"ddfMLR"} class.
#' @aliases coefficients.ddfMLR
#'
#' @param object an object of \code{"ddfMLR"} class.
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
#' \code{\link[difNLR]{ddfMLR}} for DDF detection among nominal data. \cr
#' \code{\link[stats]{coef}} for generic function extracting model coefficients.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMATtest, GMATkey)
#' Data <- GMATtest[, 1:20] # items
#' group <- GMATtest[, "group"] # group membership variable
#' key <- GMATkey # correct answers
#'
#' # testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
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
coef.ddfMLR <- function(object, SE = FALSE, simplify = FALSE, IRTpars = TRUE, CI = 0.95, ...) {
  .check_logical(SE, "SE")
  .check_logical(simplify, "simplify")
  .check_logical(IRTpars, "IRTpars")
  .check_numeric(CI, "CI", 0, 1)

  m <- dim(object$Data)[2]
  nams <- colnames(object$Data)

  if (!IRTpars) {
    pars <- object$mlrPAR
    se <- object$mlrSE
  } else {
    mlrPAR <- object$mlrPAR
    mlrCOV <- ifelse(1:m %in% object$DDFitems, object$covM1, object$covM0)
    mlrDM <- lapply(1:m, function(i) {
      .deltamethod.MLR.log2irt(
        par = mlrPAR[[i]], cov = mlrCOV[[i]]
      )
    })
    se <- lapply(mlrDM, function(x) x$se)
    pars <- lapply(1:m, function(i) {
      if (any(is.null(dim(mlrDM[[i]]$par)))) {
        par_names <- names(mlrDM[[i]]$par)
      } else {
        par_names <- colnames(mlrDM[[i]]$par)
      }
      matrix(mlrDM[[i]]$par,
        nrow = nrow(mlrPAR[[i]]), ncol = ncol(mlrPAR[[i]]),
        dimnames = list(rownames(mlrPAR[[i]]), par_names)
      )
    })
  }
  cats <- lapply(1:m, function(i) rownames(pars[[i]]))
  names(pars) <- names(cats) <- names(se) <- nams

  if (SE) {
    se_matrix <- lapply(1:m, function(i) {
      matrix(se[[i]],
        nrow = nrow(pars[[i]]), ncol = ncol(pars[[i]]),
        dimnames = list(rownames(pars[[i]]), colnames(pars[[i]]))
      )
    })
    coefs <- lapply(1:m, function(i) rbind(pars[[i]], se_matrix[[i]]))
    coefs <- lapply(1:m, function(i) {
      rownames(coefs[[i]]) <- paste(rep(c("estimate", "SE"), each = nrow(coefs[[i]]) / 2), cats[[i]], sep = "_")
      coefs[[i]]
    })
  } else {
    coefs <- pars
  }
  names(coefs) <- nams

  if (CI > 0) {
    alpha <- (1 - CI) / 2
    CIlow <- lapply(1:m, function(i) pars[[i]] - qnorm(1 - alpha) * se[[i]])
    CIupp <- lapply(1:m, function(i) pars[[i]] + qnorm(1 - alpha) * se[[i]])
    coefs <- lapply(1:m, function(i) rbind(coefs[[i]], CIlow[[i]], CIupp[[i]]))
    names(coefs) <- nams
    if (SE) {
      tmp <- c("estimate", "SE", paste0("CI", 100 * alpha), paste0("CI", 100 * (1 - alpha)))
    } else {
      tmp <- c("estimate", paste0("CI", 100 * alpha), paste0("CI", 100 * (1 - alpha)))
    }
    coefs <- lapply(1:m, function(i) {
      rownames(coefs[[i]]) <- paste(rep(tmp, each = nrow(coefs[[i]]) / length(tmp)), cats[[i]], sep = "_")
      coefs[[i]]
    })
    names(coefs) <- nams
  }

  if (simplify) {
    res <- as.data.frame(plyr::ldply(coefs, rbind, .id = NULL))
    id <- unlist(lapply(nams, function(i) paste(i, rownames(coefs[[i]]))))
    rownames(res) <- id
    res[is.na(res)] <- 0
  } else {
    res <- coefs
  }

  return(res)
}

#' Log-likelihood and information criteria for an object of \code{"ddfMLR"} class.
#'
#' @aliases AIC.ddfMLR BIC.ddfMLR
#' @rdname logLik.ddfMLR
#'
#' @description S3 methods for extracting log-likelihood, Akaike's
#'   information criterion (AIC) and Schwarz's Bayesian criterion
#'   (BIC) for an object of \code{"ddfMLR"} class.
#'
#' @param object an object of \code{"ddfMLR"} class.
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
#' @seealso
#' \code{\link[difNLR]{ddfMLR}} for DDF detection among nominal data. \cr
#' \code{\link[stats]{logLik}} for generic function extracting log-likelihood. \cr
#' \code{\link[stats]{AIC}} for generic function calculating AIC and BIC.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMATtest, GMATkey)
#' Data <- GMATtest[, 1:20] # items
#' group <- GMATtest[, "group"] # group membership variable
#' key <- GMATkey # correct answers
#'
#' # testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
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
logLik.ddfMLR <- function(object, item = "all", ...) {
  m <- length(object$mlrPAR)
  nams <- colnames(object$Data)
  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

  val <- ifelse(items %in% object$DDFitems,
    object$llM1[items],
    object$llM0[items]
  )
  df <- ifelse(items %in% object$DDFitems,
    sapply(object$parM1, length)[items],
    sapply(object$parM0, length)[items]
  )
  if (length(items) == 1) {
    attr(val, "df") <- df
    class(val) <- "logLik"
  }
  return(val)
}

#' @rdname logLik.ddfMLR
#' @aliases BIC.ddfMLR logLik.ddfMLR
#' @export
AIC.ddfMLR <- function(object, item = "all", ...) {
  m <- length(object$mlrPAR)
  nams <- colnames(object$Data)
  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

  AIC <- ifelse(items %in% object$DDFitems, object$AICM1[items], object$AICM0[items])
  return(AIC)
}

#' @rdname logLik.ddfMLR
#' @aliases AIC.ddfMLR logLik.ddfMLR
#' @export
BIC.ddfMLR <- function(object, item = "all", ...) {
  m <- length(object$mlrPAR)
  nams <- colnames(object$Data)
  # check item input
  items <- .resolve_items(item = item, colnames_data = nams)

  BIC <- ifelse(items %in% object$DDFitems, object$BICM1[items], object$BICM0[items])
  return(BIC)
}

#' Predicted values for an object of \code{"ddfMLR"} class.
#'
#' @description S3 method for predictions from the model used in the
#'   object of \code{"ddfMLR"} class.
#'
#' @param object an object of \code{"ddfMLR"} class.
#' @param item numeric or character: either character \code{"all"} to
#'   apply for all converged items (default), or a vector of item
#'   names (column names of \code{Data}), or item identifiers
#'   (integers specifying the column number).
#' @param match numeric: matching criterion for new observations.
#' @param group numeric: group membership for new observations.
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
#' \code{\link[difNLR]{ddfMLR}} for DDF detection among nominal data using multinomial log-linear regression model. \cr
#' \code{\link[stats]{predict}} for generic function for prediction.
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(GMATtest, GMATkey)
#' Data <- GMATtest[, 1:20] # items
#' group <- GMATtest[, "group"] # group membership variable
#' key <- GMATkey # correct answers
#'
#' # testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
#'
#' # fitted values
#' predict(x, item = 1)
#'
#' # predicted values
#' predict(x, item = 1, match = 0, group = c(0, 1))
#' predict(x, item = x$DDFitems, match = 0, group = c(0, 1))
#' }
#'
#' @export
predict.ddfMLR <- function(object, item = "all", match, group, ...) {
  m <- dim(object$Data)[2]
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

  coefs_all <- coef(object, IRTpars = FALSE, CI = 0)
  res <- list()

  for (i in items) {
    mat <- cbind("(Intercept)" = 1, "x" = match[, i], "group" = group, "x:group" = match[, i] * group)
    coefs <- coefs_all[[i]]

    if (is.null(dim(coefs))) coefs <- matrix(coefs, ncol = length(coefs))
    if (dim(coefs)[2] == 2) {
      coefs <- as.matrix(data.frame(coefs, 0, 0))
    }
    if (dim(coefs)[2] == 3) {
      coefs <- as.matrix(data.frame(coefs, 0))
    }

    cats <- c(object$key[i], rownames(coefs))

    df.probs <- as.data.frame(t(exp(coefs %*% t(mat))))
    df.probs <- sapply(df.probs, function(x) x / (rowSums(df.probs) + 1))
    df.probs <- data.frame(1 - rowSums(df.probs), df.probs)
    colnames(df.probs) <- cats
    res[[i]] <- df.probs
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
