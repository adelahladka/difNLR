#' DDF detection for nominal data.
#'
#' @aliases ddfMLR
#'
#' @description Performs DDF detection procedure for nominal data based on multinomial
#' log-linear regression model and likelihood ratio test of a submodel.
#'
#' @param Data data.frame or matrix: dataset which rows represent unscored examinee answers (nominal)
#' and columns correspond to the items. In addition, \code{Data} can hold the vector of
#' group membership.
#' @param group numeric or character: a dichotomous vector of the same length as \code{nrow(Data)}
#' or a column identifier of \code{Data}.
#' @param focal.name numeric or character: indicates the level of \code{group} which corresponds to
#' focal group.
#' @param key character: the answer key. Each element corresponds to the correct answer of one item.
#' @param type character: type of DDF to be tested. Either \code{"both"} for uniform and non-uniform
#' DDF (i.e., difference in parameters \code{"a"} and \code{"b"}) (default), or \code{"udif"} for
#' uniform DDF only (i.e., difference in difficulty parameter \code{"b"}), or \code{"nudif"} for
#' non-uniform DDF only (i.e., difference in discrimination parameter \code{"a"}). Can be specified
#' as a single value (for all items) or as an item-specific vector.
#' @param match numeric or character: matching criterion to be used as an estimate of trait. Can be
#' either \code{"zscore"} (default, standardized total score), \code{"score"} (total test score),
#' or vector of the same length as number of observations in \code{Data}.
#' @param anchor numeric or character: specification of DDF free items. Either \code{NULL} (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number) determining which items are currently considered as anchor (DDF free) items.
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
#' ddfMLR(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
#'        purify = FALSE, nrIter = 10, p.adjust.method = "none", parametrization = "irt",
#'        alpha = 0.05)
#'
#' @details
#' Performs DDF detection procedure for nominal data based on multinomial
#' log-linear regression model and likelihood ratio test of submodel. Probability of selection the
#' \eqn{k}-th category (distractor) is
#' \deqn{P(y = k) = exp((a_k + a_kDif*g)*(x - b_k - b_kDif*g)))/(1 + \sum exp((a_l + a_lDif*g)*(x - b_l - b_lDif*g))), }
#' where \eqn{x} is by default standardized total score (also called Z-score) and \eqn{g} is a group
#' membership. Parameters \eqn{a_k} and \eqn{b_k} are discrimination and difficulty for the \eqn{k}-th
#' category. Terms \eqn{a_kDif} and \eqn{b_kDif} then represent differences between two groups
#' (reference and focal) in relevant parameters. Probability of correct answer (specified in argument
#' \code{key}) is
#' \deqn{P(y = k) = 1/(1 + \sum exp((a_l + a_lDif*g)*(x - b_l - b_lDif*g))). }
#' Parameters are estimated via neural networks. For more details see \code{\link[nnet]{multinom}}.
#'
#' Argument \code{parametrization} is a character which specifies parametrization of regression parameters.
#' Default option is \code{"irt"} which returns IRT parametrization (difficulty-discrimination, see above).
#' Option \code{"classic"} returns intercept-slope parametrization with effect of group membership and
#' interaction with matching criterion, i.e. \eqn{b_0k + b_1k*x + b_2k*g + b_3k*x*g} instead of
#' \eqn{(a_k + a_kDif*g)*(x - b_k - b_kDif*g))}.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as \code{NA}
#' for both, \code{Data} and \code{group} arguments.
#'
#' @return The \code{ddfMLR()} function returns an object of class \code{"ddfMLR"}. The output
#' including values of the test statistics, p-values, and items marked as DDF is displayed by the
#' \code{print()} method.
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
#' @seealso
#' \code{\link[difNLR]{plot.ddfMLR}} for graphical representation of item characteristic curves. \cr
#' \code{\link[difNLR]{coef.ddfMLR}} for extraction of item parameters with their standard errors. \cr
#' \code{\link[difNLR]{logLik.ddfMLR}}, \code{\link[difNLR]{AIC.ddfMLR}}, \code{\link[difNLR]{BIC.ddfMLR}}
#' for extraction of loglikelihood and information criteria. \cr
#'
#' \code{\link[stats]{p.adjust}} for multiple comparison corrections. \cr
#' \code{\link[nnet]{multinom}} for estimation function using neural networks.
#'
#' @examples
#' # Loading data based on GMAT
#' data(GMATtest, GMATkey)
#'
#' Data <- GMATtest[, 1:20]
#' group <- GMATtest[, "group"]
#' key <- GMATkey
#'
#' # Testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
#'
#' \dontrun{
#' # Graphical devices
#' plot(x, item = "Item1", group.names = c("Group 1", "Group 2"))
#' plot(x, item = x$DDFitems)
#' plot(x, item = 1)
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
#' # Estimates
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
#'
#' # Testing both DDF effects with Benjamini-Hochberg adjustment method
#' ddfMLR(Data, group, focal.name = 1, key, p.adjust.method = "BH")
#'
#' # Testing both DDF effects with item purification
#' ddfMLR(Data, group, focal.name = 1, key, purify = TRUE)
#'
#' # Testing uniform DDF effects
#' ddfMLR(Data, group, focal.name = 1, key, type = "udif")
#' # Testing non-uniform DDF effects
#' ddfMLR(Data, group, focal.name = 1, key, type = "nudif")
#'
#' # Testing both DDF effects with total score as matching criterion
#' ddfMLR(Data, group, focal.name = 1, key, match = "score")
#'
#' # Testing both DDF effects using classic parametrization
#' ddfMLR(Data, group, focal.name = 1, key, parametrization = "classic")
#' }
#'
#' @keywords DDF
#' @export
ddfMLR <- function(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
                   purify = FALSE, nrIter = 10, p.adjust.method = "none", parametrization = "irt",
                   alpha = 0.05) {
  if (!type %in% c("udif", "nudif", "both") | !is.character(type)) {
    stop("'type' must be either 'udif', 'nudif', or 'both'.",
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
      stop("Invalid value for 'match'. Possible values are 'zscore', 'score', or vector of the same length as number
           of observations in 'Data'.")
    }
  }
  ### purification
  if (purify & !(match[1] %in% c("score", "zscore"))) {
    stop("Purification not allowed when matching variable is not 'zscore' or 'score'.", call. = FALSE)
  }

  internalMLR <- function() {
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
      stop("'Data' must be data frame or matrix of binary vectors.",
        call. = FALSE
      )
    }
    if (length(key) != dim(DATA)[2]) {
      stop("Number of items in 'Data' is not equal to the length of 'key'.",
        call. = FALSE
      )
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
      PROV <- suppressWarnings(MLR(DATA, GROUP,
        key = key, match = match, anchor = ANCHOR,
        type = type, p.adjust.method = p.adjust.method, parametrization = parametrization,
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
        llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
        AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
        BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
        DDFitems = DDFitems,
        type = type, parametrization = parametrization,
        purification = purify,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, key = key, match = match
      )
    } else {
      nrPur <- 0
      ddfPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(MLR(DATA, GROUP,
        key = key, type = type,
        p.adjust.method = p.adjust.method, parametrization = parametrization,
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
            prov2 <- suppressWarnings(MLR(DATA, GROUP,
              key = key, anchor = nodif, type = type,
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
      RES <- list(
        Sval = STATS,
        mlrPAR = mlrPAR,
        mlrSE = mlrSE,
        parM0 = PROV$par.m0, parM1 = PROV$par.m1,
        llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
        AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
        BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
        DDFitems = DDFitems,
        type = type, parametrization = parametrization,
        purification = purify, nrPur = nrPur, ddfPur = ddfPur, conv.puri = noLoop,
        p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
        alpha = alpha,
        Data = DATA, group = GROUP, group.names = group.names, key = key, match = match
      )
    }
    class(RES) <- "ddfMLR"
    return(RES)
  }
  resToReturn <- internalMLR()
  return(resToReturn)
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

  if (is.character(x$DDFitems)) {
    switch(x$type, both = cat("\nNone of items is detected as DDF"),
      udif = cat("\nNone of items is detected as uniform DDF"),
      nudif = cat("\nNone of items is detected as non-uniform DDF")
    )
  }
  else {
    switch(x$type, both = cat("\nItems detected as DDF items:"),
      udif = cat("\nItems detected as uniform DDF items:"),
      nudif = cat("\nItems detected as non-uniform DDF items:")
    )
    cat("\n", paste(colnames(x$Data)[x$DDFitems], "\n", sep = ""))
  }
}

#' ICC plots for an object of \code{"ddfMLR"} class.
#'
#' @description Plot method for an object of \code{"ddfMLR"} class using \pkg{ggplot2}.
#'
#' The characteristic curves for an item specified in \code{item} argument are plotted.
#' Plotted curves represent the best model.
#'
#' @param x an object of \code{"ddfMLR"} class.
#' @param item numeric or character: either character \code{"all"} to apply for all items (default),
#' or a vector of item names (column names of \code{Data}), or item identifiers (integers specifying
#' the column number).
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
#' \code{\link[difNLR]{ddfMLR}} for DDF detection. \cr
#' \code{\link[ggplot2]{ggplot}} for general function to plot a \code{"ggplot"} object.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMATtest, GMATkey)
#'
#' Data <- GMATtest[, 1:20]
#' group <- GMATtest[, "group"]
#' key <- GMATkey
#'
#' # Testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
#'
#' # Graphical devices
#' plot(x, item = "Item1", group.names = c("Group 1", "Group 2"))
#' plot(x, item = x$DDFitems)
#' plot(x, item = 1)
#' }
#' @export
plot.ddfMLR <- function(x, item = "all", title, group.names, ...) {
  m <- length(x$mlrPAR)
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
    anchor <- c(1:m)[!c(1:m) %in% x$DDFitems]
  } else {
    anchor <- 1:m
  }

  if (x$match[1] == "zscore") {
    score <- c(scale(unlist(CTT::score(as.data.frame(x$Data[, anchor]), x$key))))
    xlab <- "Standardized total score"
  } else {
    if (x$match[1] == "score") {
      score <- c(unlist(CTT::score(as.data.frame(x$Data[, anchor]), x$key)))
      xlab <- "Total score"
    } else {
      if (length(x$match) == dim(x$Data)[1]) {
        score <- x$match
        xlab <- "Matching criterion"
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  sq <- seq(min(score, na.rm = T), max(score, na.rm = T), length.out = 300)
  sqR <- as.matrix(data.frame(1, sq, 0, 0))
  sqF <- as.matrix(data.frame(1, sq, 1, sq))

  plot_CC <- vector("list", length = length(items))
  for (j in 1:length(items)) {
    i <- items[[j]]
    if (!missing(title)) {
      TITLE <- title
    } else {
      TITLE <- colnames(x$Data)[i]
    }

    coefs <- x$mlrPAR[[i]]
    if (x$parametrization == "irt") {
      a <- coefs[, "a"]
      b <- coefs[, "b"]
      aDIF <- coefs[, "aDIF"]
      bDIF <- coefs[, "bDIF"]
      coefs <- cbind(-a * b, a, -a * bDIF - aDIF * b - aDIF * bDIF, aDIF)
    }

    if (is.null(dim(coefs))) coefs <- matrix(coefs, ncol = length(coefs))
    if (dim(coefs)[2] == 2) {
      coefs <- as.matrix(data.frame(coefs, 0, 0))
    }
    if (dim(coefs)[2] == 3) {
      coefs <- as.matrix(data.frame(coefs, 0))
    }

    prR <- as.data.frame(t(exp(coefs %*% t(sqR))))
    prF <- as.data.frame(t(exp(coefs %*% t(sqF))))
    colnames(prR) <- colnames(prF) <- rownames(coefs)

    prR <- sapply(prR, function(x) x / (rowSums(prR) + 1))
    prF <- sapply(prF, function(x) x / (rowSums(prF) + 1))

    hvR <- data.frame(1 - rowSums(prR), prR, "R", sq)
    hvF <- data.frame(1 - rowSums(prF), prF, "F", sq)

    if (is.null(rownames(coefs)) | nrow(coefs) == 1) {
      if (is.null(levels(x$Data[, i]))) {
        lvls <- sort(unique(x$Data[, i]))
      } else {
        lvls <- levels(x$Data[, i])
      }
      nams <- lvls[lvls != x$key[i]]
    } else {
      nams <- rownames(coefs)
    }

    colnames(hvR) <- colnames(hvF) <- c(paste(x$key[i]), nams, "group", "score")
    hv <- rbind(hvR, hvF)

    df <- reshape2::melt(hv, id = c("score", "group"))
    df$group <- as.factor(df$group)

    df2 <- rbind(
      data.frame(prop.table(table(x$Data[x$group == 1, i], score[x$group == 1]), 2),
        table(x$Data[x$group == 1, i], score[x$group == 1]),
        group = "1"
      ),
      data.frame(prop.table(table(x$Data[x$group == 0, i], score[x$group == 0]), 2),
        table(x$Data[x$group == 0, i], score[x$group == 0]),
        group = "0"
      )
    )

    df2$score <- as.numeric(levels(df2$Var2))[df2$Var2]
    df2$answ <- relevel(df2$Var1, ref = paste(x$key[i]))
    df2$group <- as.factor(df2$group)

    df$variable <- relevel(df$variable, ref = paste(x$key[i]))

    levels(df$variable) <- paste0("P(Y = ", levels(df$variable), ")")
    levels(df2$answ) <- paste0("P(Y = ", levels(df2$answ), ")")

    cbPalette <- c("#ffbe33", "#34a4e5", "#ce7eaa", "#00805e", "#737373", "#f4eb71", "#0072B2", "#D55E00")
    num.col <- ceiling(length(levels(df$variable)) / 8)
    cols <- rep(cbPalette, num.col)[1:length(levels(df$variable))]

    plot_CC[[j]] <- ggplot() +
      geom_line(
        data = df,
        aes_string(
          x = "score", y = "value",
          colour = "variable", linetype = "group"
        ),
        size = 0.8
      ) +
      geom_point(
        data = df2,
        aes_string(
          x = "score", y = "Freq",
          colour = "answ", fill = "answ",
          size = "Freq.1"
        ),
        alpha = 0.5, shape = 21
      ) +

      ylim(0, 1) +
      ggtitle(TITLE) +
      labs(
        x = xlab,
        y = "Probability of answer"
      ) +
      scale_linetype_manual(
        breaks = c("R", "F"), labels = group.names,
        values = c("solid", "dashed")
      ) +
      scale_color_manual(values = cols) +
      scale_fill_manual(values = cols) +
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
        colour = guide_legend(title = "Answer", order = 2),
        fill = guide_legend(title = "Answer", order = 2),
        linetype = guide_legend(title = "Group", order = 3)
      )
  }
  plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
  return(plot_CC)
}

#' Extract model coefficients from an object of \code{"ddfMLR"} class.
#'
#' @description S3 method for extracting estimated model coefficients from an object of \code{"ddfMLR"} class.
#' @aliases coefficients.ddfMLR
#'
#' @param object an object of \code{"ddfMLR"} class.
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
#' \code{\link[difNLR]{ddfMLR}} for DDF detection among nominal data. \cr
#' \code{\link[stats]{coef}} for generic function extracting model coefficients.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMATtest, GMATkey)
#'
#' Data <- GMATtest[, 1:20]
#' group <- GMATtest[, "group"]
#' key <- GMATkey
#'
#' # Testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
#'
#' # Estimated parameters
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, simplify = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
#' }
#' @export
coef.ddfMLR <- function(object, SE = FALSE, simplify = FALSE, ...) {
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

  coefs <- object$mlrPAR
  names(coefs) <- nams

  if (SE) {
    se <- object$mlrSE
    names(se) <- nams

    coefs <- lapply(nams, function(i) rbind(coefs[[i]], se[[i]])[order(c(rownames(coefs[[i]]), rownames(se[[i]]))), ])
    rownams <- lapply(coefs, function(x) paste(rownames(x), c("estimate", "SE")))
    coefs <- lapply(coefs, function(x) {
      i <- which(coefs %in% list(x))
      rownames(x) <- rownams[[i]]
      return(x)
    })
  }

  if (simplify) {
    catnams <- unlist(lapply(coefs, rownames))
    resnams <- unlist(lapply(1:m, function(i) rep(nams[i], sapply(coefs, nrow)[i])))
    resnams <- paste(resnams, catnams)
    res <- as.data.frame(plyr::ldply(coefs, rbind))
    rownames(res) <- resnams
    res[is.na(res)] <- 0
    if (!SE) res <- res[, -1]
  } else {
    res <- coefs
  }

  return(res)
}

#' Loglikelihood and information criteria for an object of \code{"ddfMLR"} class.
#'
#' @aliases AIC.ddfMLR BIC.ddfMLR
#' @rdname logLik.ddfMLR
#'
#' @description S3 methods for extracting loglikelihood, Akaike's information criterion (AIC) and
#' Schwarz's Bayesian criterion (BIC) for an object of \code{"ddfMLR"} class.
#'
#' @param object an object of \code{"ddfMLR"} class.
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
#' \code{\link[difNLR]{ddfMLR}} for DDF detection among nominal data. \cr
#' \code{\link[stats]{logLik}} for generic function extracting loglikelihood. \cr
#' \code{\link[stats]{AIC}} for generic function calculating AIC and BIC.
#'
#' @examples
#' \dontrun{
#' # Loading data based on GMAT
#' data(GMATtest, GMATkey)
#'
#' Data <- GMATtest[, 1:20]
#' group <- GMATtest[, "group"]
#' key <- GMATkey
#'
#' # Testing both DDF effects
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

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
        call. = FALSE
      )
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
        call. = FALSE
      )
    }
  }
  if (class(item) == "numeric" & !all(item %in% 1:m)) {
    stop("Invalid number for 'item'.",
      call. = FALSE
    )
  }
  if (class(item) == "integer" & !all(item %in% 1:m)) {
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
      call. = FALSE
    )
  }
  if (class(item) == "character") {
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    items <- item
  }

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

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
        call. = FALSE
      )
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
        call. = FALSE
      )
    }
  }
  if (class(item) == "numeric" & !all(item %in% 1:m)) {
    stop("Invalid number for 'item'.",
      call. = FALSE
    )
  }
  if (class(item) == "integer" & !all(item %in% 1:m)) {
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
      call. = FALSE
    )
  }
  if (class(item) == "character") {
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    items <- item
  }

  AIC <- ifelse(items %in% object$DDFitems, object$AICM1[items], object$AICM0[items])
  return(AIC)
}

#' @rdname logLik.ddfMLR
#' @aliases AIC.ddfMLR logLik.ddfMLR
#' @export
BIC.ddfMLR <- function(object, item = "all", ...) {
  m <- length(object$mlrPAR)
  nams <- colnames(object$Data)

  if (class(item) == "character") {
    if (item != "all" & !item %in% nams) {
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
        call. = FALSE
      )
    }
  } else {
    if (class(item) != "integer" & class(item) != "numeric") {
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
        call. = FALSE
      )
    }
  }
  if (class(item) == "numeric" & !all(item %in% 1:m)) {
    stop("Invalid number for 'item'.",
      call. = FALSE
    )
  }
  if (class(item) == "integer" & !all(item %in% 1:m)) {
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
      call. = FALSE
    )
  }
  if (class(item) == "character") {
    if (item[1] == "all") {
      items <- 1:m
    } else {
      items <- which(nams %in% item)
    }
  } else {
    items <- item
  }

  BIC <- ifelse(items %in% object$DDFitems, object$BICM1[items], object$BICM0[items])
  return(BIC)
}
