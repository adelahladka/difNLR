#' Performs DDF detection using Multinomial Log-linear Regression model.
#'
#' @aliases ddfMLR print.ddfMLR plot.ddfMR
#'
#' @description Performs DDF detection procedure based on Multinomial Log-linear Regression model and
#' likelihood ratio test of submodel.
#'
#' @param Data character: either the unscored data matrix only, or the unscored data
#' matrix plus the vector of group. See \strong{Details}.
#' @param group numeric or character: either the binary vector of group membership or
#' the column indicator of group membership. See \strong{Details}.
#' @param focal.name numeric or character: indicates the level of \code{group} which corresponds to
#' focal group
#' @param key character: the answer key. See \strong{Details}.
#' @param type character: type of DDF to be tested (either "both" (default), "udif", or "nudif").
#' See \strong{Details}.
#' @param match specifies matching criterion. Can be either \code{"zscore"} (default, standardized total score),
#' \code{"score"} (total test score), or vector of the same length as number of observations in "Data". See \strong{Details}.
#' @param anchor Either \code{NULL} (default) or a vector of item names or item identifiers specifying which items are
#' currently considered as anchor (DIF free) items. Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' @param purify logical: should the item purification be applied? (default is \code{FALSE}). See \strong{Details}.
#' @param nrIter numeric: the maximal number of iterations in the item purification (default is 10).
#' @param p.adjust.method character: method for multiple comparison correction.
#' See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#' @param x an object of 'ddfMLR' class
#' @param object an object of 'ddfMLR' class
#' @param item either character ("all"), or numeric vector, or single number
#'corresponding to column indicators. See \strong{Details}.
#' @param title string: title of plot.
#' @param ... other generic parameters for \code{print} or \code{plot} functions.
#'
#' @usage ddfMLR(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
#' purify = FALSE, nrIter = 10, alpha = 0.05, p.adjust.method = "none")
#'
#' @details
#' DDF detection procedure based on Multinomial Log-linear model.
#'
#' The \code{Data} is a matrix whose rows represents examinee unscored answers and
#' columns correspond to the items. The \code{group} must be either a vector of the same
#' length as \code{nrow(data)} or column indicator of \code{Data}. The \code{key} must be
#' a vector of correct answers corresponding to columns of \code{Data}.
#'
#' The \code{type} corresponds to type of DDF to be tested. Possible values are \code{"both"}
#' to detect any DDF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DDF or
#' \code{"nudif"} to detect only non-uniform DDF.
#'
#' Argument \code{match} represents the matching criterion. It can be either the standardized test score (default, \code{"zscore"}),
#' total test score (\code{"score"}), or any other continuous or discrete variable of the same length as number of observations
#' in \code{Data}. Matching criterion is used in \code{MLR()} function as a covariate in multinomial model.
#'
#' A set of anchor items (DIF free) can be specified through the \code{anchor} argument. It need to be a vector of either
#' item names (as specified in column names of \code{Data}) or item identifiers (integers specifying the column number).
#' In case anchor items are provided, only these items are used to compute matching criterion \code{match}. If the \code{match}
#' argument is not either \code{"zscore"} or \code{"score"}, \code{anchor} argument is ignored.  When anchor items are
#' provided, purification is not applied.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the
#' \code{stats} package. Possible values are \code{"holm"}, \code{"hochberg"},
#' \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}.
#'
#' The output of the ddfMLR is displayed by the \code{print.ddfMLR} function.
#'
#' The characteristic curve for item specified in \code{item} option can be plotted. For default
#' option \code{"all"} of item, characteristic curves of all converged items are plotted.
#' The drawn curves represent best model.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as \code{NA}
#' for both, \code{data} and \code{group} parameters.
#'
#' @return A list of class 'ddfMLR' with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{mlrPAR}}{the estimates of final model.}
#'   \item{\code{mlrSE}}{standard errors of the estimates of final model.}
#'   \item{\code{parM0}}{the estimates of null model.}
#'   \item{\code{parM1}}{the estimates of alternative model.}
#'   \item{\code{alpha}}{numeric: significance level.}
#'   \item{\code{DDFitems}}{either the column indicators of the items which were detected as DDF, or \code{"No DDF item detected"}.}
#'   \item{\code{type}}{character: type of DIF that was tested.}
#'   \item{\code{purification}}{\code{purify} value.}
#'   \item{\code{nrPur}}{number of iterations in item purification process. Returned only if \code{purify}
#'   is \code{TRUE}.}
#'   \item{\code{difPur}}{a binary matrix with one row per iteration of item purification and one column per item.
#'   "1" in i-th row and j-th column means that j-th item was identified as DIF in i-1-th iteration. Returned only
#'   if \code{purify} is \code{TRUE}.}
#'   \item{\code{conv.puri}}{logical indicating whether item purification process converged before the maximal number
#'   \code{nrIter} of iterations. Returned only if \code{purify} is \code{TRUE}.}
#'   \item{\code{p.adjust.method}}{character: method for multiple comparison correction which was applied.}
#'   \item{\code{pval}}{the p-values by likelihood ratio test.}
#'   \item{\code{adj.pval}}{the adjusted p-values by likelihood ratio test using \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of likelihood ratio test.}
#'   \item{\code{group}}{the vector of group membership.}
#'   \item{\code{Data}}{the data matrix.}
#'   \item{\code{match}}{matching criterion.}
#'   \item{\code{llM0}}{log-likelihood of null model.}
#'   \item{\code{llM1}}{log-likelihood of alternative model.}
#'   \item{\code{AICM0}}{AIC of null model.}
#'   \item{\code{AICM1}}{AIC of alternative model.}
#'   \item{\code{BICM0}}{BIC of null model.}
#'   \item{\code{BICM1}}{BIC of alternative model.}
#'   }
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' drabinova@cs.cas.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @examples
#' \dontrun{
#' # loading data based on GMAT
#' data(GMATtest, GMATkey)
#'
#' Data  <- GMATtest[, 1:20]
#' group <- GMATtest[, "group"]
#' key <- GMATkey
#'
#' # Testing both DDF effects
#' (x <- ddfMLR(Data, group, focal.name = 1, key))
#'
#' # Testing both DDF effects with Benjamini-Hochberg adjustment method
#' ddfMLR(Data, group, focal.name = 1, key, p.adjust.method = "BH")
#'
#' # Testing both DDF effects with item purification
#' ddfMLR(Data, group, focal.name = 1, key, purify = T)
#'
#' # Testing uniform DDF effects
#' ddfMLR(Data, group, focal.name = 1, key, type = "udif")
#' # Testing non-uniform DDF effects
#' ddfMLR(Data, group, focal.name = 1, key, type = "nudif")
#'
#' # Testing both DDF effects with total score as matching criterion
#' ddfMLR(Data, group, focal.name = 1, key, match = "score")
#'
#' # Graphical devices
#' plot(x, item = 1)
#' plot(x, item = x$DDFitems)
#' plot(x, item = "all")
#'
#' # AIC, BIC, logLik
#' AIC(x)
#' BIC(x)
#' logLik(x)
#' }
#' @keywords DDF
#' @export
#' @importFrom reshape2 melt
#' @importFrom stats relevel

ddfMLR <- function(Data, group, focal.name, key, type = "both",
                   match = "zscore", anchor = NULL, purify = FALSE,
                   nrIter = 10, alpha = 0.05, p.adjust.method = "none")
{
  if (!type %in% c("udif", "nudif", "both") | !is.character(type))
    stop("'type' must be either 'udif', 'nudif' or 'both'",
         call. = FALSE)
  if (alpha > 1 | alpha < 0)
    stop("'alpha' must be between 0 and 1",
         call. = FALSE)
  ### matching criterion
  if (!(match[1] %in% c("score", "zscore"))){
    if (is.null(dim(Data))){
      no <- length(Data)
    } else {
      no <- dim(Data)[1]
    }
    if (length(match) != no){
      stop("Invalid value for 'match'. Possible values are 'zscore', 'score' or vector of the same length as number
           of observations in 'Data'!")
    }
  }
  ### purification
  if (purify & !(match[1] %in% c("score", "zscore")))
    stop("Purification not allowed when matching variable is not 'zscore' or 'score'",  call. = FALSE)

  internalMLR <- function() {
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
      if (nrow(DATA) != length(GROUP))
        stop("'Data' must have the same number of rows as is length of vector 'group'",
             call. = FALSE)
    } else {
      stop("'Data' must be data frame or matrix of binary vectors",
           call. = FALSE)
    }
    if (length(key) != ncol(DATA)){
      stop("Number of items in 'Data' is not equal to the length of 'key'.",
           call. = FALSE)
    }

    GROUP <- as.numeric(as.factor(GROUP) == focal.name)

    if (length(match) == dim(DATA)[1]){
      df <- data.frame(DATA, GROUP, match, check.names = F)
    } else {
      df <- data.frame(DATA, GROUP, check.names = F)
    }

    df <- df[complete.cases(df), ]

    if (dim(df)[1] == 0){
      stop("It seems that your 'Data' does not include any subjects that are complete. ",
           call. = FALSE)
    }

    GROUP <- df[, "GROUP"]
    DATA <- data.frame(df[, !(colnames(df) %in% c("GROUP", "match"))])

    if (length(match) > 1){
      match <- df[, "match"]
    }

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
    if (!purify | !(match[1] %in% c("zscore", "score")) | !is.null(anchor)) {
      PROV <- suppressWarnings(MLR(DATA, GROUP, key = key, match = match, anchor = ANCHOR,
                                   type = type, p.adjust.method = p.adjust.method, alpha = alpha))

      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      se.m1 <- lapply(lapply(PROV$cov.m1, diag), sqrt)
      se.m0 <- lapply(lapply(PROV$cov.m0, diag), sqrt)
      significant <- which(ADJ.PVAL < alpha)

      if (length(significant) > 0) {
        DDFitems <- significant
        mlrPAR <- PROV$par.m1
        mlrSE <- se.m1
        for (idif in 1:length(DDFitems)) {
          mlrPAR[[DDFitems[idif]]] <- PROV$par.m0[[DDFitems[idif]]]
          mlrSE[[DDFitems[idif]]] <- se.m0[[DDFitems[idif]]]
        }
      } else {
        DDFitems <- "No DDF item detected"
        mlrPAR <- PROV$par.m1
        mlrSE <- se.m1
        }

      RES <- list(Sval = STATS,
                  mlrPAR = mlrPAR,
                  mlrSE = mlrSE,
                  parM0 = PROV$par.m0,
                  # seM0 = PROV$se.m0, covM0 = PROV$cov.m0,
                  parM1 = PROV$par.m1,
                  # seM1 = PROV$se.m1, covM1 = PROV$cov.m1,
                  alpha = alpha, DDFitems = DDFitems,
                  type = type, purification = purify, p.adjust.method = p.adjust.method,
                  pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
                  group = GROUP, Data = DATA, key = key, match = match,
                  llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
                  AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
                  BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1)
    } else {
      nrPur <- 0
      difPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(MLR(DATA, GROUP, key = key, type = type,
                                    p.adjust.method = p.adjust.method, alpha = alpha))
      stats1 <- prov1$Sval
      pval1 <- prov1$pval
      significant1 <- which(pval1 < alpha)
      if (length(significant1) == 0) {
        PROV <- prov1
        STATS <- stats1
        DDFitems <- "No DIF item detected"
        se.m1 <- lapply(lapply(PROV$cov.m1, diag), sqrt)
        se.m0 <- lapply(lapply(PROV$cov.m0, diag), sqrt)
        mlrPAR <- PROV$par.m1
        mlrSE <- se.m1
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
            prov2 <- suppressWarnings(MLR(DATA, GROUP, key = key, anchor = nodif, type = type,
                                          p.adjust.method = p.adjust.method, alpha = alpha))
            stats2 <- prov2$Sval
            pval2 <- prov2$pval
            significant2 <- which(pval2 < alpha)
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
        significant1 <- which(PROV$adjusted.pval < alpha)
        se.m1 <- lapply(lapply(PROV$cov.m1, diag), sqrt)
        se.m0 <- lapply(lapply(PROV$cov.m0, diag), sqrt)
        mlrPAR <- PROV$par.m1
        mlrSE <- se.m1
        if (length(significant1) > 0) {
          DDFitems <- significant1
          for (idif in 1:length(DDFitems)) {
            mlrPAR[[DDFitems[idif]]] <- PROV$par.m0[[DDFitems[idif]]]
            mlrSE[[DDFitems[idif]]] <- se.m0[[DDFitems[idif]]]
          }
        } else {
          DDFitems <- "No DIF item detected"
        }
      }
      if (!is.null(difPur)) {
        rownames(difPur) <- paste("Step", 0:(nrow(difPur) - 1), sep = "")
        colnames(difPur) <- colnames(DATA)
      }
      RES <- list(Sval = STATS,
                  mlrPAR = mlrPAR,
                  mlrSE = mlrSE,
                  parM0 = PROV$par.m0,
                  # seM0 = PROV$se.m0, covM0 = PROV$cov.m0,
                  parM1 = PROV$par.m1,
                  # seM1 = PROV$se.m1, covM1 = PROV$cov.m1,
                  alpha = alpha, DDFitems = DDFitems,
                  type = type, purification = purify, nrPur = nrPur, difPur = difPur, conv.puri = noLoop,
                  p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
                  group = GROUP, Data = DATA, key = key, match = match,
                  llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
                  AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
                  BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1)
    }
    class(RES) <- "ddfMLR"
    return(RES)
  }
  resToReturn <- internalMLR()
  return(resToReturn)
}


#' @rdname ddfMLR
#' @export
print.ddfMLR <- function (x, ...){
  title <- switch(x$type,
                  both = "Detection of both types of Differential Distractor Functioning using Multinomial Log-linear Regression model",
                  udif = "Detection of uniform Differential Distractor Functioning using Multinomial Log-linear Regression model",
                  nudif = "Detection of non-uniform Differential Distractor Functioning using Multinomial Log-linear Regression model")
  cat(paste(strwrap(title, width = 60), collapse = "\n"))
  cat("\n\nLikelihood-ratio chi-square statistics\n")
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
  }
  else {
    cat(paste("Multiple comparisons made with",
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
    colnames(tab) <- c("Chisq-value", "P-value", "")
  } else {
    tab <- format(round(cbind(x$Sval, x$pval, x$adj.pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 4)
    colnames(tab) <- c("Chisq-value", "P-value", "Adj. P-value", "")
  }

  rownames(tab) <- colnames(x$Data)

  print(tab, quote = F, digits = 4, zero.print = F)
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  # critical <- ifelse(x$test == "F", qf(1 - x$alpha, x$df[1], x$df[2]), qchisq(1 - x$alpha, x$df))
  # cat(paste("\nDetection thresholds: ", round(critical, 4), " (significance level: ", x$alpha, ")", sep = ""))
  if (is.character(x$DDFitems)) {
    switch(x$type, both = cat("\nNone of items is detected as DDF"),
           udif = cat("\nNone of items is detected as uniform DDF"),
           nudif = cat("\nNone of items is detected as non-uniform DDF"))
  }
  else {
    switch(x$type, both = cat("\n\nItems detected as DDF items:"),
           udif = cat("\n\nItems detected as uniform DDF items:"),
           nudif = cat("\n\nItems detected as non-uniform DDF items:"))
    cat("\n", paste(colnames(x$Data)[x$DDFitems], "\n", sep = ""))
  }
}

#' @rdname ddfMLR
#' @export
plot.ddfMLR <- function(x, item = "all", title, ...){
  m <- length(x$mlrPAR)
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

  if (x$match[1] == "zscore"){
    score <- c(scale(unlist(CTT::score(as.data.frame(x$Data), x$key))))
  } else {
    if (x$match[1] == "score"){
      score <- c(unlist(CTT::score(as.data.frame(x$Data), x$key)))
    } else {
      if (length(x$match) == dim(x$Data)[1]){
        score <- x$match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  sq <- seq(min(score, na.rm = T), max(score, na.rm = T), by = 0.1)
  sqR <- as.matrix(data.frame(1, sq, 0, 0))
  sqF <- as.matrix(data.frame(1, sq, 1, sq))

  ### Data
  if (length(x$match) > 1){
    xlab <- "Matching criterion"
  } else {
    if (x$match == "score"){
      xlab <- "Total score"
    } else {
      xlab <- "Standardized total score"
    }
  }

  plot_CC <- list()
  for (i in items){

    if (!missing(title)){
      TITLE <- title
    } else {
      TITLE <- colnames(x$Data)[i]
    }

    if(ncol(x$mlrPAR[[i]]) == 2)
      x$mlrPAR[[i]] <- as.matrix(data.frame(x$mlrPAR[[i]], 0, 0))
    if(ncol(x$mlrPAR[[i]]) == 3)
      x$mlrPAR[[i]] <- as.matrix(data.frame(x$mlrPAR[[i]], 0))

    prR <- prF <- c()

    for (j in 1:nrow(x$mlrPAR[[i]])){
      prR <- rbind(prR, exp(x$mlrPAR[[i]][j, ] %*% t(sqR)))
      prF <- rbind(prF, exp(x$mlrPAR[[i]][j, ] %*% t(sqF)))
    }

    prR <- as.data.frame(t(prR)); prF <- as.data.frame(t(prF))
    colnames(prR) <- colnames(prF) <- rownames(x$mlrPAR[[i]])
    prR$sum <- apply(prR, 1, sum) + 1; prF$sum <- apply(prF, 1, sum) + 1

    for (j in 1:nrow(x$mlrPAR[[i]])){
      prR <- data.frame(prR, prR[, j]/prR[, "sum"])
      prF <- data.frame(prF, prF[, j]/prF[, "sum"])
    }

    hvR <- data.frame(1 - apply(prR[, (nrow(x$mlrPAR[[i]])+2):ncol(prR)], 1, sum),
                      prR[, (nrow(x$mlrPAR[[i]])+2):ncol(prR)], "R")
    hvF <- data.frame(1 - apply(prF[, (nrow(x$mlrPAR[[i]])+2):ncol(prF)], 1, sum),
                      prF[, (nrow(x$mlrPAR[[i]])+2):ncol(prF)], "F")
    hvR <- data.frame(hvR, score = sq)
    hvF <- data.frame(hvF, score = sq)
    colnames(hvR) <- colnames(hvF) <- c(paste(x$key[i]), rownames(x$mlrPAR[[i]]), "group", "score")
    hv <- rbind(hvR, hvF)

    df <- reshape2::melt(hv, id = c("score", "group"))
    df$group <- as.factor(df$group)

    df2 <- rbind(data.frame(prop.table(table(x$Data[x$group == 1, i], score[x$group == 1]), 2),
                            table(x$Data[x$group == 1, i], score[x$group == 1]),
                            group = "1"),
                 data.frame(prop.table(table(x$Data[x$group == 0, i], score[x$group == 0]), 2),
                            table(x$Data[x$group == 0, i], score[x$group == 0]),
                            group = "0"))

    df2$score <- as.numeric(levels(df2$Var2))[df2$Var2]
    df2$answ <- relevel(df2$Var1, ref = paste(x$key[i]))
    df2$group <- as.factor(df2$group)

    df$variable <- relevel(df$variable, ref = paste(x$key[i]))

    plot_CC[[i]] <-  ggplot() +
      geom_line(data = df,
                aes_string(x = "score" , y = "value",
                    colour = "variable", linetype = "group")) +
      geom_point(data = df2,
                 aes_string(x = "score", y = "Freq",
                     colour = "answ", fill = "answ",
                     size = "Freq.1"),
                 alpha = 0.5, shape = 21) +

      ylim(0, 1) +
      ggtitle(TITLE) +
      labs(x = xlab,
           y = "Probability of answer") +
      scale_linetype_discrete(name = "Group", labels = c("Reference", "Focal")) +
      scale_size_continuous(name = "Counts")  +
      scale_colour_discrete(name = "Answer", breaks = df2$answ) +
      scale_fill_discrete(name = "Answer", breaks = df2$answ) +
      guides(colour = guide_legend(title = "Answer", order = 1)) +
      guides(fill = guide_legend(title = "Answer", order = 1)) +
      guides(size = guide_legend(title = "Counts", order = 2)) +
      guides(linetype = guide_legend(title = "Group", order = 3)) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", vjust = 1.5),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "transparent", colour = NA)) +
      ### legend
      theme(legend.box.just = "top",
            legend.justification = c("left", "top"),
            legend.position = c(0, 1),
            legend.box = "horizontal",
            legend.box.margin = margin(3, 3, 3, 3))

  }
  plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
  return(plot_CC)
}

#' @rdname ddfMLR
#' @export
coef.ddfMLR <- function(object, ...){
  return(object$mlrPAR)
}

#' @rdname ddfMLR
#' @export
logLik.ddfMLR <- function(object, item = "all", ...){
  m <- length(object$mlrPAR)
  if (class(item) == "character"){
    if (item != "all")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("Invalid number for 'item'.",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
         call. = FALSE)
  if (class(item) == "character"){
    items <- 1:m
  } else {
    items <- item
  }
  val <- ifelse(items %in% object$DDFitems,
                object$llM0[[items]],
                object$llM1[[items]])
  df <- ifelse(items %in% object$DDFitems,
               sapply(object$parM0, length)[items],
               sapply(object$parM1, length)[items])
  if (length(items) == 1){
    attr(val, "df") <- df
    class(val) <- "logLik"
  }
  return(val)
}

#' @rdname ddfMLR
#' @export
AIC.ddfMLR <- function(object, item = "all", ...){
  m <- length(object$mlrPAR)
  if (class(item) == "character"){
    if (item != "all")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("Invalid number for 'item'.",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
         call. = FALSE)
  if (class(item) == "character"){
    items <- 1:m
  } else {
    items <- item
  }
  AIC <- ifelse(items %in% object$DDFitems, object$AICM0[items], object$AICM1[items])
  return(AIC)
}

#' @rdname ddfMLR
#' @export
BIC.ddfMLR <- function(object, item = "all", ...){
  m <- length(object$mlrPAR)
  if (class(item) == "character"){
    if (item != "all")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("Invalid number for 'item'.",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
         call. = FALSE)
  if (class(item) == "character"){
    items <- 1:m
  } else {
    items <- item
  }
  BIC <- ifelse(items %in% object$DDFitems, object$BICM0[items], object$BICM1[items])
  return(BIC)
}
