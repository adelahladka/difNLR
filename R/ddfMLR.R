#' DDF detection for nominal data.
#'
#' @aliases ddfMLR print.ddfMLR plot.ddfMLR
#'
#' @description Performs DDF detection procedure for nominal data based on multinomial
#' log-linear regression model and likelihood ratio test of submodel.
#'
#' @param Data character: either the unscored data matrix only, or the unscored data
#' matrix plus the vector of group. See \strong{Details}.
#' @param group numeric or character: either the binary vector of group membership or
#' the column indicator of group membership. See \strong{Details}.
#' @param focal.name numeric or character: indicates the level of \code{group} which corresponds to
#' focal group
#' @param key character: the answer key. See \strong{Details}.
#' @param type character: type of DDF to be tested (either \code{"both"} (default), \code{"udif"}, or \code{"nudif"}).
#' See \strong{Details}.
#' @param match specifies matching criterion. Can be either \code{"zscore"} (default, standardized total score),
#' \code{"score"} (total test score), or vector of the same length as number of observations in \code{Data}. See \strong{Details}.
#' @param anchor Either \code{NULL} (default) or a vector of item names or item identifiers specifying which items are
#' currently considered as anchor (DDF free) items. Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' @param purify logical: should the item purification be applied? (default is \code{FALSE}). See \strong{Details}.
#' @param nrIter numeric: the maximal number of iterations in the item purification (default is 10).
#' @param p.adjust.method character: method for multiple comparison correction.
#' See \strong{Details}.
#' @param parametrization character: parametrization of regression coefficients. Possible options are
#' \code{"classic"} and \code{"irt"}. See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#' @param x an object of 'ddfMLR' class
#' @param object an object of 'ddfMLR' class
#' @param item numeric or character: either the vector of column indicator (number or column name) or \code{'all'}
#' (default) for all items. See \strong{Details}.
#' @param title string: title of plot.
#' @param ... other generic parameters for \code{print} or \code{plot} functions.
#'
#' @usage ddfMLR(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
#' purify = FALSE, nrIter = 10, p.adjust.method = "none", parametrization = "classic",
#' alpha = 0.05)
#'
#' @details
#' Performs DDF detection procedure for nominal data based on multinomial
#' log-linear regression model and likelihood ratio test of submodel.
#'
#' The \code{Data} is a matrix which rows represents examinee unscored answers and
#' columns correspond to the items. The \code{group} must be either a vector of the same
#' length as \code{nrow(Data)} or column indicator of \code{Data}. The \code{key} must be
#' a vector of correct answers corresponding to columns of \code{Data}.
#'
#' The \code{type} corresponds to type of DDF to be tested. Possible values are \code{"both"}
#' to detect any DDF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DDF or
#' \code{"nudif"} to detect only non-uniform DDF.
#'
#' Argument \code{match} represents the matching criterion. It can be either the standardized test score (default, \code{"zscore"}),
#' total test score (\code{"score"}), or any other continuous or discrete variable of the same length as number of observations
#' in \code{Data}.
#'
#' A set of anchor items (DDF free) can be specified through the \code{anchor} argument. It need to be a vector of either
#' item names (as specified in column names of \code{Data}) or item identifiers (integers specifying the column number).
#' In case anchor items are provided, only these items are used to compute matching criterion \code{match}. If the \code{match}
#' argument is not either \code{"zscore"} or \code{"score"}, \code{anchor} argument is ignored.  When anchor items are
#' provided, purification is not applied.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the
#' \code{stats} package. Possible values are \code{"holm"}, \code{"hochberg"},
#' \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}.
#' See also \code{\link[stats]{p.adjust}} for more information.
#'
#' Argument \code{parametrization} is a character which specifies parametrization of regression parameters. Default option
#' is \code{"classic"} for intercept-slope parametrization with effect of group membership and interaction with matching criterion.
#' Option \code{"irt"} returns IRT parametrization.
#'
#' The output of the \code{ddfMLR} function is displayed by the \code{print.ddfMLR} function.
#'
#' The characteristic curve for item specified in \code{item} option can be plotted. \code{item} can be
#' column indicator (numeric or character - column name) or \code{"all"} (default). For option \code{"all"},
#' characteristic curves of all converged items are plotted. The drawn curves represent best model.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as \code{NA}
#' for both, \code{Data} and \code{group} parameters.
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
#'   \item{\code{type}}{character: type of DDF that was tested.}
#'   \item{\code{purification}}{\code{purify} value.}
#'   \item{\code{nrPur}}{number of iterations in item purification process. Returned only if \code{purify}
#'   is \code{TRUE}.}
#'   \item{\code{ddfPur}}{a binary matrix with one row per iteration of item purification and one column per item.
#'   "1" in i-th row and j-th column means that j-th item was identified as DDF in i-1-th iteration. Returned only
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
#'   \item{\code{group.names}}{levels of grouping variable.}
#'   \item{\code{llM0}}{log-likelihood of null model.}
#'   \item{\code{llM1}}{log-likelihood of alternative model.}
#'   \item{\code{AICM0}}{AIC of null model.}
#'   \item{\code{AICM1}}{AIC of alternative model.}
#'   \item{\code{BICM0}}{BIC of null model.}
#'   \item{\code{BICM1}}{BIC of alternative model.}
#'   }
#'
#' @author
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' hladka@cs.cas.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#'
#' @seealso \code{\link[stats]{p.adjust}} \code{\link[nnet]{multinom}}
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
#' plot(x, item = 1, group.names = c("Group 1", "Group 2"))
#' plot(x, item = x$DDFitems)
#' plot(x, item = "all")
#'
#' # AIC, BIC, log-likelihood
#' AIC(x); BIC(x); logLik(x)
#' # AIC, BIC, log-likelihood for the first item
#' AIC(x, item = 1); BIC(x, item = 1); logLik(x, item = 1)
#'
#' # estimates
#' coef(x)
#' coef(x, SE = TRUE)
#' coef(x, SE = TRUE, simplify = TRUE)
#' }
#'
#' @keywords DDF
#' @export
ddfMLR <- function(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
                   purify = FALSE, nrIter = 10, p.adjust.method = "none", parametrization = "classic",
                   alpha = 0.05)
{
  if (!type %in% c("udif", "nudif", "both") | !is.character(type))
    stop("'type' must be either 'udif', 'nudif' or 'both'",
         call. = FALSE)
  if (alpha > 1 | alpha < 0)
    stop("'alpha' must be between 0 and 1",
         call. = FALSE)
  if (!parametrization %in% c("classic", "irt"))
    stop("Invalid value for 'parametrization'. Possible values are 'classic' and 'irt'.",
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
        DATA <- Data[, (1:dim(Data)[2]) != group]
        colnames(DATA) <- colnames(Data)[(1:dim(Data)[2]) !=  group]
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
      if (dim(DATA)[1] != length(GROUP))
        stop("'Data' must have the same number of rows as is length of vector 'group'",
             call. = FALSE)
    } else {
      stop("'Data' must be data frame or matrix of binary vectors",
           call. = FALSE)
    }
    if (length(key) != dim(DATA)[2]){
      stop("Number of items in 'Data' is not equal to the length of 'key'.",
           call. = FALSE)
    }
    group.names = unique(GROUP)[!is.na(unique(GROUP))]
    if (group.names[1] == focal.name)
      group.names = rev(group.names)
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
          ANCHOR[i] <- (1:dim(DATA)[2])[colnames(DATA) == anchor[i]]
      }
    } else {
      ANCHOR <- 1:dim(DATA)[2]
    }
    if (!purify | !(match[1] %in% c("zscore", "score")) | !is.null(anchor)) {
      PROV <- suppressWarnings(MLR(DATA, GROUP, key = key, match = match, anchor = ANCHOR,
                                   type = type, p.adjust.method = p.adjust.method, parametrization = parametrization,
                                   alpha = alpha))

      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      se.m1 <- PROV$se.m1
      se.m0 <- PROV$se.m0
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

      nrow <- sapply(mlrPAR, nrow)
      colnams <- lapply(mlrPAR, colnames)
      rownams <- lapply(mlrPAR, rownames)
      mlrSE <- lapply(1:length(mlrSE), function(x) matrix(mlrSE[[x]], nrow = nrow[[x]],
                                                          dimnames = list(rownams[[x]], colnams[[x]])))
      RES <- list(Sval = STATS,
                  mlrPAR = mlrPAR,
                  mlrSE = mlrSE,
                  parM0 = PROV$par.m0,
                  parM1 = PROV$par.m1,
                  alpha = alpha, DDFitems = DDFitems,
                  type = type, purification = purify, p.adjust.method = p.adjust.method,
                  parametrization = parametrization,
                  pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
                  group = GROUP, Data = DATA, key = key, match = match, group.names = group.names,
                  llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
                  AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
                  BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1)
    } else {
      nrPur <- 0
      ddfPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(MLR(DATA, GROUP, key = key, type = type,
                                    p.adjust.method = p.adjust.method, parametrization = parametrization,
                                    alpha = alpha))
      stats1 <- prov1$Sval
      pval1 <- prov1$pval
      significant1 <- which(pval1 < alpha)
      if (length(significant1) == 0) {
        PROV <- prov1
        STATS <- stats1
        DDFitems <- "No DDF item detected"
        se.m1 <- PROV$se.m1
        se.m0 <- PROV$se.m0
        mlrPAR <- PROV$par.m1
        mlrSE <- se.m1
        noLoop <- TRUE
      } else {
        dif <- significant1
        ddfPur <- rep(0, length(stats1))
        ddfPur[dif] <- 1
        repeat {
          if (nrPur >= nrIter){
            break
          } else {
            nrPur <- nrPur + 1
            nodif <- NULL
            if (is.null(dif)) {
              nodif <- 1:dim(DATA)[2]
            } else {
              for (i in 1:dim(DATA)[2]) {
                if (sum(i == dif) == 0)
                  nodif <- c(nodif, i)
              }
            }
            prov2 <- suppressWarnings(MLR(DATA, GROUP, key = key, anchor = nodif, type = type,
                                          p.adjust.method = p.adjust.method, parametrization = parametrization,
                                          alpha = alpha))
            stats2 <- prov2$Sval
            pval2 <- prov2$pval
            significant2 <- which(pval2 < alpha)
            if (length(significant2) == 0)
              dif2 <- NULL
            else dif2 <- significant2
            ddfPur <- rbind(ddfPur, rep(0, dim(DATA)[2]))
            ddfPur[nrPur + 1, dif2] <- 1
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
        se.m1 <- PROV$se.m1
        se.m0 <- PROV$se.m0
        mlrPAR <- PROV$par.m1
        mlrSE <- se.m1
        if (length(significant1) > 0) {
          DDFitems <- significant1
          for (idif in 1:length(DDFitems)) {
            mlrPAR[[DDFitems[idif]]] <- PROV$par.m0[[DDFitems[idif]]]
            mlrSE[[DDFitems[idif]]] <- se.m0[[DDFitems[idif]]]
          }
        } else {
          DDFitems <- "No DDF item detected"
        }
      }
      if (!is.null(ddfPur)) {
        rownames(ddfPur) <- paste("Step", 0:(dim(ddfPur)[1] - 1), sep = "")
        colnames(ddfPur) <- colnames(DATA)
      }
      RES <- list(Sval = STATS,
                  mlrPAR = mlrPAR,
                  mlrSE = mlrSE,
                  parM0 = PROV$par.m0,
                  parM1 = PROV$par.m1,
                  alpha = alpha, DDFitems = DDFitems,
                  type = type, purification = purify, nrPur = nrPur, ddfPur = ddfPur, conv.puri = noLoop,
                  p.adjust.method = p.adjust.method,
                  parametrization = parametrization,
                  pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
                  group = GROUP, Data = DATA, key = key, match = match, group.names = group.names,
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
                 symnum(x$adj.pval,
                        c(0, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", "")))
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

#' @param group.names character: names of reference and focal group.
#' @rdname ddfMLR
#' @export
plot.ddfMLR <- function(x, item = "all", title, group.names, ...){
  m = length(x$mlrPAR)
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
    items = 1:m
  } else {
    items = item
  }

  if (missing(group.names)){
    group.names = x$group.names
    if (all(group.names %in% c(0, 1))) group.names = c("Reference", "Focal")
  }
  if (length(group.names) > 2){
    group.names = group.names[1:2]
    warning("Only first two values for 'group.names' argument are used. ")
  } else {
    if (length(group.names) < 2){
      group.names = c("Reference", "Focal")
      warning("Argument 'group.names' need to have length of two. Default value is used.")
    }
  }

  if (x$match[1] == "zscore"){
    score = c(scale(unlist(CTT::score(as.data.frame(x$Data), x$key))))
    xlab = "Standardized total score"
  } else {
    if (x$match[1] == "score"){
      score = c(unlist(CTT::score(as.data.frame(x$Data), x$key)))
      xlab = "Total score"
    } else {
      if (length(x$match) == dim(x$Data)[1]){
        score = x$match
        xlab = "Matching criterion"
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  sq = seq(min(score, na.rm = T), max(score, na.rm = T), length.out = 300)
  sqR = as.matrix(data.frame(1, sq, 0, 0))
  sqF = as.matrix(data.frame(1, sq, 1, sq))

  plot_CC = list()
  for (i in items){

    if (!missing(title)){
      TITLE = title
    } else {
      TITLE = colnames(x$Data)[i]
    }

    coefs = x$mlrPAR[[i]]
    if (x$parametrization == "irt"){
      a = coefs[, "a"]
      b = coefs[, "b"]
      aDIF = coefs[, "aDIF"]
      bDIF = coefs[, "bDIF"]
      coefs = cbind(-a*b, a, -a*bDIF - aDIF*b - aDIF*bDIF, aDIF)
    }

    if (is.null(dim(coefs))) coefs = matrix(coefs, ncol = length(coefs))
    if (dim(coefs)[2] == 2)
      coefs = as.matrix(data.frame(coefs, 0, 0))
    if (dim(coefs)[2] == 3)
      coefs = as.matrix(data.frame(coefs, 0))

    prR = as.data.frame(t(exp(coefs %*% t(sqR))))
    prF = as.data.frame(t(exp(coefs %*% t(sqF))))
    colnames(prR) = colnames(prF) = rownames(coefs)

    prR = sapply(prR, function(x) x/(rowSums(prR) + 1))
    prF = sapply(prF, function(x) x/(rowSums(prF) + 1))

    hvR = data.frame(1 - rowSums(prR), prR, "R", sq)
    hvF = data.frame(1 - rowSums(prF), prF, "F", sq)

    if (is.null(rownames(coefs))){
      nams = levels(x$Data[, i])[levels(x$Data[, i]) != x$key[i]]
    } else {
      nams = rownames(coefs)
    }

    colnames(hvR) = colnames(hvF) = c(paste(x$key[i]), nams, "group", "score")
    hv = rbind(hvR, hvF)

    df = reshape2::melt(hv, id = c("score", "group"))
    df$group = as.factor(df$group)

    df2 = rbind(data.frame(prop.table(table(x$Data[x$group == 1, i], score[x$group == 1]), 2),
                            table(x$Data[x$group == 1, i], score[x$group == 1]),
                            group = "1"),
                 data.frame(prop.table(table(x$Data[x$group == 0, i], score[x$group == 0]), 2),
                            table(x$Data[x$group == 0, i], score[x$group == 0]),
                            group = "0"))

    df2$score = as.numeric(levels(df2$Var2))[df2$Var2]
    df2$answ = relevel(df2$Var1, ref = paste(x$key[i]))
    df2$group = as.factor(df2$group)

    df$variable = relevel(df$variable, ref = paste(x$key[i]))

    plot_CC[[i]] =  ggplot() +
      geom_line(data = df,
                aes_string(x = "score" , y = "value",
                           colour = "variable", linetype = "group"),
                size = 0.8) +
      geom_point(data = df2,
                 aes_string(x = "score", y = "Freq",
                            colour = "answ", fill = "answ",
                            size = "Freq.1"),
                 alpha = 0.5, shape = 21) +

      ylim(0, 1) +
      ggtitle(TITLE) +
      labs(x = xlab,
           y = "Probability of answer") +
      scale_linetype_manual(breaks = c("R", "F"), labels = group.names,
                            values = c("solid", "dashed")) +
      theme_bw() +
      theme(axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.key = element_rect(fill = "white", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            legend.box.background = element_rect(fill = "transparent", colour = NA)) +
      ### legend
      theme(legend.box.just = "top",
            legend.justification = c("left", "top"),
            legend.position = c(0.02, 0.98),
            legend.box = "horizontal",
            legend.box.margin = margin(3, 3, 3, 3)) +
      guides(size = guide_legend(title = "Counts", order = 1),
             colour = guide_legend(title = "Answer", order = 2),
             fill = guide_legend(title = "Answer", order = 2),
             linetype = guide_legend(title = "Group", order = 3))

  }
  plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
  return(plot_CC)
  }

#' @param SE logical: should be standard errors also returned? (default is \code{FALSE}).
#' @param simplify logical: should the result be simplified to a matrix? (default is \code{FALSE}).
#' @rdname ddfMLR
#' @export
coef.ddfMLR <- function(object, SE = FALSE, simplify = FALSE, ...){
  if (class(SE) != "logical")
    stop("Invalid value for 'SE'. 'SE' need to be logical. ",
         call. = FALSE)
  if (class(simplify) != "logical")
    stop("Invalid value for 'simplify'. 'simplify' need to be logical. ",
         call. = FALSE)

  m = dim(object$Data)[2]
  nams = colnames(object$Data)

  coefs = object$mlrPAR
  names(coefs) = nams

  if (SE){
    se = object$mlrSE
    names(se) = nams

    coefs = lapply(nams, function(i) rbind(coefs[[i]], se[[i]])[order(c(rownames(coefs[[i]]), rownames(se[[i]]))), ])
    rownams = lapply(coefs, function(x) paste(rownames(x), c("estimate", "SE")))
    coefs = lapply(coefs, function(x) {
      i = which(coefs %in% list(x))
      rownames(x) = rownams[[i]]
      return(x)
    })
  }

  if (simplify){
    catnams = unlist(lapply(coefs, rownames))
    resnams = unlist(lapply(1:m, function(i) rep(nams[i], sapply(coefs, nrow)[i])))
    resnams = paste(resnams, catnams)
    res = as.data.frame(plyr::ldply(coefs, rbind))
    rownames(res) = resnams
    res[is.na(res)] = 0
    if (!SE) res = res[, -1]
  } else {
    res = coefs
  }

  return(res)
}

#' @rdname ddfMLR
#' @export
logLik.ddfMLR <- function(object, item = "all", ...){
  m = length(object$mlrPAR)
  nams = colnames(object$Data)

  if (class(item) == "character"){
    if (item != "all" & !item %in% nams)
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("Invalid number for 'item'.",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
         call. = FALSE)
  if (class(item) == "character"){
    if (item[1] == "all"){
      items = 1:m
    } else {
      items = which(nams %in% item)
    }
  } else {
    items = item
  }

  val = ifelse(items %in% object$DDFitems,
               object$llM0[[items]],
               object$llM1[[items]])
  df = ifelse(items %in% object$DDFitems,
              sapply(object$parM0, length)[items],
              sapply(object$parM1, length)[items])
  if (length(items) == 1){
    attr(val, "df") = df
    class(val) = "logLik"
  }
  return(val)
}

#' @rdname ddfMLR
#' @export
AIC.ddfMLR <- function(object, item = "all", ...){
  m = length(object$mlrPAR)
  nams = colnames(object$Data)

  if (class(item) == "character"){
    if (item != "all" & !item %in% nams)
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("Invalid number for 'item'.",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
         call. = FALSE)
  if (class(item) == "character"){
    if (item[1] == "all"){
      items = 1:m
    } else {
      items = which(nams %in% item)
    }
  } else {
    items = item
  }

  AIC = ifelse(items %in% object$DDFitems, object$AICM0[items], object$AICM1[items])
  return(AIC)
}

#' @rdname ddfMLR
#' @export
BIC.ddfMLR <- function(object, item = "all", ...){
  m = length(object$mlrPAR)
  nams = colnames(object$Data)

  if (class(item) == "character"){
    if (item != "all" & !item %in% nams)
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
           call. = FALSE)
  } else {
    if (class(item) != "integer" & class(item) != "numeric")
      stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all' or name of item. ",
           call. = FALSE)
  }
  if (class(item) == "numeric" & !all(item %in% 1:m))
    stop("Invalid number for 'item'.",
         call. = FALSE)
  if (class(item) == "integer" & !all(item %in% 1:m))
    stop("Invalid value for 'item'. Item must be either numeric vector or character string 'all'. ",
         call. = FALSE)
  if (class(item) == "character"){
    if (item[1] == "all"){
      items = 1:m
    } else {
      items = which(nams %in% item)
    }
  } else {
    items = item
  }

  BIC = ifelse(items %in% object$DDFitems, object$BICM0[items], object$BICM1[items])
  return(BIC)
}
