#' DDF detection for ordinal data.
#'
#' @aliases ddfORD print.ddfORD plot.ddfORD
#'
#' @description Performs DDF detection procedure for ordinal data based either on adjacent category logit model
#' or on cumulative logit model and likelihood ratio test of submodel.
#'
#' @param Data matrix or data.frame: the ordinarily scored data matrix only or the ordinarily scored
#' data matrix plus the vector of group. See \strong{Details}.
#' @param group numeric or character: either the binary vector of group membership or
#' the column indicator of group membership. See \strong{Details}.
#' @param focal.name numeric or character: indicates the level of \code{group} which corresponds to
#' focal group
#' @param model character: logistic regression model for ordinal data (either \code{"adjacent"} (default) or \code{"cumulative"}).
#' See \strong{Details}.
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
#' \code{"irt"} (default) and \code{"classic"}. See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#'
#' @usage ddfORD(Data, group, focal.name, model = "adjacent", type = "both", match = "zscore",
#' anchor = NULL, purify = FALSE, nrIter = 10, p.adjust.method = "none",
#' parametrization = "irt", alpha = 0.05)
#'
#' @details
#' DDF detection procedure for ordinal data based either on adjacent category logit model
#' or on cumulative logit model.
#'
#' The \code{Data} is a matrix or data.frame which rows represents examinee ordinarily scored answers and
#' columns correspond to the items. The \code{group} must be either a vector of the same length as \code{nrow(Data)}
#' or column indicator of \code{Data}.
#'
#' The \code{model} corresponds to model to be used for DDF detection. Options are \code{"adjacent"}
#' for adjacent category logit model or \code{"cumulative"} for cumulative logit model.
#'
#' The \code{type} corresponds to type of DDF to be tested. Possible values are \code{"both"}
#' to detect any DDF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DDF or
#' \code{"nudif"} to detect only non-uniform DDF.
#'
#' Argument \code{match} represents the matching criterion. It can be either the standardized test score (default,
#' \code{"zscore"}), total test score (\code{"score"}), or any other continuous or discrete variable of the same
#' length as number of observations in \code{Data}.
#'
#' A set of anchor items (DDF free) can be specified through the \code{anchor} argument. It need to be a vector of either
#' item names (as specified in column names of \code{Data}) or item identifiers (integers specifying the column number).
#' In case anchor items are provided, only these items are used to compute matching criterion \code{match}. If the \code{match}
#' argument is not either \code{"zscore"} or \code{"score"}, \code{anchor} argument is ignored.  When anchor items are
#' provided, purification is not applied.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the \code{stats} package. Possible values are
#' \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, and
#' \code{"none"}. See also \code{\link[stats]{p.adjust}} for more information.
#'
#' Argument \code{parametrization} is a character which specifies parametrization of regression parameters. Default option
#' is \code{"irt"} which returns IRT parametrization (difficulty-discrimination). Option \code{"classic"} returns
#' intercept-slope parametrization with effect of group membership and interaction with matching criterion.
#'
#' The output of the \code{ddfORD()} function is displayed by the \code{print.ddfORD} function.
#'
#' The characteristic curve for item specified in \code{item} option can be plotted. For default
#' option \code{"all"} of item, characteristic curves of all converged items are plotted.
#' The drawn curves represent best model.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as \code{NA}
#' for both, \code{Data} and \code{group} parameters.
#'
#' @return A list of class 'ddfORD' with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{ordPAR}}{the estimates of the final model.}
#'   \item{\code{ordSE}}{standard errors of the estimates of the final model.}
#'   \item{\code{parM0}}{the estimates of null model.}
#'   \item{\code{parM1}}{the estimates of alternative model.}
#'   \item{\code{model}}{model used for DDF detection.}
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
#'   \item{\code{parametrization}}{Parameters' parametrization.}
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
#' martinkova@cs.cas.cz \cr
#'
#' @references
#' Agresti, A. (2010). Analysis of ordinal categorical data. Second edition. John Wiley & Sons.
#'
#' @seealso \code{\link[stats]{p.adjust}} \code{\link[VGAM]{vglm}}
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(dataMedicalgraded, package = "ShinyItemAnalysis")
#' Data <- dataMedicalgraded[, 1:5]
#' group <- dataMedicalgraded[, 101]
#'
#' # Testing both DDF effects with adjacent category logit model
#' (x <- ddfORD(Data, group, focal.name = 1, model = "adjacent"))
#'
#' # graphical devices
#' plot(x, item = 3)
#' plot(x, item = "X2003")
#' plot(x, item = "X2003", group.names = c("Group 1", "Group 2"))
#'
#' # estimated parameters
#' coef(x)
#' coef(x, SE = T) # with SE
#' coef(x, SE = T, simplify = T) # with SE, simplified
#'
#' # AIC, BIC, log-likelihood
#' AIC(x); BIC(x); logLik(x)
#' # AIC, BIC, log-likelihood for the first item
#' AIC(x, item = 1); BIC(x, item = 1); logLik(x, item = 1)
#'
#' # Testing both DDF effects with Benjamini-Hochberg adjustment method
#' ddfORD(Data, group, focal.name = 1, model = "adjacent", p.adjust.method = "BH")
#'
#' # Testing both DDF effects with item purification
#' ddfORD(Data, group, focal.name = 1, model = "adjacent", purify = T)
#'
#' # Testing uniform DDF effects
#' ddfORD(Data, group, focal.name = 1, model = "adjacent", type = "udif")
#' # Testing non-uniform DDF effects
#' ddfORD(Data, group, focal.name = 1, model = "adjacent", type = "nudif")
#'
#' # Testing both DDF effects with total score as matching criterion
#' ddfORD(Data, group, focal.name = 1, model = "adjacent", match = "score")
#'
#' # Testing both DDF effects with cumulative logit model
#' # using IRT parametrization
#' (x <- ddfORD(Data, group, focal.name = 1, model = "cumulative", parametrization = "irt"))
#'
#' # graphical devices
#' plot(x, item = 3, plot.type = "cumulative")
#' plot(x, item = 3, plot.type = "category")
#'
#' # estimated parameters in IRT parametrization
#' coef(x, simplify = T)
#' }
#'
#' @keywords DDF
#' @export
ddfORD <- function(Data, group, focal.name, model = "adjacent", type = "both", match = "zscore",
                   anchor = NULL, purify = FALSE, nrIter = 10, p.adjust.method = "none",
                   parametrization = "irt", alpha = 0.05)
{
  if (!type %in% c("udif", "nudif", "both") | !is.character(type))
    stop("'type' must be either 'udif', 'nudif' or 'both'",
         call. = FALSE)
  if (!model %in% c("adjacent", "cumulative") | !is.character(model))
    stop("'model' must be either 'adjacent' or 'cumulative'",
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
           of observations in 'Data'!",  call. = FALSE)
    }
  }
  ### purification
  if (purify & !(match[1] %in% c("score", "zscore")))
    stop("Purification not allowed when matching variable is not 'zscore' or 'score'",  call. = FALSE)

  internalORD <- function() {
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
      PROV <- suppressWarnings(ORD(DATA, GROUP, model = model, match = match, anchor = ANCHOR,
                                   type = type, p.adjust.method = p.adjust.method, parametrization = parametrization,
                                   alpha = alpha))

      STATS <- PROV$Sval
      ADJ.PVAL <- PROV$adjusted.pval
      se.m1 <- PROV$se.m1
      se.m0 <- PROV$se.m0
      significant <- which(ADJ.PVAL < alpha)

      if (length(significant) > 0) {
        DDFitems <- significant
        ordPAR <- PROV$par.m1
        ordSE <- se.m1
        for (idif in 1:length(DDFitems)) {
          ordPAR[[DDFitems[idif]]] <- PROV$par.m0[[DDFitems[idif]]]
          ordSE[[DDFitems[idif]]] <- se.m0[[DDFitems[idif]]]
        }
      } else {
        DDFitems <- "No DDF item detected"
        ordPAR <- PROV$par.m1
        ordSE <- se.m1
      }

      RES <- list(Sval = STATS,
                  ordPAR = ordPAR,
                  ordSE = ordSE,
                  parM0 = PROV$par.m0,
                  parM1 = PROV$par.m1,
                  model = model,
                  alpha = alpha, DDFitems = DDFitems,
                  type = type, purification = purify, p.adjust.method = p.adjust.method,
                  pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
                  group = GROUP, Data = DATA, match = match, group.names = group.names,
                  llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
                  AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
                  BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
                  parametrization = parametrization)
    } else {
      nrPur <- 0
      ddfPur <- NULL
      noLoop <- FALSE
      prov1 <- suppressWarnings(ORD(DATA, GROUP, model = model, type = type, match = match,
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
        ordPAR <- PROV$par.m1
        ordSE <- se.m1
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
            prov2 <- suppressWarnings(ORD(DATA, GROUP, model = model, anchor = nodif, type = type, match = match,
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
        ordPAR <- PROV$par.m1
        ordSE <- se.m1
        if (length(significant1) > 0) {
          DDFitems <- significant1
          for (idif in 1:length(DDFitems)) {
            ordPAR[[DDFitems[idif]]] <- PROV$par.m0[[DDFitems[idif]]]
            ordSE[[DDFitems[idif]]] <- se.m0[[DDFitems[idif]]]
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
                  ordPAR = ordPAR,
                  ordSE = ordSE,
                  parM0 = PROV$par.m0,
                  parM1 = PROV$par.m1,
                  model = model,
                  alpha = alpha, DDFitems = DDFitems,
                  type = type, purification = purify, nrPur = nrPur, ddfPur = ddfPur, conv.puri = noLoop,
                  p.adjust.method = p.adjust.method, pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
                  group = GROUP, Data = DATA, match = match, group.names = group.names,
                  llM0 = PROV$ll.m0, llM1 = PROV$ll.m1,
                  AICM0 = PROV$AIC.m0, AICM1 = PROV$AIC.m1,
                  BICM0 = PROV$BIC.m0, BICM1 = PROV$BIC.m1,
                  parametrization = parametrization)
    }
    class(RES) <- "ddfORD"
    return(RES)
  }
  resToReturn <- internalORD()
  return(resToReturn)
}

#' @param x an object of 'ddfORD' class
#' @param ... other generic parameters for \code{print} function.
#' @rdname ddfORD
#' @export
print.ddfORD <- function (x, ...){
  title <- paste0("Detection of ",
                  switch(x$type,
                         both = "both types of ",
                         udif = "uniform ",
                         nudif = "non-uniform"),
                  "Differential Distractor Functioning for ordinal data using ",
                  x$model, " logistic regression model.")
  cat(paste(strwrap(title, width = 60), collapse = "\n"))
  cat("\n\nLikelihood-ratio Chi-square statistics\n")
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
    switch(x$type,
           both = cat("\nNone of items is detected as DDF"),
           udif = cat("\nNone of items is detected as uniform DDF"),
           nudif = cat("\nNone of items is detected as non-uniform DDF"))
  }
  else {
    switch(x$type,
           both = cat("\n\nItems detected as DDF items:"),
           udif = cat("\n\nItems detected as uniform DDF items:"),
           nudif = cat("\n\nItems detected as non-uniform DDF items:"))
    cat("\n", paste(colnames(x$Data)[x$DDFitems], "\n", sep = ""))
  }
}

#' @param object an object of 'ddfORD' class
#' @param SE logical: should be standard errors also returned? (default is \code{FALSE}).
#' @param simplify logical: should the result be simplified to a matrix? (default is \code{FALSE}).
#' @rdname ddfORD
#' @export
coef.ddfORD <- function(object, SE = FALSE, simplify = FALSE, ...){
  if (class(SE) != "logical")
    stop("Invalid value for 'SE'. 'SE' need to be logical. ",
         call. = FALSE)
  if (class(simplify) != "logical")
    stop("Invalid value for 'simplify'. 'simplify' need to be logical. ",
         call. = FALSE)

  m = dim(object$Data)[2]
  nams = colnames(object$Data)

  coefs = object$ordPAR
  names(coefs) = nams

  if (SE){
    se = object$ordSE
    names(se) = nams
    coefs = lapply(nams, function(i) rbind(coefs[[i]], se[[i]]))
    coefs = lapply(coefs, "rownames<-", c("estimate", "SE"))
    names(coefs) = nams
  }

  if (simplify){
    res = as.data.frame(plyr::ldply(coefs, rbind))

    if (SE) {
      resnams = paste(res[, 1], c("estimate", "SE"))
    } else {
      resnams = res[, 1]
    }
    res = res[, -1]
    rownames(res) = resnams
    res[is.na(res)] = 0

    cat.est = sort(unique(unlist(lapply(object$Data, function(x) sort(unique(x))[-1]))))
    if (object$parametrization == "irt"){
      res = res[, c(paste0("b", cat.est), "a", paste0("bDIF", cat.est), "aDIF")]
    } else {
      res = res[, c(paste0("(Intercept):", cat.est), "x", "group", "x:group")[1:ncol(res)]]
    }
  } else {
    res = coefs
  }

  return(res)
}

#' @param item numeric or character: either the vector of column indicator (number or column name) or \code{'all'}
#' (default) for all items.
#' @rdname ddfORD
#' @export
logLik.ddfORD <- function(object, item = "all", ...){
  m = length(object$ordPAR)
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
                object$llM0[items],
                object$llM1[items])
  df = ifelse(items %in% object$DDFitems,
               sapply(object$parM0, length)[items],
               sapply(object$parM1, length)[items])
  if (length(items) == 1){
    attr(val, "df") = df
    class(val) = "logLik"
  }
  return(val)
}

#' @rdname ddfORD
#' @export
AIC.ddfORD <- function(object, item = "all", ...){
  m = length(object$ordPAR)
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

#' @rdname ddfORD
#' @export
BIC.ddfORD <- function(object, item = "all", ...){
  m = length(object$ordPAR)
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

#' @param title string: title of plot.
#' @param plot.type character: which plot should be displayed for cumulative logistic
#' regression model. Either \code{"category"} for category probabilities or
#' \code{"cumulative"} for cumulative probabilities.
#' @param group.names character: names of reference and focal group.
#' @rdname ddfORD
#' @export
plot.ddfORD <- function(x, item = "all", title, plot.type, group.names, ...){
  m = length(x$ordPAR)
  nams = colnames(x$Data)

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

  if (missing(plot.type))
    if (x$model == "cumulative") {
      plot.type = "category"
    } else {
      plot.type = NULL
    }

  if (x$model == "adjacent" & !is.null(plot.type)){
    warning("Argument 'plot.type' is ignored for adjacent category logit model. ")
  }
  if (!is.null(plot.type))
    if (!plot.type %in% c("category", "cumulative")){
      stop("'plot.type' can be either 'category' or 'cumulative'.  ")
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

  if (x$purification){
    anchor = c(1:m)[!c(1:m) %in% x$DDFitems]
  } else {
    anchor = 1:m
  }

  if (x$match[1] == "zscore"){
    Data = sapply(1:m, function(i) as.numeric(paste(Data[, i])))
    matching = c(unlist(scale(rowSums(as.data.frame(Data[, anchor])))))
    xlab = "Standardized total score"
  } else {
    if (x$match[1] == "score"){
      Data = sapply(1:m, function(i) as.numeric(paste(Data[, i])))
      matching = rowSums(as.data.frame(Data[, anchor]))
      xlab = "Total score"
    } else {
      if (length(x$match) == dim(x$Data)[1]){
        matching = x$match
        xlab = "Matching criterion"
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  match = seq(min(matching, na.rm = T), max(matching, na.rm = T), length.out = 300)

  mat0 = switch(x$type,
                "both"  = cbind("intercept" = 1, "match" = match, "group" = 0, "x:match" = 0),
                "udif"  = cbind("intercept" = 1, "match" = match, "group" = 0, "x:match" = 0),
                "nudif" = cbind("intercept" = 1, "match" = match, "group" = 1, "x:match" = 0))
  mat1 = switch(x$type,
                "both"  = cbind("intercept" = 1, "match" = match, "group" = 1, "x:match" = match),
                "udif"  = cbind("intercept" = 1, "match" = match, 'group' = 1, "x:match" = 0),
                "nudif" = cbind("intercept" = 1, "match" = match, "group" = 1, "x:match" = match))

  I = length(items)
  plot_CC = as.list(rep(NA, I))
  if (x$model == "adjacent"){
    for (k in 1:I){
      i = items[k]
      if (!missing(title)){
        TITLE = title
      } else {
        TITLE = colnames(x$Data)[i]
      }

      cat = unique(sort(x$Data[, i]))
      num.cat = length(cat)
      num.cat.est = num.cat - 1
      cat.est = cat[-1]

      coefs = x$ordPAR[[i]]
      if (x$parametrization == "classic"){
        coefs = c(coefs, rep(0, num.cat.est + 3 - length(coefs)))
      } else {
        a = coefs["a"]
        b = coefs[paste0("b", cat.est)]
        aDIF = coefs["aDIF"]
        bDIF = coefs[paste0("bDIF", cat.est)]
        coefs = c(-a*b, a, unique(round(-a*bDIF - aDIF*b - aDIF*bDIF, 7)), aDIF)
      }
      coefs = sapply(1:num.cat.est, function(j) coefs[c(j, (num.cat.est + 1):length(coefs))])

      # calculation probabilities on formula exp(\sum_{t = 0}^{k} b_{0t} + b1X)/(\sum_{r = 0}^{K}exp(\sum_{t=0}^{r}b_{0t} + b1X))
      df.probs.cat = matrix(0, nrow = 2*length(match), ncol = num.cat,
                            dimnames = list(NULL, cat))
      df.probs.cat[, as.character(cat.est)] = rbind(mat0 %*% coefs,
                                                    mat1 %*% coefs)
      # cumulative sum
      df.probs.cat = t(apply(df.probs.cat, 1, cumsum))
      # exponential
      df.probs.cat = exp(df.probs.cat)
      # norming
      df.probs.cat = df.probs.cat/rowSums(df.probs.cat)
      summary(df.probs.cat)

      # melting data
      df.probs.cat = data.frame(match, group = rep(c(0, 1), each = length(match)), df.probs.cat)
      colnames(df.probs.cat) = c("matching", "group", paste0("P = ", cat))
      df.probs.cat = reshape2::melt(df.probs.cat, id.vars = c("matching", "group"),
                                    variable.name = "category", value.name = "probability")
      df.probs.cat$group = as.factor(df.probs.cat$group)

      # empirical category values
      df.emp.cat0 = data.frame(table(matching[x$group == 0], x$Data[x$group == 0, i]),
                               prop.table(table(matching[x$group == 0], x$Data[x$group == 0, i]), 1),
                               group = 0)[, c(1, 2, 3, 6, 7)]
      df.emp.cat1 = data.frame(table(matching[x$group == 1], x$Data[x$group == 1, i]),
                               prop.table(table(matching[x$group == 1], x$Data[x$group == 1, i]), 1),
                               group = 1)[, c(1, 2, 3, 6, 7)]
      df.emp.cat = rbind(df.emp.cat0, df.emp.cat1)
      colnames(df.emp.cat) = c("matching", "category", "size", "probability", "group")

      df.emp.cat$matching = as.numeric(paste(df.emp.cat$matching))
      df.emp.cat$category = as.factor(df.emp.cat$category)
      df.emp.cat$group = as.factor(df.emp.cat$group)
      levels(df.emp.cat$category) = paste0("P = ", levels(df.emp.cat$category))

      plot_CC[[k]] = ggplot() +
        geom_point(data = df.emp.cat,
                   aes_string(x = "matching", y = "probability", group = "category",
                              size = "size", col = "category", fill = "category"),
                   shape = 21, alpha = 0.5) +
        geom_line(data = df.probs.cat,
                  aes_string(x = "matching", y = "probability",
                             col = "category", linetype = "group"),
                  size = 0.8) +
        xlab(xlab) +
        ylab("Category probability") +
        ylim(0, 1) +
        ggtitle(TITLE) +
        scale_linetype_manual(breaks = c(0, 1), labels = group.names,
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
              legend.position = c(0, 1),
              legend.box = "horizontal",
              legend.box.margin = margin(3, 3, 3, 3),
              legend.key = element_rect(fill = "white", colour = NA)) +
        guides(size = guide_legend(title = "Counts", order = 1),
               colour = guide_legend(title = "Score", order = 2),
               fill = guide_legend(title = "Score", order = 2),
               linetype = guide_legend(title = "Group", order = 3))

    }
  } else {

    for (k in 1:I){
      i = items[k]
      if (!missing(title)){
        TITLE = title
      } else {
        TITLE = colnames(x$Data)[i]
      }

      cat = sort(unique(x$Data[, i]))
      num.cat = length(cat)

      num.cat.est = num.cat - 1
      cat.est = cat[-1]

      coefs = x$ordPAR[[i]]
      if (x$parametrization == "classic"){
        coefs = c(coefs, rep(0, num.cat.est + 3 - length(coefs)))
      } else {
        a = coefs["a"]
        b = coefs[paste0("b", cat.est)]
        aDIF = coefs["aDIF"]
        bDIF = coefs[paste0("bDIF", cat.est)]
        coefs = c(-a*b, a, unique(round(-a*bDIF - aDIF*b - aDIF*bDIF, 7)), aDIF)
      }
      coefs = sapply(1:num.cat.est, function(j) coefs[c(j, (num.cat.est + 1):length(coefs))])

      # cumulative probabilities
      df.probs.cum = matrix(1, nrow = 2*length(match), ncol = num.cat,
                            dimnames = list(NULL, cat))

      # calculation of cumulative probabilities based on formula P(Y >= k) = exp(b0 + b1*x)/(1 + exp(b0 + b1*x))
      df.probs.cum[, as.character(cat.est)] = exp(rbind(mat0 %*% coefs, mat1 %*% coefs)) / (1 + exp(rbind(mat0 %*% coefs, mat1 %*% coefs)))

      # if column between non-ones valued columns consist of ones, it has to be changed to value on the left side
      need.correction = which(sapply(2:num.cat, function(i) (all(df.probs.cum[, i] == 1) & all(df.probs.cum[, i - 1] != 1))))
      df.probs.cum[, need.correction + 1] = df.probs.cum[, need.correction]

      # category probabilities
      df.probs.cat = data.frame(sapply(1:(num.cat - 1), function(i) df.probs.cum[, i] - df.probs.cum[, i + 1]),
                                df.probs.cum[, num.cat])


      # melting data
      df.probs.cum = data.frame(match, group = rep(c(0, 1), each = length(match)), df.probs.cum)
      colnames(df.probs.cum) = c("matching", "group", paste0("P >= ", cat))
      df.probs.cum = reshape2::melt(df.probs.cum, id.vars = c("matching", "group"),
                                    variable.name = "category", value.name = "probability")
      df.probs.cum$group = as.factor(df.probs.cum$group)

      df.probs.cat = data.frame(match, group = rep(c(0, 1), each = length(match)), df.probs.cat)
      colnames(df.probs.cat) = c("matching", "group", paste0("P = ", cat))
      df.probs.cat = reshape2::melt(df.probs.cat, id.vars = c("matching", "group"),
                                    variable.name = "category", value.name = "probability")
      df.probs.cat$group = as.factor(df.probs.cat$group)

      # empirical category values
      tmp = table(matching, x$Data[, i], x$group)
      tmp0 = tmp[, , 1]
      tmp1 = tmp[, , 2]

      df.emp.cat0 = data.frame(tmp0,
                               prop.table(tmp0, 1),
                               group = 0)[, c(1, 2, 3, 6, 7)]
      df.emp.cat1 = data.frame(tmp1,
                               prop.table(tmp1, 1),
                               group = 1)[, c(1, 2, 3, 6, 7)]

      df.emp.cat = rbind(df.emp.cat0, df.emp.cat1)
      colnames(df.emp.cat) = c("matching", "category", "size", "probability", "group")

      df.emp.cat$matching = as.numeric(paste(df.emp.cat$matching))
      df.emp.cat$category = as.factor(df.emp.cat$category)
      df.emp.cat$group = as.factor(df.emp.cat$group)
      levels(df.emp.cat$category) = paste0("P = ", levels(df.emp.cat$category))


      # empirical cumulative values
      df.emp.cum0 = prop.table(tmp0, 1)
      df.emp.cum0 = cbind(t(apply(tmp0, 1, function(x) sum(x) - cumsum(x) + x)),
                          t(apply(prop.table(tmp0, 1), 1, function(x) sum(x) - cumsum(x) + x)))
      df.emp.cum0 = data.frame(matching = as.numeric(rownames(tmp0)), group = 0, df.emp.cum0)

      df.emp.cum1 = prop.table(tmp1, 1)
      df.emp.cum1 = cbind(t(apply(tmp1, 1, function(x) sum(x) - cumsum(x) + x)),
                          t(apply(prop.table(tmp1, 1), 1, function(x) sum(x) - cumsum(x) + x)))
      df.emp.cum1 = data.frame(matching = as.numeric(rownames(tmp1)), group = 1, df.emp.cum1)

      df.emp.cum = rbind(df.emp.cum0, df.emp.cum1)
      df.emp.cum = df.emp.cum[complete.cases(df.emp.cum), ]

      df.emp.cum.count = reshape2::melt(df.emp.cum[, c(1:2, 3:(2 + num.cat))],
                                        id.vars = c("matching", "group"),
                                        variable.name = "category", value.name = "size")
      levels(df.emp.cum.count$category) = paste0("P >= ", cat)

      df.emp.cum.prob = reshape2::melt(df.emp.cum[, c(1:2, (3 + num.cat):dim(df.emp.cum)[2])],
                                       id.vars = c("matching", "group"),
                                       variable.name = "category", value.name = "probability")
      levels(df.emp.cum.prob$category) = paste0("P >= ", cat)
      df.emp.cum = merge(df.emp.cum.count, df.emp.cum.prob, by = c("matching", "group", "category"))

      # colours
      hues = seq(15, 375, length = num.cat)
      cols  = c("black", hcl(h = hues, l = 65, c = 100)[1:(num.cat - 1)])

      if (plot.type == "cumulative") {

        df.emp.cum = df.emp.cum[df.emp.cum$category != paste0("P >= ", cat[1]), ]
        df.probs.cum = df.probs.cum[df.probs.cum$category != paste0("P >= ", cat[1]), ]
        cols = cols[-1]

        df.emp.cum <- df.emp.cum[complete.cases(df.emp.cum), ]
        df.probs.cum <- df.probs.cum[complete.cases(df.probs.cum), ]

        plot_CC[[k]] <- ggplot() +
          geom_point(data = df.emp.cum,
                     aes_string(x = "matching", y = "probability",
                                size = "size", colour = "category", fill = "category"),
                     shape = 21, alpha = 0.5) +
          geom_line(data = df.probs.cum,
                    aes_string(x = "matching", y = "probability",
                               col = "category", linetype = "group"),
                    size = 0.8) +
          scale_fill_manual(values = cols) +
          scale_colour_manual(values = cols) +
          xlab(xlab) +
          ylab("Cumulative probability") +
          ggtitle(TITLE) +
          ylim(0, 1) +
          scale_linetype_manual(breaks = c(0, 1), labels = group.names,
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
                legend.justification = c("right", "bottom"),
                legend.position = c(0.98, 0.02),
                legend.box = "horizontal",
                legend.box.margin = margin(3, 3, 3, 3)) +
          guides(size = guide_legend(title = "Counts", order = 3),
                 colour = guide_legend(title = "Score", order = 2),
                 fill = guide_legend(title = "Score", order = 2),
                 linetype = guide_legend(title = "Group", order = 1))
      } else {

        df.emp.cat <- df.emp.cat[complete.cases(df.emp.cat), ]
        df.probs.cat <- df.probs.cat[complete.cases(df.probs.cat), ]

        plot_CC[[k]] <- ggplot() +
          geom_point(data = df.emp.cat,
                     aes_string(x = "matching", y = "probability",
                                size = "size", col = "category", fill = "category"),
                     shape = 21, alpha = 0.5) +
          geom_line(data = df.probs.cat,
                    aes_string(x = "matching", y = "probability",
                               col = "category", linetype = "group"),
                    size = 0.8) +
          scale_fill_manual(values = cols) +
          scale_colour_manual(values = cols) +
          xlab(xlab) +
          ggtitle(TITLE) +
          ylab("Category probability") +
          ylim(0, 1) +
          scale_linetype_manual(breaks = c(0, 1), labels = group.names,
                                values = c("solid", "dashed")) +
          theme_bw() +
          theme(axis.line  = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background = element_rect(fill = "transparent", colour = NA),
                legend.key = element_rect(fill = "white", colour = NA),
                legend.background = element_rect(fill = "transparent", colour = NA),
                legend.box.background = element_rect(fill = "transparent", colour = NA)) +
          theme(legend.box.just = "top",
                legend.justification = c("left", "top"),
                legend.position = c(0.02, 0.98),
                legend.box = "horizontal",
                legend.box.margin = margin(3, 3, 3, 3)) +
          guides(size = guide_legend(title = "Counts", order = 1),
                 colour = guide_legend(title = "Score", order = 2),
                 fill = guide_legend(title = "Score", order = 2),
                 linetype = guide_legend(title = "Group", order = 3))
      }
    }

  }
  plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
  return(plot_CC)
}
