#' DDF likelihood ratio statistics for ordinal data.
#'
#' @aliases ORD
#'
#' @description Calculates DDF likelihood ratio statistics for ordinal data based either on adjacent logistic regression
#' model or on cumulative logistic regression model.
#'
#' @param Data matrix or data.frame: the ordinarily scored data matrix.
#' @param group numeric or character: the binary vector of group membership
#' @param model character: logistic regression model for ordinal data (either \code{"adjacent"} (default) or \code{"cumulative"}).
#' See \strong{Details}.
#' @param type character: type of DDF to be tested (either \code{"both"} (default), \code{"udif"}, or \code{"nudif"}).
#' See \strong{Details}.
#' @param match specifies matching criterion. Can be either \code{"zscore"} (default, standardized total score),
#' \code{"score"} (total test score), or vector of the same length as number of observations in \code{Data}. See \strong{Details}.
#' @param anchor a vector of integers specifying which items are currently considered as anchor (DIF free) items. By
#' default, all items are considered as anchors. Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' See \strong{Details}.
#' @param p.adjust.method character: method for multiple comparison correction.
#' See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#'
#' @usage ORD(Data, group, model = "adjacent", type = "both", match = "zscore",
#' anchor = 1:ncol(Data), p.adjust.method = "none", alpha = 0.05)
#'
#' @details
#' Calculates DDF likelihood ratio statistics for ordinal data based either on adjacent logistic regression
#' model or on cumulative logistic regression model
#'
#' The \code{Data} is a matrix or data.frame which rows represents examinee ordinarily scored answers and
#' columns correspond to the items. The \code{group} must be a vector of the same length as \code{nrow(Data)}.
#'
#' The \code{model} corresponds to model to be used for DDF detection. Options are \code{"adjacent"}
#' for adjacent logistic regression model or \code{"cumulative"} for cumulative logistic regression model.
#'
#' The \code{type} corresponds to type of DDF to be tested. Possible values are \code{"both"}
#' to detect any DDF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DDF or
#' \code{"nudif"} to detect only non-uniform DDF.
#'
#' Argument \code{match} represents the matching criterion. It can be either the standardized test score (default, \code{"zscore"}),
#' total test score (\code{"score"}), or any other continuous or discrete variable of the same length as number of observations
#' in \code{Data}.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the \code{stats} package. Possible values are
#' \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, and
#' \code{"none"}. See also \code{\link[stats]{p.adjust}} for more information.
#'
#' @return A list with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{pval}}{the p-values by likelihood ratio test.}
#'   \item{\code{adj.pval}}{the adjusted p-values by likelihood ratio test using \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of likelihood ratio test.}
#'   \item{\code{par.m0}}{the estimates of null model.}
#'   \item{\code{par.m1}}{the estimates of alternative model.}
#'   \item{\code{cov.m0}}{the estimates of covariance structure of null model.}
#'   \item{\code{cov.m1}}{the estimates of covariance structure of alternative model.}
#'   \item{\code{ll.m0}}{log-likelihood of null model.}
#'   \item{\code{ll.m1}}{log-likelihood of alternative model.}
#'   \item{\code{AIC.m0}}{AIC of null model.}
#'   \item{\code{AIC.m1}}{AIC of alternative model.}
#'   \item{\code{BIC.m0}}{BIC of null model.}
#'   \item{\code{BIC.m1}}{BIC of alternative model.}
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
#' @seealso \code{\link[stats]{p.adjust}} \code{\link[VGAM]{vglm}}
#'
#' @examples
#' \dontrun{
#' # loading data based on GMAT
#'
#' # Testing both DDF effects
#' ORD(Data, group, key, type = "both")
#'
#' # Testing uniform DDF effects
#' ORD(Data, group, key, type = "udif")
#'
#' # Testing non-uniform DDF effects
#' ORD(Data, group, key, type = "nudif")
#' }
#' @keywords DDF
#' @export
#' @importFrom VGAM cumulative acat lrtest_vglm

ORD <- function(Data, group, model = "adjacent", type = "both", match = "zscore",
                anchor = 1:ncol(Data), p.adjust.method = "none", alpha = 0.05){

  if (match[1] == "zscore"){
    x = c(unlist(scale(rowSums(Data))))
  } else {
    if (match[1] == "score"){
      x = rowSums(Data)
    } else {
      if (length(match) == dim(Data)[1]){
        x = match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  m = dim(Data)[2]
  n = dim(Data)[1]

  uval <- lapply(Data, function(x) sort(unique(x)))
  for (i in 1:dim(Data)[2]){
    Data[, i] <- factor(Data[, i], levels = uval[[i]], ordered = T)
  }

  if (model == "adjacent"){
    family = VGAM::acat(reverse = FALSE, parallel = TRUE)
  } else {
    family = VGAM::cumulative(reverse = TRUE, parallel = TRUE)
  }

  m0 = lapply(1:m, function(i) switch(type,
                                      "both"  = VGAM::vglm(Data[, i] ~ x * group, family = family),
                                      "nudif" = VGAM::vglm(Data[, i] ~ x * group, family = family),
                                      "udif"  = VGAM::vglm(Data[, i] ~ x + group, family = family)))
  m1 = lapply(1:m, function(i) switch(type,
                                      "both"  = VGAM::vglm(Data[, i] ~ x, family = family),
                                      "nudif" = VGAM::vglm(Data[, i] ~ x + group, family = family),
                                      "udif"  = VGAM::vglm(Data[, i] ~ x, family = family)))

  ORDtest <- lapply(1:m, function(i) VGAM::lrtest_vglm(m0[[i]], m1[[i]])@Body)
  ORDstat <- sapply(1:m, function(i) c(ORDtest[[i]]$Chisq[2],
                                       ORDtest[[i]]$`Pr(>Chisq)`[2],
                                       ORDtest[[i]]$Df[2]))

  adjusted.pval <- p.adjust(ORDstat[2, ], method = p.adjust.method)

  par.m1 <- lapply(m1, coef)
  par.m0 <- lapply(m0, coef)

  cov.m1 <- lapply(m1, vcov)
  cov.m0 <- lapply(m0, vcov)

  ll.m0 <- sapply(m0, logLik)
  ll.m1 <- sapply(m0, logLik)

  AIC.m0 <- sapply(m0, AIC)
  AIC.m1 <- sapply(m1, AIC)

  BIC.m0 <- sapply(m0, BIC)
  BIC.m1 <- sapply(m1, BIC)

  results <- list(Sval = ORDstat[1, ],
                  pval = ORDstat[2, ], adjusted.pval = adjusted.pval,
                  df = ORDstat[3, ],
                  par.m0 = par.m0, cov.m0 = cov.m0,
                  par.m1 = par.m1, cov.m1 = cov.m1,
                  ll.m0 = ll.m0, ll.m1 = ll.m1,
                  AIC.m0 = AIC.m0, AIC.m1 = AIC.m1,
                  BIC.m0 = BIC.m0, BIC.m1 = BIC.m1)
  return(results)
}
