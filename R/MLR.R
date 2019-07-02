#' DDF likelihood ratio statistics based on Multinomial Log-linear Regression model.
#'
#' @aliases MLR
#'
#' @description Calculates DDF likelihood ratio statistics based on multinomial log-linear model.
#'
#' @param Data character: the unscored data matrix.
#' @param group numeric or character: the binary vector of group membership
#' @param key character: the answer key.
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
#' @usage MLR(Data, group, key, type = "both", match = "zscore", anchor = 1:ncol(Data),
#' p.adjust.method = "none", alpha = 0.05)
#'
#' @details
#' Calculates DDF likelihood ratio statistics based on multinomial log-linear model.
#'
#' The \code{Data} is a matrix which rows represents examinee unscored answers and
#' columns correspond to the items. The \code{group} must be a vector of the same
#' length as \code{nrow(data)}. The \code{key} must be a vector of correct answers
#' corresponding to columns of \code{Data}.
#'
#' The \code{type} corresponds to type of DDF to be tested. Possible values are \code{"both"}
#' to detect any DDF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DDF or
#' \code{"nudif"} to detect only non-uniform DDF.
#'
#' Argument \code{match} represents the matching criterion. It can be either the standardized test score (default, \code{"zscore"}),
#' total test score (\code{"score"}), or any other continuous or discrete variable of the same length as number of observations
#' in \code{Data}. Matching criterion is used in \code{NLR} function as a covariate in non-linear regression model.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the
#' \code{stats} package. Possible values are \code{"holm"}, \code{"hochberg"},
#' \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}.
#' See also \code{\link[stats]{p.adjust}} for more information.
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
#'   \item{\code{ll.m0}}{log-likelihood of m0 model.}
#'   \item{\code{ll.m1}}{log-likelihood of m1 model.}
#'   \item{\code{AIC.m0}}{AIC of m0 model.}
#'   \item{\code{AIC.m1}}{AIC of m1 model.}
#'   \item{\code{BIC.m0}}{BIC of m0 model.}
#'   \item{\code{BIC.m1}}{BIC of m1 model.}
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
#' MLR(Data, group, key, type = "both")
#'
#' # Testing uniform DDF effects
#' MLR(Data, group, key, type = "udif")
#'
#' # Testing non-uniform DDF effects
#' MLR(Data, group, key, type = "nudif")
#' }
#' @keywords DDF
#' @export
#' @importFrom stats coef vcov p.adjust.methods anova
#' @importFrom nnet multinom
#' @importFrom CTT score


MLR <- function(Data, group, key, type = "both", match = "zscore", anchor = 1:ncol(Data), p.adjust.method = "none", alpha = 0.05){

  if (match[1] == "zscore"){
    x <- c(scale(unlist(CTT::score(as.data.frame(Data[, anchor]), key[anchor]))))
  } else {
    if (match[1] == "score"){
      x <- c(unlist(CTT::score(as.data.frame(Data[, anchor]), key[anchor])))
    } else {
      if (length(match) == dim(Data)[1]){
        x <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'!")
      }
    }
  }

  m <- dim(Data)[2]
  n <- dim(Data)[1]

  m0 <- lapply(1:m, function(i) switch(type,
                                       "both" = nnet::multinom(relevel(as.factor(Data[, i]),
                                                                 ref = paste(key[i])) ~ x * group,
                                                         trace = F),
                                       "nudif" = nnet::multinom(relevel(as.factor(Data[, i]),
                                                                  ref = paste(key[i])) ~ x * group,
                                                          trace = F),
                                       "udif" = nnet::multinom(relevel(as.factor(Data[, i]),
                                                                 ref = paste(key[i])) ~ x + group,
                                                         trace = F)))
  m1 <- lapply(1:m, function(i) switch(type,
                                       "both" = nnet::multinom(relevel(as.factor(Data[, i]),
                                                                 ref = paste(key[i])) ~ x,
                                                         trace = F),
                                       "nudif" = nnet::multinom(relevel(as.factor(Data[, i]),
                                                                  ref = paste(key[i])) ~ x + group,
                                                          trace = F),
                                       "udif" = nnet::multinom(relevel(as.factor(Data[, i]),
                                                                 ref = paste(key[i])) ~ x,
                                                         trace = F)))

  MLRtest <- lapply(1:m, function(i) anova(m0[[i]], m1[[i]]))
  MLRstat <- sapply(1:m, function(i) c(MLRtest[[i]]$`LR stat.`[2], MLRtest[[i]]$`Pr(Chi)`[2]))
  df <- lapply(1:m, function(i) MLRtest[[i]]$`Resid. df`)

  adjusted.pval <- p.adjust(MLRstat[2, ], method = p.adjust.method)

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

  results <- list(Sval = MLRstat[1, ],
                  pval = MLRstat[2, ], adjusted.pval = adjusted.pval,
                  df = df,
                  par.m0 = par.m0, cov.m0 = cov.m0,
                  par.m1 = par.m1, cov.m1 = cov.m1,
                  ll.m0 = ll.m0, ll.m1 = ll.m1,
                  AIC.m0 = AIC.m0, AIC.m1 = AIC.m1,
                  BIC.m0 = BIC.m0, BIC.m1 = BIC.m1)
  return(results)
}



