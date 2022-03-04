#' DDF likelihood ratio statistics based on multinomial log-linear
#' regression model.
#'
#' @aliases MLR
#'
#' @description Calculates DDF likelihood ratio statistics for nominal
#'   data based on multinomial log-linear model.
#'
#' @param Data data.frame or matrix: dataset which rows represent
#'   unscored examinee answers (nominal) and columns correspond to the
#'   items.
#' @param group numeric: binary vector of group membership. \code{"0"}
#'   for reference group, \code{"1"} for focal group.
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
#' @param match numeric or character: matching criterion to be used as
#'   an estimate of trait. Can be either \code{"zscore"} (default,
#'   standardized total score), \code{"score"} (total test score), or
#'   vector of the same length as number of observations in
#'   \code{Data}.
#' @param anchor character or numeric: specification of DIF free
#'   items. A vector of item identifiers (integers specifying the
#'   column  number) specifying which items are currently considered
#'   as anchor (DIF free) items. Argument is ignored if \code{match}
#'   is not \code{"zscore"} or \code{"score"}.
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
#' MLR(Data, group, key, type = "both", match = "zscore", anchor = 1:ncol(Data),
#'     p.adjust.method = "none", alpha = 0.05, parametrization)
#'
#' @details
# Calculates DDF likelihood ratio statistics based on multinomial
# log-linear model. Probability of selection the \eqn{k}-th category
# (distractor) is
#' \deqn{P(y = k) = exp(b_0k + b_1k * x + b_2k * g + b_3k * x * g) / (1 + \sum exp(b_0l + b_1l * x + b_2l * g + b_3l * x * g)), }
#' where \eqn{x} is by default standardized total score (also called
#' Z-score) and \eqn{g} is a group membership. Probability of correct
#' answer (specified in argument \code{key}) is
#' \deqn{P(y = k) = 1/(1 + \sum exp(b_0l + b_1l * x + b_2l * g + b_3l * x * g)). }
#' Parameters are estimated via neural networks. For more details see
#' \code{\link[nnet]{multinom}}.
#'
#' @return A list with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{pval}}{the p-values by likelihood ratio test.}
#'   \item{\code{adj.pval}}{the adjusted p-values by likelihood ratio test using \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of likelihood ratio test.}
#'   \item{\code{par.m0}}{the estimates of null model.}
#'   \item{\code{par.m1}}{the estimates of alternative model.}
#'   \item{\code{se.m0}}{standard errors of parameters in null model.}
#'   \item{\code{se.m1}}{standard errors of parameters in alternative model.}
#'   \item{\code{cov.m0}}{list of covariance matrices of item parameters for null model.}
#'   \item{\code{cov.m1}}{list of covariance matrices of item parameters for alternative model.}
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
#' @seealso \code{\link[stats]{p.adjust}} \code{\link[nnet]{multinom}}
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
#' MLR(Data, group, key, type = "both")
#'
#' # testing uniform DDF effects
#' MLR(Data, group, key, type = "udif")
#'
#' # testing non-uniform DDF effects
#' MLR(Data, group, key, type = "nudif")
#' }
#'
#' @keywords DDF
#' @export
MLR <- function(Data, group, key, type = "both", match = "zscore", anchor = 1:ncol(Data), p.adjust.method = "none",
                alpha = 0.05, parametrization) {
  # deprecated args handling
  if (!missing(parametrization)) {
    warning("Argument 'parametrization' is deprecated; please use 'coef.difORD()' method for different parameterizations. ",
            call. = FALSE
    )
  }

  if (match[1] == "zscore") {
    x <- c(scale(.score(as.data.frame(Data[, anchor]), key[anchor])))
  } else {
    if (match[1] == "score") {
      x <- c(.score(as.data.frame(Data[, anchor]), key[anchor]))
    } else {
      if (length(match) == dim(Data)[1]) {
        x <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore', or vector of the same length as number
             of observations in 'Data'.", call. = FALSE)
      }
    }
  }

  m <- dim(Data)[2]

  m1 <- lapply(1:m, function(i) {
    switch(type,
      "both" = nnet::multinom(relevel(as.factor(Data[, i]),
        ref = paste(key[i])
      ) ~ x * group,
      trace = FALSE
      ),
      "nudif" = nnet::multinom(relevel(as.factor(Data[, i]),
        ref = paste(key[i])
      ) ~ x * group,
      trace = FALSE
      ),
      "udif" = nnet::multinom(relevel(as.factor(Data[, i]),
        ref = paste(key[i])
      ) ~ x + group,
      trace = FALSE
      )
    )
  })
  m0 <- lapply(1:m, function(i) {
    switch(type,
      "both" = nnet::multinom(relevel(as.factor(Data[, i]),
        ref = paste(key[i])
      ) ~ x,
      trace = FALSE
      ),
      "nudif" = nnet::multinom(relevel(as.factor(Data[, i]),
        ref = paste(key[i])
      ) ~ x + group,
      trace = FALSE
      ),
      "udif" = nnet::multinom(relevel(as.factor(Data[, i]),
        ref = paste(key[i])
      ) ~ x,
      trace = FALSE
      )
    )
  })

  MLRtest <- lapply(1:m, function(i) anova(m0[[i]], m1[[i]]))
  MLRstat <- sapply(1:m, function(i) c(MLRtest[[i]]$`LR stat.`[2], MLRtest[[i]]$`Pr(Chi)`[2]))
  df <- lapply(1:m, function(i) MLRtest[[i]]$`Resid. df`)

  adjusted.pval <- p.adjust(MLRstat[2, ], method = p.adjust.method)

  par.m1 <- lapply(m1, function(x) {
    if (is.null(dim(coef(x)))) {
      tmp <- matrix(coef(x), nrow = 1, dimnames = list(1, names(coef(x))))
      # rownames(tmp)[is.null(rownames(tmp))] <- 1
    } else {
      tmp <- coef(x)
    }
    tmp
  })
  par.m0 <- lapply(m0, function(x) {
    if (is.null(dim(coef(x)))) {
      tmp <- matrix(coef(x), nrow = 1, dimnames = list(1, names(coef(x))))
    } else {
      tmp <- coef(x)
    }
    tmp
  })

  cov.m1 <- lapply(m1, vcov)
  cov.m0 <- lapply(m0, vcov)

  se.m1 <- lapply(lapply(cov.m1, diag), sqrt)
  se.m0 <- lapply(lapply(cov.m0, diag), sqrt)

  ll.m0 <- sapply(m0, logLik)
  ll.m1 <- sapply(m1, logLik)

  AIC.m0 <- sapply(m0, AIC)
  AIC.m1 <- sapply(m1, AIC)

  BIC.m0 <- sapply(m0, BIC)
  BIC.m1 <- sapply(m1, BIC)

  results <- list(
    Sval = MLRstat[1, ],
    pval = MLRstat[2, ], adjusted.pval = adjusted.pval,
    df = df,
    par.m0 = par.m0, se.m0 = se.m0, cov.m0 = cov.m0,
    par.m1 = par.m1, se.m1 = se.m1, cov.m1 = cov.m1,
    ll.m0 = ll.m0, ll.m1 = ll.m1,
    AIC.m0 = AIC.m0, AIC.m1 = AIC.m1,
    BIC.m0 = BIC.m0, BIC.m1 = BIC.m1
  )
  return(results)
}

.deltamethod.MLR.log2irt <- function(par, cov) {
  cats <- rownames(par)
  n_cats <- length(cats)
  num_cats <- 1:n_cats

  par_tmp <- matrix(NA, nrow = nrow(par), ncol = 4,
                    dimnames = list(cats, c("(Intercept)", "x", "group", "x:group")))
  par_tmp[rownames(par), colnames(par)] <- par

  par_new <- matrix(cbind(
    -par_tmp[, "(Intercept)"] / par_tmp[, "x"],
    par_tmp[, "x"],
    (par_tmp[, "(Intercept)"] * par_tmp[, "x:group"] - par_tmp[, "x"] * par_tmp[, "group"]) / (par_tmp[, "x"]^2 + par_tmp[, "x"] * par_tmp[, "x:group"]),
    par_tmp[, "x:group"]
  ),
  ncol = 4,
  dimnames = list(cats, c("b", "a", "bDIF", "aDIF"))
  )

  nams <- paste(rep(cats, each = 4), c("(Intercept)", "x", "group", "x:group"), sep = ":")

  cov_tmp <- matrix(0, nrow = 4 * n_cats, ncol = 4 * n_cats,
                    dimnames = list(nams, nams))
  cov_tmp[rownames(cov), colnames(cov)] <- cov

  betas0 <- paste0("x", num_cats)
  betas1 <- paste0("x", num_cats + n_cats)
  betas2 <- paste0("x", num_cats + 2 * n_cats)
  betas3 <- paste0("x", num_cats + 3 * n_cats)

  ak <- betas1
  akDIF <- betas3
  bk <- paste0("-", betas0, "/", betas1)
  bkDIF <- paste0("(", betas0, "*", betas3, "-", betas1, "*", betas2, ")/(", betas1, "*(", betas1, "+", betas3, "))")

  formulas <- append(append(
    append(as.list(bk), as.list(ak)),
    as.list(bkDIF)
  ), as.list(akDIF))
  formulas <- lapply(formulas, function(x) paste0("~", x))
  formulas <- lapply(formulas, as.formula)
  formulas <- formulas[!is.na(par_new)]
  cov_new <- msm::deltamethod(
    formulas,
    par,
    cov,
    ses = FALSE
  )

  se_new <- sqrt(diag(cov_new))
  nams <- paste0(rep(colnames(par_new), each = n_cats), cats)[!is.na(par_new)]
  par_new <- par_new[, !colSums(!is.finite(par_new))]
  rownames(cov_new) <- colnames(cov_new) <- names(se_new) <- nams

  return(list(par = par_new, cov = cov_new, se = se_new))
}
