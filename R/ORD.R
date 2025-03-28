#' DIF likelihood ratio statistics for ordinal data.
#'
#' @aliases ORD
#'
#' @description Calculates DIF likelihood ratio statistics for ordinal
#'   data based either on adjacent category logit regression model or
#'   on cumulative logit regression model.
#'
#' @param Data data.frame or matrix: dataset which rows represent
#'   ordinally scored examinee answers and columns correspond to the
#'   items.
#' @param group numeric: binary vector of group membership. \code{"0"}
#'   for reference group, \code{"1"} for focal group.
#' @param model character: logistic regression model for ordinal data
#'   (either \code{"adjacent"} (default) or \code{"cumulative"}). See
#'   \strong{Details}.
#' @param type character: type of DIF to be tested. Either
#'   \code{"both"} for uniform and non-uniform DIF (i.e., difference
#'   in parameters \code{"a"} and \code{"b"}) (default), or
#'   \code{"udif"} for uniform DIF only (i.e., difference in
#'   difficulty parameter \code{"b"}), or \code{"nudif"} for
#'   non-uniform DIF only (i.e., difference in discrimination
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
#'   \code{\link[difNLR]{coef.difORD}} for different
#'   parameterizations.
#'
#' @usage
#' ORD(Data, group, model = "adjacent", type = "both", match = "zscore",
#'     anchor = 1:ncol(Data), p.adjust.method = "none",
#'     alpha = 0.05, parametrization)
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
#' @seealso \code{\link[stats]{p.adjust}} \code{\link[VGAM]{vglm}}
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(Anxiety, package = "ShinyItemAnalysis")
#' Data <- Anxiety[, paste0("R", 1:29)] # items
#' group <- Anxiety[, "gender"] # group membership variable
#'
#' # testing both DIF effects
#' ORD(Data, group, type = "both")
#'
#' # testing uniform DIF effects
#' ORD(Data, group, type = "udif")
#'
#' # testing non-uniform DIF effects
#' ORD(Data, group, type = "nudif")
#'
#' # testing DIF using cumulative logit model
#' ORD(Data, group, model = "cumulative")
#' }
#'
#' @keywords DIF
#' @export
ORD <- function(Data, group, model = "adjacent", type = "both", match = "zscore",
                anchor = 1:ncol(Data), p.adjust.method = "none",
                alpha = 0.05, parametrization) {
  # deprecated args handling
  if (!missing(parametrization)) {
    warning("Argument 'parametrization' is deprecated; please use 'coef.difORD()' method for different parameterizations. ",
      call. = FALSE
    )
  }

  m <- dim(Data)[2]

  for (i in 1:m) {
    Data[, i] <- as.numeric(paste(Data[, i]))
  }

  if (match[1] == "zscore") {
    x <- c(unlist(scale(rowSums(as.data.frame(Data[, anchor])))))
  } else {
    if (match[1] == "score") {
      x <- rowSums(as.data.frame(Data[, anchor]))
    } else {
      if (length(match) == dim(Data)[1]) {
        x <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of the same length as number
             of observations in 'Data'.", call. = FALSE)
      }
    }
  }

  # reordering data
  uval <- lapply(Data, function(x) sort(unique(x)))
  for (i in 1:dim(Data)[2]) {
    Data[, i] <- factor(Data[, i], levels = uval[[i]], ordered = TRUE)
  }

  if (model == "adjacent") {
    family <- VGAM::acat(reverse = FALSE, parallel = TRUE)
  } else {
    family <- VGAM::cumulative(reverse = TRUE, parallel = TRUE)
  }

  m1 <- lapply(1:m, function(i) {
    switch(type,
      "both"  = VGAM::vglm(Data[, i] ~ x * group, family = family),
      "nudif" = VGAM::vglm(Data[, i] ~ x * group, family = family),
      "udif"  = VGAM::vglm(Data[, i] ~ x + group, family = family)
    )
  })
  m0 <- lapply(1:m, function(i) {
    switch(type,
      "both"  = VGAM::vglm(Data[, i] ~ x, family = family),
      "nudif" = VGAM::vglm(Data[, i] ~ x + group, family = family),
      "udif"  = VGAM::vglm(Data[, i] ~ x, family = family)
    )
  })

  ORDtest <- lapply(1:m, function(i) VGAM::lrtest_vglm(m1[[i]], m0[[i]])@Body)
  ORDstat <- sapply(1:m, function(i) {
    c(
      ORDtest[[i]]$Chisq[2],
      ORDtest[[i]]$`Pr(>Chisq)`[2],
      ORDtest[[i]]$Df[2]
    )
  })
  adjusted.pval <- p.adjust(ORDstat[2, ], method = p.adjust.method)

  par.m0 <- lapply(m0, coef)
  par.m1 <- lapply(m1, coef)

  cov.m0 <- lapply(m0, vcov)
  cov.m1 <- lapply(m1, vcov)

  se.m0 <- lapply(lapply(cov.m0, diag), sqrt)
  se.m1 <- lapply(lapply(cov.m1, diag), sqrt)

  names(par.m0) <- names(par.m1) <- names(cov.m0) <- names(cov.m1) <- names(se.m0) <- names(se.m1) <- colnames(Data)[1:m]

  ll.m0 <- sapply(m0, logLik)
  ll.m1 <- sapply(m1, logLik)

  AIC.m0 <- sapply(m0, VGAM::AICvlm)
  AIC.m1 <- sapply(m1, VGAM::AICvlm)

  BIC.m0 <- sapply(m0, VGAM::BICvlm)
  BIC.m1 <- sapply(m1, VGAM::BICvlm)

  results <- list(
    Sval = ORDstat[1, ],
    pval = ORDstat[2, ], adjusted.pval = adjusted.pval,
    df = ORDstat[3, ],
    par.m0 = par.m0, se.m0 = se.m0, cov.m0 = cov.m0,
    par.m1 = par.m1, se.m1 = se.m1, cov.m1 = cov.m1,
    ll.m0 = ll.m0, ll.m1 = ll.m1,
    AIC.m0 = AIC.m0, AIC.m1 = AIC.m1,
    BIC.m0 = BIC.m0, BIC.m1 = BIC.m1
  )
  return(results)
}

#' @noRd
.deltamethod.ORD.log2irt <- function(par, cov) {
  if (sum((grepl("Intercept", names(par)))) == 1) {
    cats <- 1
    num_cats <- 1
  } else {
    cats <- as.numeric(paste(gsub("\\(Intercept\\)\\:", "", names(par)[(grepl("Intercept", names(par)))])))
    num_cats <- length(cats)
  }

  par_new <- setNames(
    c(
      -par[grepl("Intercept", names(par))] / par["x"],
      par["x"],
      (par[grepl("Intercept", names(par))] * par["x:group"] - par["x"] * par["group"]) / (par["x"]^2 + par["x"] * par["x:group"]),
      par["x:group"]
    ),
    c(paste0("b", as.numeric(paste(cats))), "a", paste0("bDIF", as.numeric(paste(cats))), "aDIF")
  )

  betas0 <- paste0("x", cats)
  beta1 <- paste0("x", num_cats + 1)
  beta2 <- paste0("x", num_cats + 2)
  beta3 <- paste0("x", num_cats + 3)

  a <- beta1
  aDIF <- beta3
  bk <- paste0("-", betas0, "/", beta1)
  bkDIF <- paste0("(", betas0, "*", beta3, "-", beta1, "*", beta2, ")/(", beta1, "*(", beta1, "+", beta3, "))")

  formulas <- append(append(
    append(as.list(bk), as.list(a)),
    as.list(bkDIF)
  ), as.list(aDIF))
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
  par_new <- par_new[!is.na(par_new)]
  nams <- names(par_new)
  rownames(cov_new) <- colnames(cov_new) <- names(se_new) <- nams
  return(list(par = par_new, cov = cov_new, se = se_new))
}

#' #' @noRd
#' .deltamethod.ORD.irt2log <- function(par, cov) {
#'   cats <- names(par)[grepl("b", names(par))]
#'   cats <- as.numeric(paste(gsub("b", "", cats[nchar(cats) < 4])))
#'   par_new <- c(
#'       -par["a"] * par[paste0("b", cats)],
#'       par["a"],
#'       # -par["aDIF"] * par[paste0("b", cats)] - par["a"] * par[paste0("bDIF", cats)] - par["aDIF"] * par[paste0("bDIF", cats)],
#'       par["tmp"],
#'       par["aDIF"]
#'     )
#'   num_cats <- length(cats)
#'   # duplicates <- duplicated(round(par_new, 11))
#'
#'   bk <- paste0("x", 1:num_cats)
#'   tmp <- paste0("x", 2 * num_cats + 3)
#'   a <- paste0("x", num_cats + 1)
#'   aDIF <- paste0("x", 2 * num_cats + 2)
#'
#'   betas0 <- paste0("-", a, "*", bk)
#'   beta1 <- a
#'   # betas2 <- paste0("-", aDIF, "*", bk, "-", a, "*", bkDIF, "-", aDIF, "*", bkDIF)
#'   beta2 <- tmp
#'   beta3 <- aDIF
#'
#'   formulas <- append(append(
#'     append(as.list(betas0), as.list(beta1)),
#'     as.list(beta2)
#'   ), as.list(beta3))
#'   formulas <- lapply(formulas, function(x) paste0("~", x))
#'   formulas <- lapply(formulas, as.formula)
#'   formulas <- formulas[!is.na(par_new)]
#'   cov_new <- msm::deltamethod(
#'     formulas,
#'     par,
#'     cov,
#'     ses = FALSE
#'   )
#'   cov_new <- cov_new#[!duplicates, !duplicates]
#'   se_new <- sqrt(diag(cov_new))
#'
#'   # SE for group is not well defined
#'   # cov_new <- cov_new[!is.na(par_new), !is.na(par_new)]
#'   # par_new <- par_new[!duplicates]
#'   names(par_new) <- c(paste0("(Intercept):", cats), "x", "group", "x:group")
#'   par_new <- par_new[!is.na(par_new)]
#'   nams <- names(par_new)
#'   rownames(cov_new) <- colnames(cov_new) <- names(se_new) <- nams
#'   return(list(par = par_new, cov = cov_new, se = se_new))
#' }
