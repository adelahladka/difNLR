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
#' @param anchor a vector of integers specifying which items are currently considered as anchor (DDF free) items. By
#' default, all items are considered as anchors. Argument is ignored if \code{match} is not \code{"zscore"} or \code{"score"}.
#' See \strong{Details}.
#' @param p.adjust.method character: method for multiple comparison correction.
#' See \strong{Details}.
#' @param parametrization character: parametrization of regression coefficients. Possible options are
#' \code{"irt"} (default) and \code{"classic"}. See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#'
#' @usage ORD(Data, group, model = "adjacent", type = "both", match = "zscore",
#' anchor = 1:ncol(Data), p.adjust.method = "none", parametrization = "irt",
#' alpha = 0.05)
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
#' Argument \code{parametrization} is a character which specifies parametrization of regression parameters. Default option
#' is \code{"irt"} which returns IRT parametrization (difficulty-discrimination). Option \code{"classic"} returns
#' intercept-slope parametrization with effect of group membership and interaction with matching criterion.
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
#' @seealso \code{\link[stats]{p.adjust}} \code{\link[VGAM]{vglm}}
#'
#' @examples
#' \dontrun{
#' # loading data
#' data(dataMedicalgraded, package = "ShinyItemAnalysis")
#' df <- dataMedicalgraded[, c(1:5, 101)]
#' df <- df[complete.cases(df), ]
#'
#' Data <- df[, 1:5]
#' group <- df[, 6]
#'
#' # Testing both DDF effects
#' ORD(Data, group, type = "both")
#'
#' # Testing uniform DDF effects
#' ORD(Data, group, type = "udif")
#'
#' # Testing non-uniform DDF effects
#' ORD(Data, group, type = "nudif")
#'
#' # Testing DDF using cumulative logit model
#' ORD(Data, group, model = "cumulative")
#' }
#'
#' @keywords DDF
#' @export
ORD <- function(Data, group, model = "adjacent", type = "both", match = "zscore",
                anchor = 1:ncol(Data), p.adjust.method = "none",
                parametrization = "irt", alpha = 0.05) {
  for (i in 1:dim(Data)[2]) {
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
             of observations in 'Data'!")
      }
    }
  }

  m <- dim(Data)[2]
  n <- dim(Data)[1]

  uval <- lapply(Data, function(x) sort(unique(x)))
  for (i in 1:dim(Data)[2]) {
    Data[, i] <- factor(Data[, i], levels = uval[[i]], ordered = T)
  }

  if (model == "adjacent") {
    family <- VGAM::acat(reverse = FALSE, parallel = TRUE)
  } else {
    family <- VGAM::cumulative(reverse = TRUE, parallel = TRUE)
  }

  m0 <- lapply(1:m, function(i) {
    switch(type,
      "both"  = VGAM::vglm(Data[, i] ~ x * group, family = family),
      "nudif" = VGAM::vglm(Data[, i] ~ x * group, family = family),
      "udif"  = VGAM::vglm(Data[, i] ~ x + group, family = family)
    )
  })
  m1 <- lapply(1:m, function(i) {
    switch(type,
      "both"  = VGAM::vglm(Data[, i] ~ x, family = family),
      "nudif" = VGAM::vglm(Data[, i] ~ x + group, family = family),
      "udif"  = VGAM::vglm(Data[, i] ~ x, family = family)
    )
  })

  ORDtest <- lapply(1:m, function(i) VGAM::lrtest_vglm(m0[[i]], m1[[i]])@Body)
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

  cats <- lapply(Data, function(x) sort(unique(x))[-1])

  if (parametrization == "irt") {
    b0s <- lapply(par.m0, function(x) names(x)[grepl("Intercept", names(x))])
    num.b0s <- sapply(b0s, length)

    par.m0.tmp <- lapply(1:m, function(i) c(par.m0[[i]], rep(0, num.b0s[i] + 3 - length(par.m0[[i]]))))
    par.m0.tmp <- lapply(1:m, function(i) {
      names(par.m0.tmp[[i]])[-c(1:num.b0s[[i]])] <- c("x", "group", "x:group")
      par.m0.tmp[[i]]
    })
    par.m1.tmp <- lapply(1:m, function(i) c(par.m1[[i]], rep(0, num.b0s[i] + 3 - length(par.m1[[i]]))))
    par.m1.tmp <- lapply(1:m, function(i) {
      names(par.m1.tmp[[i]])[-c(1:num.b0s[[i]])] <- c("x", "group", "x:group")
      par.m1.tmp[[i]]
    })


    par.m0.delta <- lapply(par.m0.tmp, function(x) {
      c(
        -x[grepl("Intercept", names(x))] / x["x"],
        x["x"],
        (x[grepl("Intercept", names(x))] * x["x:group"] - x["x"] * x["group"]) / (x["x"]^2 + x["x"] * x["x:group"]),
        x["x:group"]
      )
    })
    par.m1.delta <- lapply(par.m1.tmp, function(x) {
      c(
        -x[grepl("Intercept", names(x))] / x["x"],
        x["x"],
        (x[grepl("Intercept", names(x))] * x["x:group"] - x["x"] * x["group"]) / (x["x"]^2 + x["x"] * x["x:group"]),
        x["x:group"]
      )
    })

    nams.m0 <- lapply(par.m0.tmp, names)
    nams.m1 <- lapply(par.m1.tmp, names)

    nams.cov.m0 <- lapply(cov.m0, rownames)
    nams.cov.m1 <- lapply(cov.m1, rownames)

    nams.add.m0 <- lapply(1:m, function(i) c(nams.m0[[i]][nams.m0[[i]] %in% nams.cov.m0[[i]]], nams.m0[[i]][!nams.m0[[i]] %in% nams.cov.m0[[i]]]))
    nams.add.m1 <- lapply(1:m, function(i) c(nams.m1[[i]][nams.m1[[i]] %in% nams.cov.m1[[i]]], nams.m1[[i]][!nams.m1[[i]] %in% nams.cov.m1[[i]]]))

    cov.m0.tmp <- lapply(1:m, function(i) {
      rbind(
        cbind(cov.m0[[i]], matrix(0, nrow = nrow(cov.m0[[i]]), ncol = num.b0s[i] + 3 - nrow(cov.m0[[i]]))),
        matrix(0, nrow = num.b0s[i] + 3 - nrow(cov.m0[[i]]), ncol = num.b0s[i] + 3)
      )
    })
    cov.m0.tmp <- lapply(1:m, function(i) {
      colnames(cov.m0.tmp[[i]]) <- rownames(cov.m0.tmp[[i]]) <- nams.add.m0[[i]]
      cov.m0.tmp[[i]]
    })
    cov.m1.tmp <- lapply(1:m, function(i) {
      rbind(
        cbind(cov.m1[[i]], matrix(0, nrow = nrow(cov.m1[[i]]), ncol = num.b0s[i] + 3 - nrow(cov.m1[[i]]))),
        matrix(0, nrow = num.b0s[i] + 3 - nrow(cov.m1[[i]]), ncol = num.b0s[i] + 3)
      )
    })
    cov.m1.tmp <- lapply(1:m, function(i) {
      colnames(cov.m1.tmp[[i]]) <- rownames(cov.m1.tmp[[i]]) <- nams.add.m1[[i]]
      cov.m1.tmp[[i]]
    })


    se.m0 <- lapply(1:m, function(i) {
      c(
        sapply(1:num.b0s[[i]], function(j) {
          deltamethod(
            ~ -x1 / x2,
            par.m0.tmp[[i]][c(b0s[[i]][j], "x")],
            cov.m0.tmp[[i]][c(b0s[[i]][j], "x"), c(b0s[[i]][j], "x")]
          )
        }),
        sqrt(cov.m0.tmp[[i]]["x", "x"]),
        sapply(1:num.b0s[[i]], function(j) {
          deltamethod(
            ~ (x1 * x4 - x2 * x3) / (x2 * x2 + x2 * x4),
            par.m0.tmp[[i]][c(b0s[[i]][j], "x", "group", "x:group")],
            cov.m0.tmp[[i]][c(b0s[[i]][j], "x", "group", "x:group"), c(b0s[[i]][j], "x", "group", "x:group")]
          )
        }),
        sqrt(cov.m0.tmp[[i]]["x:group", "x:group"])
      )
    })

    se.m1 <- lapply(1:m, function(i) {
      c(
        sapply(1:num.b0s[[i]], function(j) {
          deltamethod(
            ~ -x1 / x2,
            par.m1.tmp[[i]][c(b0s[[i]][j], "x")],
            cov.m1.tmp[[i]][c(b0s[[i]][j], "x"), c(b0s[[i]][j], "x")]
          )
        }),
        sqrt(cov.m1.tmp[[i]]["x", "x"]),
        sapply(1:num.b0s[[i]], function(j) {
          deltamethod(
            ~ (x1 * x4 - x2 * x3) / (x2 * x2 + x2 * x4),
            par.m1.tmp[[i]][c(b0s[[i]][j], "x", "group", "x:group")],
            cov.m1.tmp[[i]][c(b0s[[i]][j], "x", "group", "x:group"), c(b0s[[i]][j], "x", "group", "x:group")]
          )
        }),
        sqrt(cov.m1.tmp[[i]]["x:group", "x:group"])
      )
    })


    se.m0 <- lapply(1:m, function(i) {
      names(se.m0[[i]]) <- c(paste0("b", as.numeric(paste(cats[[i]]))), "a", paste0("bDIF", as.numeric(paste(cats[[i]]))), "aDIF")
      se.m0[[i]]
    })
    se.m1 <- lapply(1:m, function(i) {
      names(se.m1[[i]]) <- c(paste0("b", as.numeric(paste(cats[[i]]))), "a", paste0("bDIF", as.numeric(paste(cats[[i]]))), "aDIF")
      se.m1[[i]]
    })

    par.m0 <- par.m0.delta
    par.m1 <- par.m1.delta
    par.m0 <- lapply(1:m, function(i) {
      names(par.m0[[i]]) <- c(paste0("b", as.numeric(paste(cats[[i]]))), "a", paste0("bDIF", as.numeric(paste(cats[[i]]))), "aDIF")
      par.m0[[i]]
    })
    par.m1 <- lapply(1:m, function(i) {
      names(par.m1[[i]]) <- c(paste0("b", as.numeric(paste(cats[[i]]))), "a", paste0("bDIF", as.numeric(paste(cats[[i]]))), "aDIF")
      par.m1[[i]]
    })
  } else {
    se.m1 <- lapply(lapply(cov.m1, diag), sqrt)
    se.m0 <- lapply(lapply(cov.m0, diag), sqrt)

    se.m0 <- lapply(1:m, function(i) {
      names(se.m0[[i]]) <- c(paste0("(Intercept):", as.numeric(paste(cats[[i]]))), "x", "group", "x:group")[1:length(se.m0[[i]])]
      se.m0[[i]]
    })
    se.m1 <- lapply(1:m, function(i) {
      names(se.m1[[i]]) <- c(paste0("(Intercept):", as.numeric(paste(cats[[i]]))), "x", "group", "x:group")[1:length(se.m1[[i]])]
      se.m1[[i]]
    })

    par.m0 <- lapply(1:m, function(i) {
      names(par.m0[[i]]) <- c(paste0("(Intercept):", as.numeric(paste(cats[[i]]))), "x", "group", "x:group")[1:length(se.m0[[i]])]
      par.m0[[i]]
    })
    par.m1 <- lapply(1:m, function(i) {
      names(par.m1[[i]]) <- c(paste0("(Intercept):", as.numeric(paste(cats[[i]]))), "x", "group", "x:group")[1:length(se.m1[[i]])]
      par.m1[[i]]
    })
  }

  names(par.m0) <- names(par.m1) <- names(se.m0) <- names(par.m1) <- colnames(Data)[1:m]

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
    par.m0 = par.m0, se.m0 = se.m0,
    par.m1 = par.m1, se.m1 = se.m1,
    ll.m0 = ll.m0, ll.m1 = ll.m1,
    AIC.m0 = AIC.m0, AIC.m1 = AIC.m1,
    BIC.m0 = BIC.m0, BIC.m1 = BIC.m1
  )
  return(results)
}
