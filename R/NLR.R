#' Non-Linear Regression DIF statistic
#'
#' @aliases NLR
#'
#' @description Performs DIF detection procedure based on Non-Linear Regression and either F-test or likelihood ratio test of submodel.
#'
#' @param Data numeric: either binary data matrix only, or the binary data matrix plus the vector of group .
#' See \strong{Details}.
#' @param group numeric: binary vector of group membership. "0" for reference group, "1" for focal group.
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param type character: type of DIF to be tested (either "both" (default), "udif", or "nudif").
#' See \strong{Details}.
#' @param p.adjust.method character: method for multiple comparison correction. See \strong{Details}.
#' @param start numeric: matrix with n rows (where n is the number of items) and 8 columns containing initial
#' item parameters estimates. See \strong{Details}.
#' @param test character: test to be performed for DIF detection (either "LR" (default), or "F").
#' See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#'
#' @usage NLR(Data, group, model, type = "both", start,
#' p.adjust.method = "none", test = "LR", alpha = 0.05)
#'
#' @details
#' DIF detection procedure based on Non-Linear Regression is the extension
#' of Logistic Regression procedure (Swaminathan and Rogers, 1990).
#'
#' The \code{Data} is a matrix whose rows represents examinee scored answers
#' ("1" correct, "0" incorrect) and columns correspond to the items.
#' The \code{group} must be a vector of the same length as \code{nrow(data)}.
#'
#' The options of \code{model} are as follows: \code{Rasch} for one-parameter logistic model with
#' discrimination parameter fixed on value 1 for both groups, \code{1PL} for one-parameter logistic
#' model with discrimination parameter fixed for both groups, \code{2PL} for logistic regression model,
#' \code{3PLcg} for three-parameter logistic regression model with fixed guessing for both groups,
#' \code{3PLdg} for three-parameter logistic regression model with fixed inattention for both groups, or
#' \code{4PLcgdg} for four-parameter logistic regression model with fixed guessing and inattention
#' parameter for both groups.

#' The \code{type} corresponds to type of DIF to be tested. Possible values
#' are \code{"both"} to detect any DIF (uniform and/or non-uniform), \code{"udif"}
#' to detect only uniform DIF or \code{"nudif"} to detect only non-uniform DIF.
#'
#' The \code{start} is a matrix with a number of rows equal to number of items and with 8 columns.
#' First 4 columns represent parameters (a, b, c, d) of generalized logistic regression model
#' for reference group. Last 4 columns represent differences of parameters (aDif, bDif, cDif, dDif)
#' of generalized logistic regression model between reference and focal group.  If not specified, starting
#' values are calculated with \code{startNLR} function.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the
#' \code{stats} package. Possible values are \code{"holm"}, \code{"hochberg"},
#' \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"},
#' \code{"none"}.
#'
#' @return A list with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of \code{test} statistics.}
#'   \item{\code{pval}}{the p-values by \code{test}.}
#'   \item{\code{adjusted.pval}}{adjusted p-values by \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of \code{test}.}
#'   \item{\code{test}}{used test.}
#'   \item{\code{par.m0}}{the matrix of estimated item parameters for m0 model.}
#'   \item{\code{se.m0}}{the matrix of standard errors of item parameters for m0 model.}
#'   \item{\code{cov.m0}}{list of covariance matrices of item parameters for m0 model.}
#'   \item{\code{par.m1}}{the matrix of estimated item parameters for m1 model.}
#'   \item{\code{se.m1}}{the matrix of standard errors of item parameters for m1 model.}
#'   \item{\code{cov.m1}}{list of covariance matrices of item parameters for m1 model.}
#'   \item{\code{conv.fail}}{numeric: number of convergence issues.}
#'   \item{\code{conv.fail.which}}{the indicators of the items which did not converge.}
#'   \item{\code{ll.m0}}{log-likelihood of m0 model.}
#'   \item{\code{ll.m1}}{log-likelihood of m1 model.}
#'   \item{\code{AIC.m0}}{AIC of m0 model.}
#'   \item{\code{AIC.m1}}{AIC of m1 model.}
#'   \item{\code{BIC.m0}}{BIC of m0 model.}
#'   \item{\code{BIC.m1}}{BIC of m1 model.}
#' }
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' adela.drabinova@gmail.com \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' Karel Zvara \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' @references
#' Drabinova, A. & Martinkova P. (2016). Detection of Differenctial Item Functioning Based on Non-Linear Regression,
#' Technical Report, V-1229, \url{http://hdl.handle.net/11104/0259498}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27, 361-370.
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @examples
#' \dontrun{
#' # loading data based on GMAT
#' data(GMAT)
#'
#' Data  <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using LR test (default)
#' # and model with fixed guessing for both groups
#' NLR(Data, group, model = "3PLcg")
#'
#' # using F test
#' NLR(Data, group, model = "3PLcg", test = "F")
#'
#' # Testing both DIF effects with Benjamini-Hochberg correction
#' NLR(Data, group, model = "3PLcg", p.adjust.method = "BH")
#'
#' # Testing uniform DIF effects
#' NLR(Data, group, model = "3PLcg", type = "udif")
#' # Testing non-uniform DIF effects
#' NLR(Data, group, model = "3PLcg", type = "nudif")
#' }
#' @keywords DIF
#' @export
#' @importFrom stats AIC BIC logLik


NLR <- function(Data, group, model, type = "both", start,
                p.adjust.method = "none", test = "LR", alpha = 0.05){

  gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){
    return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
  }

  if(missing(start)){
    start <- startNLR(Data, group, model)
  }
  start_m0 <- start_m1 <- start
  start_m1[, "aDif"] <- 0
  if (!(type == "nudif")){
    start_m1[, "bDif"] <- 0
    if (type == "udif"){
      start_m0[, "aDif"] <- 0
    }
  }

  constr <- constrNLR(model = model, type = type)
  lowerM0 <- constr["lowerM0", ]; upperM0 <- constr["upperM0", ]
  lowerM1 <- constr["lowerM1", ]; upperM1 <- constr["upperM1", ]

  fixedM0 <- lowerM0[lowerM0 == upperM0]
  fixedM1 <- lowerM1[lowerM1 == upperM1]

  x <- scale(apply(Data, 1, sum))
  m <- ncol(Data)
  n <- nrow(Data)

  if (length(fixedM0) == 0){
    whM0 <- colnames(start_m0)
  } else {
    for (i in 1:length(fixedM0)){
      assign(names(fixedM0)[i], fixedM0[i])
    }
    whM0 <- colnames(start_m0)[(!(colnames(start_m0) %in% names(fixedM0)))]
    start_m0 <- structure(data.frame(start_m0[, whM0]), .Names = whM0)
    lowerM0 <- lowerM0[whM0]; upperM0 <- upperM0[whM0]
  }

  m0 <- lapply(1:m, function(i) tryCatch(nls(Data[, i] ~ gNLR(x, group, a, b, c, d, aDif, bDif, cDif, dDif),
                                             algorithm = "port",
                                             start = structure(start_m0[i, ], .Names = whM0),
                                             lower = lowerM0,
                                             upper = upperM0),
                                         error = function(e){cat("ERROR : ",
                                                                 conditionMessage(e), "\n")},
                                         finally = ""))

  if (length(fixedM0) != 0){
    for (i in 1:length(fixedM0)){
      rm(list = as.character(names(fixedM0)[i]))
    }
  }

  whM1 <- colnames(start_m1)[(!(colnames(start_m1) %in% names(fixedM1)))]
  start_m1 <- structure(data.frame(start_m1[, whM1]), .Names = whM1)

  lowerM1 <- lowerM0[whM1]; upperM1 <- upperM0[whM1]

  for (i in 1:length(fixedM1)){
    assign(names(fixedM1)[i], fixedM1[i])
  }

  m1 <- lapply(1:m, function(i) tryCatch(nls(Data[, i] ~ gNLR(x, group, a, b, c, d, aDif, bDif, cDif, dDif),
                                             algorithm = "port",
                                             start = structure(start_m1[i, ], .Names = whM1),
                                             lower = lowerM1,
                                             upper = upperM1),
                                         error = function(e){cat("ERROR : ",
                                                                 conditionMessage(e), "\n")},
                                         finally = ""))

  cfM0 <- unlist(lapply(m0, is.null)); cfM1 <- unlist(lapply(m1, is.null))
  conv.fail <- sum(cfM0, cfM1)
  conv.fail.which <- which(cfM0 | cfM1)

  if (conv.fail > 0) {
    warning("Convergence failure")
  }

  if (test == "F"){
    pval <- Fval <- rep(NA, m)
    df <- switch(type,
                 both = c(2, n - 5),
                 udif = c(1, n - 4),
                 nudif = c(1, n - 5))
    Fval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 |  cfM0)),
                                                        function(l) ((m1[[l]]$m$deviance() - m0[[l]]$m$deviance())/df[1])/(m0[[l]]$m$deviance()/df[2]))
    pval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 | cfM0)),
                                                        function(l) (1 - pf(Fval[l], df[1], df[2])))
  } else {
    pval <- LRval <- rep(NA, m)
    df <- switch(type,
                 both = 2,
                 udif = 1,
                 nudif = 1)
    LRval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 |  cfM0)),
                                                         function(l) -2 * c(logLik(m1[[l]]) - logLik(m0[[l]])))
    pval[which(!(cfM1 | cfM0))] <- sapply(which(!(cfM1 | cfM0)),
                                                        function(l) (1 - pchisq(LRval[l], df)))
  }

  ll.m0 <- ll.m1 <- lapply(1:m, function(i) NA)
  ll.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], logLik)
  ll.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], logLik)

  adjusted.pval <- p.adjust(pval, method = p.adjust.method)

  par.m1 <- se.m1 <- structure(data.frame(matrix(NA, nrow = m, ncol = length(lowerM1))), .Names = names(lowerM1))
  par.m0 <- se.m0 <- structure(data.frame(matrix(NA, nrow = m, ncol = length(lowerM0))), .Names = names(lowerM0))

  if (dim(par.m1)[2] == 1){
    par.m1[which(!cfM1), ] <- sapply(m1[which(!cfM1)], coef)
    par.m1 <- structure(data.frame(par.m1), names = unique(names(par.m1)))
  } else {
    par.m1[which(!cfM1), ] <- t(sapply(m1[which(!cfM1)], coef))
  }
  par.m0[which(!cfM0), ] <- t(sapply(m0[which(!cfM0)], coef))

  cov.m0 <- cov.m1 <- lapply(1:m, function(i) NA)
  cov.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], vcov)
  cov.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], vcov)


  if (dim(par.m1)[2] == 1){
    se.m1[which(!cfM1), ] <- sqrt(sapply(cov.m1[which(!cfM1)], diag))
    se.m1 <- structure(data.frame(se.m1), names = unique(names(se.m1)))
  } else {
    se.m1[which(!cfM1), ] <- sqrt(t(sapply(cov.m1[which(!cfM1)], diag)))
  }
  se.m0[which(!cfM0), ] <- sqrt(t(sapply(cov.m0[which(!cfM0)] , diag)))

  rownames(par.m1) <- rownames(par.m0) <- rownames(se.m1) <- rownames(se.m0) <- paste("Item", 1:m, sep = "")

  AIC.m0 <- AIC.m1 <- lapply(1:m, function(i) NA)
  AIC.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], AIC)
  AIC.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], AIC)

  BIC.m0 <- BIC.m1 <- lapply(1:m, function(i) NA)
  BIC.m0[which(!cfM0)] <- lapply(m0[which(!cfM0)], BIC)
  BIC.m1[which(!cfM1)] <- lapply(m1[which(!cfM1)], BIC)

  results <- list(Sval = switch(test, "F" = Fval, "LR" = LRval),
                  pval = pval, adjusted.pval = adjusted.pval,
                  df = df, test = test,
                  par.m0 = par.m0, se.m0 = se.m0, cov.m0 = cov.m0,
                  par.m1 = par.m1, se.m1 = se.m1, cov.m1 = cov.m1,
                  conv.fail = conv.fail, conv.fail.which = conv.fail.which,
                  ll.m0 = ll.m0, ll.m1 = ll.m1,
                  AIC.m0 = AIC.m0, AIC.m1 = AIC.m1,
                  BIC.m0 = BIC.m0, BIC.m1 = BIC.m1 )
  return(results)
}
