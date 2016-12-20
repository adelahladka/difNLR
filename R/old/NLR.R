#' Non-Linear Regression DIF statistic
#'
#' @aliases NLR
#'
#' @description Performs DIF detection procedure based on Non-Linear Regression and either F-test or likelihood ratio test of submodel.
#'
#' @param Data numeric: either binary data matrix only, or the binary data matrix plus the vector of group . See \strong{Details}.
#' @param group numeric: binary vector of group membership. "0" for reference group, "1" for focal group.
#' @param type character: type of DIF to be tested (either "both" (default), "udif", or "nudif"). See \strong{Details}.
#' @param p.adjust.method character: method for multiple comparison correction. See \strong{Details}.
#' @param start numeric: matrix with n rows (where n is the number of items) and at most 5 columns containing initial item parameters estimates. See \strong{Details}.
#' @param test character: test to be performed for DIF detection (either "F" (default), or "LR"). See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#'
#' @usage NLR(Data, group, type, start, p.adjust.method, test, alpha)
#'
#' @details
#' DIF detection procedure based on Non-Linear Regression is the extension of Logistic Regression procedure (Swaminathan and Rogers, 1990).
#'
#' The \code{data} is a matrix whose rows represents examinee answers ("1" correct, "0" incorrect) and columns correspond to the items. The \code{group} must be a vector of the same length as \code{nrow(data)}.
#'
#' The \code{type} corresponds to type of DIF to be tested. Possible values are \code{"both"} to detect any DIF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DIF or \code{"nudif"} to detect only non-uniform DIF.
#'
#' The \code{start} is a matrix with a number of rows equal to number of items. The number of columns correspond to number of parameters in model in alternative hypothesis (5 for values \code{"both"} and \code{"nudif"} in type, 4 for \code{"udif"} in type). If start missing, initial values are calculated by \code{startNLR} function.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the \code{stats} package. Possible values are \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as \code{NA} for both, \code{data} and \code{group} parameters.
#'
# ' @return A list of class 'difNLR' with the following arguments:
# ' \describe{
# '   \item{\code{DIF}}{either the column indicators of the items which were detected as DIF, or \code{"NONE"}.}
# '   \item{\code{test}}{the test used for DIF detection.}
# '   \item{\code{Sval}}{the values of \code{test} statistics.}
# '   \item{\code{pval}}{the p-values by \code{test}.}
# '   \item{\code{df}}{the degress of freedom of \code{test}.}
# '   \item{\code{coef}}{the matrix of estimated item parameters.}
# '   \item{\code{vcov}}{the list of estimated covariance matrices of item parameters.}
# '   \item{\code{group}}{the vector of group membership.}
# '   \item{\code{data}}{the binary data matrix.}
# '   \item{\code{type}}{character: type of DIF that was tested.}
# '   \item{\code{alpha}}{numeric: significance level.}
# '   \item{\code{conv.fail}}{numeric: number of convergence issues.}
# '   \item{\code{conv.fail.which}}{the indicators of the items which did not converge.}
# '   \item{\code{p.adjust.method}}{character: method for multiple comparison correction which was applied.}
# ' }
#'
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
#' Drabinova, A. and Martinkova P. (2016). Detection of Differenctial Item Functioning Based on Non-Linear Regression, Technical Report, V-1229, \url{http://hdl.handle.net/11104/0259498}.
#'
#' Swaminathan, H. and Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures. Journal of Educational Measurement, 27, 361-370.
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @examples
#' \dontrun{
#' # loading data based on GMAT
#' data(GMAT)
#'
#' data  <- GMAT[, colnames(GMAT) != "group"]
#' group <- GMAT[, "group"]
#' # Testing both DIF effects using F test and Benjamini-Hochberg correction (default)
#' NLR(Data, group)
#'
#' # Testing both DIF effects using likelihood-ratio test
#' NLR(Data, group, test = "LR")
#'
#' # Testing both DIF effects with none multiple comparison correction
#' NLR(Data, group, type = "both", p.adjust.method = "none")
#'
#' # Testing uniform DIF effects
#' NLR(Data, group, type = "udif")
#' # Testing non-uniform DIF effects
#' NLR(Data, group, type = "nudif")
#'
#' }
#' @keywords DIF
#' @export



NLR <- function(Data, group, type, start, p.adjust.method, test, alpha){

  NLRnudif <- deriv3( ~ c + (1 - c) / (1 + exp(-(a + aDif * group) * (x - (b + bDif * group)))),
                      namevec = c("a", "b", "c", "aDif", "bDif"),
                      function.arg = function(x, group, a, b, c, aDif, bDif){})
  NLRudif <- deriv3( ~ c + (1 - c) / (1 + exp(-a * (x - (b + bDif * group)))),
                     namevec = c("a", "b", "c", "bDif"),
                     function.arg = function(x, group, a, b, c, bDif){})
  NLRnodif <- deriv3( ~ c + (1 - c) / (1 + exp(-a * (x - b))),
                      namevec = c("a", "b", "c"),
                      function.arg = function(x, group, a, b, c){})

  start_m0 <- start
  start_m1 <- switch(type,
                     both = start[, -c(4, 5)],
                     nudif = start[, -4],
                     udif = start[, -4])

  k <- Inf
  x <- scale(apply(Data, 1, sum))
  m <- ncol(Data)
  n <- nrow(Data)
  m0 <- lapply(1:m, function(i) tryCatch(switch(type,
                                      both = nls(Data[, i] ~ NLRnudif(x, group, a, b, c, aDif, bDif),
                                                 algorithm = "port",
                                                 start = start_m0[i, ],
                                                 lower = c(-k, -k, 0, -k, -k),
                                                 upper = c(k, k, 1, k, k)),
                                      nudif = nls(Data[, i] ~ NLRnudif(x, group, a, b, c, aDif, bDif),
                                                  algorithm = "port",
                                                  start = start_m0[i, ],
                                                  lower = c(-k, -k, 0, -k, -k),
                                                  upper = c(k, k, 1, k, k)),
                                      udif = nls(Data[, i] ~ NLRudif(x, group, a, b, c, bDif),
                                                 algorithm = "port",
                                                 start = start_m0[i, ],
                                                 lower = c(-k, -k, 0, -k),
                                                 upper = c(k, k, 1, k))),
                                      error = function(e){cat("ERROR :",conditionMessage(e), "\n")}))
  m1 <- lapply(1:m, function(i) tryCatch(switch(type, both = nls(Data[, i] ~ NLRnodif(x, group, a, b, c),
                                                       algorithm = "port",
                                                       start = start_m1[i, ],
                                                       lower = c(-k, -k, 0),
                                                       upper = c(k, k, 1)),
                                      nudif = nls(Data[, i] ~ NLRudif(x, group, a, b, c, bDif),
                                                  algorithm = "port",
                                                  start = start_m1[i, ],
                                                  lower = c(-k, -k, 0, -k),
                                                  upper = c(k, k, 1, k)),
                                      udif = nls(Data[, i] ~ NLRnodif(x, group, a, b, c),
                                                 algorithm = "port",
                                                 start = start_m1[i, ],
                                                 lower = c(-k, -k, 0),
                                                 upper = c(k, k, 1))),
                                      error = function(e){cat("ERROR :",conditionMessage(e), "\n")}))
  conv.fail <- sum(is.na(m1) | is.na(m0))
  conv.fail.which <- which(is.na(m1) | is.na(m0))
  if (conv.fail > 0) {
    warning("Convergence failure")
  }

  if (test == "F"){
    pval <- Fval <- rep(NA, m)
    df <- switch(type,
                 both = c(2, n - 5),
                 udif = c(1, n - 4),
                 nudif = c(1, n - 5))
    Fval[which(!((is.na(m1)) | (is.na(m0))))] <- sapply(which(!((is.na(m1)) |  (is.na(m0)))),
                                                                    function(l) ((m1[[l]]$m$deviance() - m0[[l]]$m$deviance())/df[1])/(m0[[l]]$m$deviance()/df[2]))
    pval[which(!((is.na(m1)) | (is.na(m0))))] <- sapply(which(!((is.na(m1)) | (is.na(m0)))),
                                                                    function(l) (1 - pf(Fval[l], df[1], df[2])))
  } else {
    pval <- LRval <- rep(NA, m)
    df <- switch(type,
                 both = 2,
                 udif = 1,
                 nudif = 1)
    LRval[which(!((is.na(m1)) | (is.na(m0))))] <- sapply(which(!((is.na(m1)) |  (is.na(m0)))),
                                                                     function(l) -2 * c(logLik(m1[[l]]) - logLik(m0)))
    pval[which(!((is.na(m1)) | (is.na(m0))))] <- sapply(which(!((is.na(m1)) | (is.na(m0)))),
                                                                    function(l) (1 - pchisq(LRval[l], df)))
  }
  adjusted.pval <- p.adjust(pval, method = p.adjust.method)

  par.m1 <- t(sapply(m1[which(!is.na(m1))], coef))
  par.m0 <- t(sapply(m0[which(!is.na(m0))], coef))

  cov.m1 <- lapply(m1[which(!is.na(m1))], vcov)
  cov.m0 <- lapply(m0[which(!is.na(m0))], vcov)

  se.m1 <- sqrt(t(sapply(cov.m1, diag)))
  se.m0 <- sqrt(t(sapply(cov.m0, diag)))

  rownames(par.m1) <- rownames(par.m0) <- rownames(se.m1) <- rownames(se.m0) <- paste("Item", 1:m, sep = "")
  results <- list(Sval = switch(test, "F" = Fval, "LR" = LRval),
                  pval = pval, adjusted.pval = adjusted.pval,
                  df = df, test = test,
                  par.m0 = par.m0, se.m0 = se.m0, cov.m0 = cov.m0,
                  par.m1 = par.m1, se.m1 = se.m1, cov.m1 = cov.m1,
                  conv.fail = conv.fail, conv.fail.which = conv.fail.which)
  return(results)
}
