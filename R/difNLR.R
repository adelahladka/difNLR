#' Performs DIF detection using Non-Linear Regression method.
#'
#' @aliases difNLR print.difNLR plot.difNLR fitted.difNLR predict.difNLR
#'
#' @description Performs DIF detection procedure based on Non-Linear Regression and either F-test or likelihood ratio test of submodel.
#' @param data numeric: binary data matrix. See \strong{Details}.
#' @param group numeric: binary vector of group membership. "0" for reference group, "1" for focal group.
#' @param type character: type of DIF to be tested (either "both" (default), "udif", or "nudif"). See \strong{Details}.
#' @param p.adjust.method character: method for multiple comparison correction. See \strong{Details}.
#' @param start numeric: matrix with n rows (where n is the number of items) and at most 5 columns containing initial item parameters estimates. See \strong{Details}.
#' @param test character: test to be performed for DIF detection (either "F" (default), or "LR"). See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#' @param x an object of 'difNLR' class
#' @param object an object of 'difNLR' class
#' @param plot.type character: type of plot to be plotted (either "cc" for characteristic curve (default), or "stat" for test statistics). See \strong{Details}.
#' @param item either character ("all"), or numeric vector, or single number corresponding to column indicators. See \strong{Details}.
#' @param col character: single value, or vector of two values representing colors for plot.
#' @param shape integer: shape parameter for plot.
#' @param size numeric: single number, or vector of two numbers representing line width in plot.
#' @param linetype numeric: single number, or vector of two numbers representing line type in plot for reference and focal group.
#' @param title string: title of plot.
#' @param score numeric: standardized total score of subject.
#' @param ... other generic parameters for \code{plot}, \code{fitted} or \code{predict} functions.
#'
#' @usage difNLR(data, group,  type = "both", p.adjust.method = "BH",
#'   start, test = "F", alpha = 0.05)
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
#' The output of the difNLR is displayed by the \code{print.difNLR} function.
#'
#' Two types of plots are available. The first one is obtained by setting \code{plot.type = "cc"} (default). The characteristic curve for item specified in item option is plotted. For default option \code{"all"} of item, characteristic curves of all converged items are plotted. The drawn curves represent best model.
#' The second plot is obtained by setting \code{plot.type = "stat"}. The  test statistics (either F-test, or LR-test, depends on argument \code{test}) are displayed on the Y axis, for each coverged item. The detection threshold is displayed by a horizontal line and items detected as DIF are printed with the red color. Only parameters \code{size} and \code{title} are used.
#'
#' Fitted values are extracted by the \code{fitted.difNLR} function for item(s) specified in \code{item} argument.
#'
#' Predicted values are produced by the \code{predict.difNLR} function for item(s) specified in \code{item} argument. \code{score} represents standardized total score of new subject and \code{group} argument represents group membership of new subject.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as \code{NA} for both, \code{data} and \code{group} parameters.
#'
#' @return A list of class 'difNLR' with the following arguments:
#' \describe{
#'   \item{\code{DIF}}{either the column indicators of the items which were detected as DIF, or \code{"NONE"}.}
#'   \item{\code{test}}{the test used for DIF detection.}
#'   \item{\code{Sval}}{the values of \code{test} statistics.}
#'   \item{\code{pval}}{the p-values by \code{test}.}
#'   \item{\code{df}}{the degress of freedom of \code{test}.}
#'   \item{\code{coef}}{the matrix of estimated item parameters.}
#'   \item{\code{vcov}}{the list of estimated covariance matrices of item parameters.}
#'   \item{\code{group}}{the vector of group membership.}
#'   \item{\code{data}}{the binary data matrix.}
#'   \item{\code{type}}{character: type of DIF that was tested.}
#'   \item{\code{alpha}}{numeric: significance level.}
#'   \item{\code{conv.fail}}{numeric: number of convergence issues.}
#'   \item{\code{conv.fail.which}}{the indicators of the items which did not converge.}
#'   \item{\code{p.adjust.method}}{character: method for multiple comparison correction which was applied.}
#' }
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
#' x <- difNLR(data, group)
#'
#' # Testing both DIF effects using likelihood-ratio test
#' x <- difNLR(data, group, test = "LR")
#'
#' # Testing both DIF effects with none multiple comparison correction
#' difNLR(data, group, type = "both", p.adjust.method = "none")
#'
#' # Testing uniform DIF effects
#' difNLR(data, group, type = "udif")
#' # Testing non-uniform DIF effects
#' difNLR(data, group, type = "nudif")
#'
#' # Graphical devices
#' plot(x)
#' plot(x, item = x$DIF)
#' plot(x, plot.type = "stat")
#'
#' # Fitted values
#' fitted(x)
#' fitted(x, item = 1)
#' # Predicted values
#' predict(x)
#' predict(x, item = 1)
#'
#' # Predicted values for new subjects
#' predict(x, item = 1, score = 0, group = 1)
#' predict(x, item = 1, score = 0, group = 0)
#' }
#' @keywords DIF
#' @export


difNLR <- function(data, group, type = "both", p.adjust.method = "BH", start,
                    test = "F", alpha = 0.05)
{
  if (length(levels(as.factor(group))) != 2)
    stop("'group' must be binary vector", call. = FALSE)
  if (length(levels(as.factor(group))) != 2)
    stop("'group' must be binary vector", call. = FALSE)
  if (is.matrix(data) | is.data.frame(data)) {
    if (!all(apply(data, 2, function(i) {
      length(levels(as.factor(i))) == 2
    })))
      stop("'data' must be data frame or matrix of binary vectors",
           call. = FALSE)
    if (nrow(data) != length(group))
      stop("'data' must have the same number of rows as is length of vector 'group'",
           call. = FALSE)
  }
  else {
    stop("'data' must be data frame or matrix of binary vectors",
         call. = FALSE)
  }
  if (!type %in% c("udif", "nudif", "both") | !is.character(type))
    stop("'type' must be either 'udif', 'nudif' or 'both'",
         call. = FALSE)
  if (!test %in% c("F", "LR") | !is.character(type))
    stop("'test' must be either 'F' or 'LR'",
         call. = FALSE)
  if (alpha > 1 | alpha < 0)
    stop("'alpha' must be between 0 and 1",
         call. = FALSE)
  if (missing(start)) {
    start <- startNLR(data, group)
    start_m0 <- switch(type,
                       both = start,
                       nudif = start,
                       udif = start[, -4])
    start_m1 <- switch(type,
                       both = start[, -c(4, 5)],
                       nudif = start[, -4],
                       udif = start[, -c(4, 5)])
  }
  else {
    if (ncol(start) != 5 & type != "udif")
      stop("'start' must be data frame or matrix with 5 columns",
           call. = FALSE)
    if (ncol(start) != 4 & type == "udif")
      stop("'start' must be data frame or matrix with 4 columns for detecting uniform DIF",
           call. = FALSE)
    if (nrow(start) != ncol(data))
      stop("'start' must be data frame or matrix with starting values for each item",
           call. = FALSE)
    start_m0 <- start
    start_m1 <- switch(type,
                       both = start[, -c(4, 5)],
                       nudif = start[, -4],
                       udif = start[, -4])
  }

  # NA values
  DATA <- data.frame(data, group)
  DATA <- na.omit(DATA)
  data <- DATA[, !(names(DATA) %in% "group")]
  group <- DATA$group

  regFce_nonUDIF <- deriv3( ~ c + (1 - c) / (1 + exp(-(a + aDif * group) * (x - (b + bDif * group)))),
                            namevec = c("a", "b", "c", "aDif", "bDif"),
                            function.arg = function(x, group, a, b, c, aDif, bDif){})
  regFce_UDIF    <- deriv3( ~ c + (1 - c) / (1 + exp(-a * (x - (b + bDif * group)))),
                            namevec = c("a", "b", "c", "bDif"),
                            function.arg = function(x, group, a, b, c, bDif){})
  regFce_noDIF   <- deriv3( ~ c + (1 - c) / (1 + exp(-a * (x - b))),
                            namevec = c("a", "b", "c"),
                            function.arg = function(x, group, a, b, c){})

  stand_total_score <- c(scale(apply(data, 1, sum)))
  m <- ncol(data)
  n <- nrow(data)
  conv.fail <- 0
  k <- Inf
  estim_m0 <- lapply(1:m, function(i) NA)
  hv <- which(!(sapply(1:m, function(i) is(try(switch(type,
                                                      both = nls(data[, i] ~ regFce_nonUDIF(stand_total_score, group, a, b, c, aDif, bDif),
                                                                 algorithm = "port",
                                                                 start = start_m0[i, ],
                                                                 lower = c(-k, -k, 0, -k, -k),
                                                                 upper = c(k, k, 1, k, k)),
                                                      nudif = nls(data[, i] ~ regFce_nonUDIF(stand_total_score, group, a, b, c, aDif, bDif),
                                                                  algorithm = "port",
                                                                  start = start_m0[i, ],
                                                                  lower = c(-k, -k, 0, -k, -k),
                                                                  upper = c(k, k, 1, k, k)),
                                                      udif = nls(data[, i] ~ regFce_UDIF(stand_total_score, group, a, b, c, bDif),
                                                                 algorithm = "port",
                                                                 start = start_m0[i, ],
                                                                 lower = c(-k, -k, 0, -k),
                                                                 upper = c(k, k, 1, k))),
                                               silent = T), "try-error"))))
  estim_m0[hv] <- lapply(hv, function(i) switch(type,
                                                both = nls(data[, i] ~ regFce_nonUDIF(stand_total_score, group, a, b, c, aDif, bDif),
                                                           algorithm = "port",
                                                           start = start_m0[i, ],
                                                           lower = c(-k, -k, 0, -k, -k),
                                                           upper = c(k, k, 1, k, k)),
                                                nudif = nls(data[, i] ~ regFce_nonUDIF(stand_total_score, group, a, b, c, aDif, bDif),
                                                            algorithm = "port",
                                                            start = start_m0[i, ],
                                                            lower = c(-k, -k, 0, -k, -k),
                                                            upper = c(k, k, 1, k, k)),
                                                udif = nls(data[, i] ~ regFce_UDIF(stand_total_score, group, a, b, c, bDif),
                                                           algorithm = "port",
                                                           start = start_m0[i, ],
                                                           lower = c(-k, -k, 0, -k),
                                                           upper = c(k, k, 1, k))))
  estim_m1 <- lapply(1:m, function(i) NA)
  hv <- which(!(sapply(1:m, function(i) is(try(switch(type,
                                                      both = nls(data[, i] ~ regFce_noDIF(stand_total_score, group, a, b, c),
                                                                 algorithm = "port",
                                                                 start = start_m1[i, ],
                                                                 lower = c(-k, -k, 0),
                                                                 upper = c(k, k, 1)),
                                                      nudif = nls(data[, i] ~ regFce_UDIF(stand_total_score, group, a, b, c, bDif),
                                                                  algorithm = "port",
                                                                  start = start_m1[i, ],
                                                                  lower = c(-k, -k, 0, -k),
                                                                  upper = c(k, k, 1, k)),
                                                      udif = nls(data[, i] ~ regFce_noDIF(stand_total_score, group, a, b, c),
                                                                 algorithm = "port", start = start_m1[i, ],
                                                                 lower = c(-k, -k, 0),
                                                                 upper = c(k, k, 1))),
                                               silent = T),
                                           "try-error"))))
  estim_m1[hv] <- lapply(hv, function(i) switch(type, both = nls(data[, i] ~ regFce_noDIF(stand_total_score, group, a, b, c),
                                                                 algorithm = "port",
                                                                 start = start_m1[i, ],
                                                                 lower = c(-k, -k, 0),
                                                                 upper = c(k, k, 1)),
                                                nudif = nls(data[, i] ~ regFce_UDIF(stand_total_score, group, a, b, c, bDif),
                                                            algorithm = "port",
                                                            start = start_m1[i, ],
                                                            lower = c(-k, -k, 0, -k),
                                                            upper = c(k, k, 1, k)),
                                                udif = nls(data[, i] ~ regFce_noDIF(stand_total_score, group, a, b, c),
                                                           algorithm = "port",
                                                           start = start_m1[i, ],
                                                           lower = c(-k, -k, 0),
                                                           upper = c(k, k, 1))))

  conv.fail <- conv.fail + sum(is.na(estim_m1) | is.na(estim_m0))
  conv.fail.which <- which(is.na(estim_m1) | is.na(estim_m0))
  if (conv.fail > 0) {
    warning("Convergence failure")
  }

  if (test == "F"){
    pval <- Fval <- rep(NA, m)
    df <- switch(type,
                 both = c(2, n - 5),
                 udif = c(1, n - 4),
                 nudif = c(1, n - 5))
    Fval[which(!((is.na(estim_m1)) | (is.na(estim_m0))))] <- sapply(which(!((is.na(estim_m1)) |  (is.na(estim_m0)))),
                                                                    function(l) ((estim_m1[[l]]$m$deviance() - estim_m0[[l]]$m$deviance())/df[1])/(estim_m0[[l]]$m$deviance()/df[2]))
    pval[which(!((is.na(estim_m1)) | (is.na(estim_m0))))] <- sapply(which(!((is.na(estim_m1)) | (is.na(estim_m0)))),
                                                                    function(l) (1 - pf(Fval[l], df[1], df[2])))
  } else {
    pval <- LRval <- rep(NA, m)
    df <- switch(type,
                 both = 2,
                 udif = 1,
                 nudif = 1)
    LRval[which(!((is.na(estim_m1)) | (is.na(estim_m0))))] <- sapply(which(!((is.na(estim_m1)) |  (is.na(estim_m0)))),
                                                                    function(l) -2 * c(logLik(estim_m1[[l]]) - logLik(estim_m0[[l]])))
    pval[which(!((is.na(estim_m1)) | (is.na(estim_m0))))] <- sapply(which(!((is.na(estim_m1)) | (is.na(estim_m0)))),
                                                                    function(l) (1 - pchisq(LRval[l], df)))
  }
  pval_adj <- p.adjust(pval, method = p.adjust.method)
  significant <- which(pval_adj < alpha)
  if (length(significant) > 0) {
    DIF <- significant
  }
  else {
    DIF <- "NONE"
  }

  coef <- ifelse(pval_adj < alpha, lapply(estim_m0[which(!is.na(estim_m0))], coef),
                                  lapply(estim_m1[which(!is.na(estim_m1))], coef))
  vcov <- ifelse(pval_adj < alpha, lapply(estim_m0[which(!is.na(estim_m0))], vcov),
                                  lapply(estim_m1[which(!is.na(estim_m1))], vcov))

  mat <- t(sapply(coef, "[", i = 1:5))
  mat[is.na(mat)] <- 0
  coef <- switch(type, both = mat, nudif = mat[, c(1:3, 5, 4)], udif = mat[, c(1:3, 5, 4)])
  colnames(coef) <- c(letters[1:3], "aDif", "bDif")
  results <- list(DIF = DIF,
                  test = test, Sval = switch(test, "F" = Fval, "LR" = LRval), Pval = pval_adj, df = df,
                  coef = coef, vcov = vcov,
                  group = group, data = data, type = type, alpha = alpha,
                  conv.fail = conv.fail, conv.fail.which = conv.fail.which,
                  p.adjust.method = p.adjust.method)
  class(results) <- "difNLR"
  results
}


#' @rdname difNLR
#' @export
print.difNLR <- function (x, ...){
  title <- switch(x$type,
                  both = "Detection of both types of Differential Item Functioning using Non-Linear Regression method",
                  udif = "Detection of uniform Differential Item Functioning using Non-Linear Regression method",
                  nudif = "Detection of non-uniform Differential Item Functioning using Non-Linear Regression method")
  cat(paste(strwrap(title, width = 60), collapse = "\n"))
  cat(switch(x$test,
             "F" = "\n\nNon-linear regression F-test statistics\n",
             "LR" = "\n\nNon-linear regression Likelihood Ratio chi-square statistics\n"))
  if (x$p.adjust.method == "none") {
    cat("p-values with none multiple comparison correction\n\n")
  }
  else {
    cat(paste("p-values adjusted using", switch(x$p.adjust.method,
                                                holm = "Holm", hochberg = "Hochberg", hommel = "Hommel",
                                                bonferroni = "Bonferroni", BH = "Benjamini-Hochberg",
                                                BY = "Benjamini-Yekutieli", fdr = "FDR"), "method\n\n"))
  }
  tab <- format(round(cbind(x$Sval, x$Pval), 4))
  sign <- ifelse(is.na(x$Pval), " ", ifelse(x$Pval < 0.001,
                                            "***", ifelse(x$Pval < 0.01, "**", ifelse(x$Pval < 0.05,
                                                                                      "*", ifelse(x$Pval < 0.1, ".", " ")))))
  tab <- matrix(cbind(tab, sign), ncol = 3)
  rownames(tab) <- paste("Item", 1:length(x$Pval))
  colnames(tab) <- switch(x$test,
                          "F" = c("F-value", "P-value", ""),
                          "LR" = c("Chisq-value", "P-value", ""))
  print(tab, quote = F, digits = 4, zero.print = F)
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  n <- nrow(x$data)
  df <- switch(x$type,
               both = c(2, n - 5),
               udif = c(1, n - 4),
               nudif = c(1, n - 5))
  df <- ifelse(x$test == "F", df, df[1])
  critical <- ifelse(x$test == "F", qf(1 - x$alpha, df[1], df[2]), qchisq(1 - x$alpha, df))
  cat(paste("\nDetection thresholds: ", round(critical, 4), " (significance level: ", x$alpha, ")", sep = ""))
  if (is.character(x$DIF)) {
    switch(x$type, both = cat("\nNone of items is detected as DIF"),
           udif = cat("\nNone of items is detected as uniform DIF"),
           nudif = cat("\nNone of items is detected as non-uniform DIF"))
  }
  else {
    switch(x$type, both = cat("\nItems detected as DIF items:\n"),
           udif = cat("\nItems detected as uniform DIF items:\n"),
           nudif = cat("\nItems detected as non-uniform DIF items:\n"))
    cat("\n", paste("Item ", x$DIF, "\n", sep = ""))
  }
}

#' @rdname difNLR
#' @export
plot.difNLR <- function(x, plot.type = "cc", item = "all",
                        col = c("dodgerblue2", "goldenrod2"),
                        shape = 21, size = .8,
                        linetype = c(2, 1), title,
                        ...){

  plotstat <- function(x, size = size, title = title){
    if (x$conv.fail != 0){
      if (length(x$conv.fail) == length(x$Sval)){
        switch(x$test, "F" = stop("None of items does converge. F-statistic values not plotted", call. = FALSE),
                       "LR" = stop("None of items does converge. LR-statistic values not plotted", call. = FALSE))
      } else {
        switch(x$test, "F" = warning(paste("Item ", x$conv.fail.which, " does not converge. F-statistic value not plotted",
                                           sep = "", collapse = "\n"), call. = FALSE),
                       "LR" = warning(paste("Item ", x$conv.fail.which, " does not converge. LR-statistic value not plotted",
                                           sep = "", collapse = "\n"), call. = FALSE))
      }
    }

    if(missing(title)){
      title <- "Non-Linear Regression DIF Detection \n with None Multiple Comparison Correction"
    }
    n <- nrow(x$data)
    Sval_critical <- switch(x$test, "F" = qf(1 - x$alpha, x$df[1], x$df[2]), "LR" = qchisq(1 - x$alpha, x$df))
    g <- as.factor(ifelse(x$Sval > Sval_critical, 1, 0))
    items <- setdiff(1:length(x$Sval), x$conv.fail.which)
    hv <- na.omit(as.data.frame(cbind(1:length(x$Sval), x$Sval, g)))
    plot_stat <- ggplot(hv, aes_string(x = "V1", y = "V2",
                                       label = as.character("V1"),
                                       col = as.factor(g))) +
                  ### points
                  geom_text() +
                  scale_color_manual(values = c("black", "red")) +
                  ### critical value
                  geom_hline(yintercept = Sval_critical, size = size) +
                  ### theme
                  ggtitle(title) +
                  labs(x = "Item", y = switch(x$test, "F" = "F-statistic", "LR" = "Chisq-statistic")) +
                  theme_bw() +
                  theme(text = element_text(size = 11),
                        plot.title = element_text(size = 11, face = "bold", vjust = 1.5),
                        axis.line  = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.background = element_rect(fill = "transparent", colour = NA),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        legend.position = "none")

    print(plot_stat)
  }

  plotCC <- function(x, item = item,
                     col = col, shape = shape, size = size,
                     linetype = linetype, title = title){
    m <- nrow(x$coef)
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
    if (length(col) == 1){
      col <- rep(col, 2)
    } else {
      if (length(col) > 2){
        col <- col[1:2]
        warning("Length of 'col' is greater than 2. Only first two values are used",
                call. = FALSE)
      }
    }
    if (class(item) == "character"){
      items <- 1:m
    } else {
      items <- item
    }
    if (any(x$conv.fail.which %in% items)){
      if (length(setdiff(items, x$conv.fail.which)) == 0){
        stop(paste("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted",
                   sep = "", collapse = "\n"),
             call. = FALSE)
      } else {
        warning(paste("Item ", intersect(x$conv.fail.which, items), " does not converge. Characteristic curve not plotted",
                      sep = "", collapse = "\n"),
                call. = FALSE)
        items <- setdiff(items, x$conv.fail.which)
      }
    }
    if (length(linetype) != 2){
      if (length(linetype) == 1){
        linetype <- rep(linetype, 2)
      } else {
        linetype <- linetype[1:2]
        warning("Length of 'linetype' is greater than 2. Only first two values are used",
                call. = FALSE)
      }
    }
    if (!missing(title)){
      TITLE <- title
    }

    ### functions
    regFce_nonUDIF <- function(STS, group, a, b, c, aDif, bDif){
      return(c + (1 - c)/(1 + exp(-(a + aDif*group)*(STS - (b + bDif*group)))))
    }

    ### data
    stand_total_score_R <- c(scale(apply(x$data[x$group == 0, ], 1, sum)))
    stand_total_score_F <- c(scale(apply(x$data[x$group == 1, ], 1, sum)))
    max_sts <- max(as.numeric(levels(as.factor(stand_total_score_R))),
                   as.numeric(levels(as.factor(stand_total_score_F))))
    min_sts <- min(as.numeric(levels(as.factor(stand_total_score_R))),
                   as.numeric(levels(as.factor(stand_total_score_F))))
    alpha <- 0.5
    plot_CC <- list()

    for (i in items){
      hv_R <- data.frame(cbind(as.numeric(levels(as.factor(stand_total_score_R))),
                               tapply(x$data[x$group == 0, i],
                                      as.factor(stand_total_score_R), mean)))
      hv_F <- data.frame(cbind(as.numeric(levels(as.factor(stand_total_score_F))),
                               tapply(x$data[x$group == 1, i],
                                      as.factor(stand_total_score_F), mean)))
      hv   <- data.frame(rbind(cbind(hv_R, Group = "Reference"),
                               cbind(hv_F, Group = "Focal")))
      rownames(hv) <- 1:dim(hv)[1]
      hv$size <- c(table(stand_total_score_R),
                   table(stand_total_score_F))
      if (missing(title)){
        TITLE <- paste("Item", i)
      }
      plot_CC[[i]] <- ggplot(hv, aes_string("X1", "X2")) +
                      ### points
                      geom_point(aes_string(colour = "Group", fill = "Group",
                                            size = "size"),
                                 alpha = alpha, shape = shape) +
                      ### lines
                      stat_function(aes(colour = "Reference", linetype = "Reference"),
                                    fun = regFce_nonUDIF,
                                    args = list(group = 0,
                                                a = x$coef[i, 1], b = x$coef[i, 2], c = x$coef[i, 3],
                                                aDif = x$coef[i, 4], bDif = x$coef[i, 5]),
                                    size = size,
                                    geom = "line") +
                      stat_function(aes(colour = "Focal", linetype = "Focal"),
                                    fun = regFce_nonUDIF,
                                    args = list(group = 1,
                                                a = x$coef[i, 1], b = x$coef[i, 2], c = x$coef[i, 3],
                                                aDif = x$coef[i, 4], bDif = x$coef[i, 5]),
                                    size = size,
                                    geom = "line") +
                      ### style
                      scale_size_continuous(name = "Counts")  +
                      scale_colour_manual(breaks = hv$Group, values = col, name = "Group") +
                      scale_fill_manual(breaks = hv$Group, values = col, name = "Group") +
                      scale_linetype_manual(breaks = hv$Group, values = linetype, name = "Group") +
                      ### theme
                      ggtitle(TITLE) +
                      labs(x = "Standardized total score", y = "Probability of correct answer") +
                      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
                      theme_bw() +
                      theme(text = element_text(size = 11),
                            plot.title = element_text(size = 11, face = "bold", vjust = 1.5),
                            axis.line  = element_line(colour = "black"),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            plot.background = element_rect(fill = "transparent", colour = NA)) +
                      ### legend
                      theme(legend.box.just = "left",
                            legend.justification = c(1, 0),
                            legend.position = c(0.97, 0.03),
                            legend.margin = unit(0, "lines"),
                            legend.box = "vertical",
                            legend.key.size = unit(0.9, "cm"),
                            legend.key.height = unit(0.8, "line"),
                            legend.text.align = 0,
                            legend.title.align = 0,
                            legend.key = element_rect(colour = "white"))
    }

    plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
    # names(plot_CC) <- paste("Item", items)
    class(plot_CC)

    return(plot_CC)
  }
  ### checking input
  if (!(plot.type %in% c("cc", "stat"))){
    stop("Possible values of 'plot.type' is 'cc' or 'stat' ",
         call. = FALSE)
  } else {
    if (plot.type == "cc"){
      plotCC(x, item = item,
             col = col, shape = shape, size = size,
             linetype = linetype, title = title)
    } else {
      plotstat(x, size = size, title = title)
    }
  }
}

#' @rdname difNLR
#' @export
fitted.difNLR <- function(object, item = "all", ...){

  ### checking input
  m <- nrow(object$coef)
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
  if (any(object$conv.fail.which %in% items)){
    if (length(setdiff(items, object$conv.fail.which)) == 0){
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                 sep = "", collapse = "\n"),
           call. = FALSE)
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                    sep = "", collapse = "\n"),
              call. = FALSE)
      items <- setdiff(items, object$conv.fail.which)
    }
  }


  ### functions
  NLR <- function(STS, group, a, b, c, aDif, bDif){
    return(c + (1 - c)/(1 + exp(-(a + aDif*group)*(STS - (b + bDif*group)))))
  }

  ### data
  STS <- c(scale(apply(object$data, 1, sum)))

  ### fitted values
  FV <- lapply(items, function(i) NLR(STS, object$group, object$coef[i, 1], object$coef[i, 2],
                                      object$coef[i, 3], object$coef[i, 4], object$coef[i, 5]))
  FV <- lapply(FV, setNames, NULL)
  names(FV) <- paste("Item", items)
  return(FV)

}

#' @rdname difNLR
#' @export
predict.difNLR <- function(object, item = "all",
                           score, group, ...){

  ### checking input
  m <- nrow(object$coef)
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
  if (missing(score)){
    score <- c(scale(apply(object$data, 1, sum)))
  }
  if (missing(group)){
    group <- object$group
  }
  if(length(score) != length(group)){
    stop("'score' and 'group' must be the same length",
         call. = FALSE)
  }
  if (class(item) == "character"){
    items <- 1:m
  } else {
    items <- item
  }
  if (any(object$conv.fail.which %in% items)){
    if (length(setdiff(items, object$conv.fail.which)) == 0){
      stop(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                 sep = "", collapse = "\n"),
           call. = FALSE)
    } else {
      warning(paste("Item ", intersect(object$conv.fail.which, items), " does not converge. No values displayed",
                    sep = "", collapse = "\n"),
              call. = FALSE)
      items <- setdiff(items, object$conv.fail.which)
    }
  }
  ### functions
  NLR <- function(STS, group, a, b, c, aDif, bDif){
    return(c + (1 - c)/(1 + exp(-(a + aDif*group)*(STS - (b + bDif*group)))))
  }

  ### data
  STS <- score


  ### predicted values
  PV <- lapply(items, function(i) NLR(STS, group, object$coef[i, 1], object$coef[i, 2],
                                      object$coef[i, 3], object$coef[i, 4], object$coef[i, 5]))
  PV <- lapply(PV, setNames, NULL)
  names(PV) <- paste("Item", items)
  return(predict = PV)
}


