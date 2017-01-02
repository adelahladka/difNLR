#' Performs DIF detection using Non-Linear Regression method.
#'
#' @aliases difNLR print.difNLR plot.difNLR fitted.difNLR predict.difNLR coef.difNLR
#'
#' @description Performs DIF detection procedure based on Non-Linear Regression and either likelihood-ratio
#' or F test of submodel.
#'
#' @param Data numeric: either the scored data matrix only, or the scored data
#' matrix plus the vector of group. See \strong{Details}.
#' @param group numeric or character: either the binary vector of group membership or
#' the column indicator of group membership. See \strong{Details}.
#' @param focal.name numeric or character: indicated the level of \code{group} which corresponds to
#' focal group
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param type character: type of DIF to be tested (either "both" (default), "udif", or "nudif").
#' See \strong{Details}.
#' @param test character: test to be performed for DIF detection (either "F" (default), or "LR").
#' See \strong{Details}.
#' @param alpha numeric: significance level (default is 0.05).
#' @param p.adjust.method character: method for multiple comparison correction. See \strong{Details}.
#' @param start numeric: matrix with n rows (where n is the number of items) and at most 5 columns
#' containing initial item parameters estimates. See \strong{Details}.
#' @param x an object of 'difNLR' class
#' @param object an object of 'difNLR' class
#' @param plot.type character: type of plot to be plotted (either "cc" for characteristic curve
#' (default), or "stat" for test statistics). See \strong{Details}.
#' @param item either character ("all"), or numeric vector, or single number corresponding to column indicators. See \strong{Details}.
#' @param col character: single value, or vector of two values representing colors for plot.
#' @param shape integer: shape parameter for plot.
#' @param size numeric: single number, or vector of two numbers representing line width in plot.
#' @param linetype numeric: single number, or vector of two numbers representing line type in plot for reference and focal group.
#' @param title string: title of plot.
#' @param score numeric: standardized total score of subject.
#' @param ... other generic parameters for \code{print}, \code{plot}, \code{fitted},
#' \code{predict} or \code{coef} functions.
#'
#' @usage difNLR(Data, group, focal.name, model, type = "both",
#' test = "LR", alpha = 0.05, p.adjust.method = "none", start)
#'
#' @details
#' DIF detection procedure based on Non-Linear Regression is the extension of Logistic Regression
#' procedure (Swaminathan and Rogers, 1990).
#'
#' The \code{Data} is a matrix whose rows represents scored examinee answers ("1" correct,
#' "0" incorrect) and columns correspond to the items. In addition, \code{Data} can hold
#' the vector of group membership. If so, \code{group} is a column indicator of \code{Data}.
#' Otherwise, \code{group} must be either a vector of the same length as \code{nrow(Data)}.
#'
#' The options of \code{model} are as follows: \code{Rasch} for one-parameter logistic model with
#' discrimination parameter fixed on value 1 for both groups, \code{1PL} for one-parameter logistic
#' model with discrimination parameter fixed for both groups, \code{2PL} for logistic regression model,
#' \code{3PLcg} for three-parameter logistic regression model with fixed guessing for both groups,
#' \code{3PLdg} for three-parameter logistic regression model with fixed inattention for both groups, or
#' \code{4PLcgdg} for four-parameter logistic regression model with fixed guessing and inattention
#' parameter for both groups.
#'
#' The \code{type} corresponds to type of DIF to be tested. Possible values are \code{"both"} to
#' detect any DIF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DIF or
#' \code{"nudif"} to detect only non-uniform DIF.
#'
#' The \code{p.adjust.method} is a character for \code{p.adjust} function from the \code{stats}
#' package. Possible values are \code{"holm"}, \code{"hochberg"}, \code{"hommel"},
#' \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}.
#'
#' The \code{start} is a matrix with a number of rows equal to number of items and with 8 columns.
#' First 4 columns represent parameters (a, b, c, d) of generalized logistic regression model
#' for reference group. Last 4 columns represent differences of parameters (aDif, bDif, cDif, dDif)
#' of generalized logistic regression model between reference and focal group.
#'
#' The output of the difNLR is displayed by the \code{print.difNLR} function.
#'
#' Two types of plots are available. The first one is obtained by setting \code{plot.type = "cc"}
#' (default). The characteristic curve for item specified in \code{item} option is plotted. For default
#' option \code{"all"} of item, characteristic curves of all converged items are plotted. The drawn
#' curves represent best model.
#' The second plot is obtained by setting \code{plot.type = "stat"}. The  test statistics
#' (either F-test, or LR-test, depends on argument \code{test}) are displayed on the Y axis,
#' for each coverged item. The detection threshold is displayed by a horizontal line and items
#' detected as DIF are printed with the red color. Only parameters \code{size} and \code{title}
#' are used.
#'
#' Fitted values are extracted by the \code{fitted.difNLR} function for item(s) specified in
#' \code{item} argument.
#'
#' Predicted values are produced by the \code{predict.difNLR} function for item(s) specified in
#' \code{item} argument. \code{score} represents standardized total score of new subject and
#' \code{group} argument represents group membership of new subject.
#'
#' Missing values are allowed but discarded for item estimation. They must be coded as
#' \code{NA} for both, \code{data} and \code{group} parameters.
#'
#' @return A list of class 'difNLR' with the following arguments:
#' \describe{
#'   \item{\code{Sval}}{the values of likelihood ratio test statistics.}
#'   \item{\code{nlrPAR}}{the estimates of final model.}
#'   \item{\code{parM0}}{the estimates of null model.}
#'   \item{\code{parM1}}{the estimates of alternative model.}
#'   \item{\code{alpha}}{numeric: significance level.}
#'   \item{\code{DIFitems}}{either the column indicators of the items which were detected as DIF, or \code{"No DIF item detected"}.}
#'   \item{\code{type}}{character: type of DIF that was tested.}
#'   \item{\code{p.adjust.method}}{character: method for multiple comparison correction which was applied.}
#'   \item{\code{pval}}{the p-values by likelihood ratio test.}
#'   \item{\code{adj.pval}}{the adjusted p-values by likelihood ratio test using \code{p.adjust.method}.}
#'   \item{\code{df}}{the degress of freedom of likelihood ratio test.}
#'   \item{\code{group}}{the vector of group membership.}
#'   \item{\code{Data}}{the data matrix.}
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
#' Data  <- GMAT[, colnames(GMAT) != "group"]
#' group <- GMAT[, "group"]
#'
#' # Testing both DIF effects using likelihood-ratio test and
#' # 3PL model with fixed guessing for groups
#' x <- difNLR(Data, group, focal.name = 1, model = "3PLcg")
#'
#' # Testing both DIF effects using F test and
#' # 3PL model with fixed guessing for groups
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "F")
#'
#' # Testing both DIF effects using LR test,
#' # 3PL model with fixed guessing for groups
#' # and Benjamini-Hochberg correction
#' difNLR(Data, group, focal.name = 1, model = "3PLcg", p.adjust.method = "BH")
#'
#' # Testing both DIF effects using Rasch model
#' difNLR(Data, group, focal.name = 1, model = "Rasch")
#'
#' # Testing both DIF effects using 2PL model
#' difNLR(Data, group, focal.name = 1, model = "2PL")
#'
#' # Testing uniform DIF effects
#' difNLR(Data, group, focal.name = 1, model = "2PL", type = "udif")
#' # Testing non-uniform DIF effects
#' difNLR(Data, group, focal.name = 1, model = "2PL", type = "nudif")
#'
#' # Graphical devices
#' plot(x)
#' plot(x, item = x$DIFitems)
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


difNLR <- function(Data, group, focal.name, model,
                   type = "both",
                   test = "LR", alpha = 0.05,
                   p.adjust.method = "none", start
                   )
{
  if (type == "nudif" & model == "1PL")
    stop("Detection of non-uniform DIF is not possible with 1PL model!",
         call. = FALSE)
  if (type == "nudif" & model == "Rasch")
    stop("Detection of non-uniform DIF is not possible with Rasch model!",
         call. = FALSE)
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
    start <- NULL
  }
  if (missing(model)) {
    stop("'model' is missing",
         call. = FALSE)
  } else {
    if (!(model %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLdg", "4PLcgdg"))){
      stop("Invalid value for 'model'",
           call. = FALSE)
    }
  }
  internalNLR <- function() {
    if (length(group) == 1) {
      if (is.numeric(group)) {
        GROUP <- Data[, group]
        DATA <- Data[, (1:ncol(Data)) != group]
        colnames(DATA) <- colnames(Data)[(1:ncol(Data)) !=  group]
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
      if (!all(apply(DATA, 2, function(i) {
        length(levels(as.factor(i))) == 2
      })))
        stop("'Data' must be data frame or matrix of binary vectors",
             call. = FALSE)
      if (nrow(DATA) != length(GROUP))
        stop("'Data' must have the same number of rows as is length of vector 'group'",
             call. = FALSE)
    } else {
      stop("'Data' must be data frame or matrix of binary vectors",
           call. = FALSE)
    }

    GROUP <- as.numeric(as.factor(GROUP) == focal.name)
    if (is.null(start)) {
      start <- startNLR(DATA, GROUP, model)
      # start <- switch(type,
      #                 both = start,
      #                 nudif = start,
      #                 udif = start[, -4])

    } else {
      if (ncol(start) != 5 & type != "udif")
        stop("'start' must be data frame or matrix with 5 columns",
             call. = FALSE)
      if (ncol(start) != 4 & type == "udif")
        stop("'start' must be data frame or matrix with 4 columns for detecting uniform DIF",
             call. = FALSE)
      if (nrow(start) != ncol(Data))
        stop("'start' must be data frame or matrix with starting values for each item",
             call. = FALSE)
    }

    PROV <- NLR(DATA, GROUP, model = model, type = type, start = start,
                p.adjust.method = p.adjust.method, test = test,
                alpha = alpha)
    STATS <- PROV$Sval
    ADJ.PVAL <- PROV$adjusted.pval
    significant <- which(ADJ.PVAL < alpha)
    if (length(significant) > 0) {
      DIFitems <- significant
      nlrPAR <- nlrSE <- structure(data.frame(matrix(0, ncol = ncol(PROV$par.m0), nrow = nrow(PROV$par.m0))),
                                   .Names = colnames(PROV$par.m0))

      nlrPAR[, colnames(PROV$par.m1)] <- PROV$par.m1
      nlrSE[, colnames(PROV$par.m1)] <- PROV$se.m1

      for (idif in 1:length(DIFitems)) {
        nlrPAR[DIFitems[idif], ] <- PROV$par.m0[DIFitems[idif], ]
        nlrSE[DIFitems[idif], ] <- PROV$se.m0[DIFitems[idif], ]
      }

    } else {
      DIFitems <- "No DIF item detected"
      nlrPAR <- PROV$par.m1
      nlrSE <- PROV$se.m1
    }


    RES <- list(Sval = STATS,
                nlrPAR = nlrPAR, nlrSE = nlrSE,
                parM0 = PROV$par.m0, seM0 = PROV$se.m0, covM0 = PROV$cov.m0,
                parM1 = PROV$par.m1, seM1 = PROV$se.m1, covM1 = PROV$cov.m1,
                alpha = alpha, DIFitems = DIFitems,
                model = model,
                type = type, p.adjust.method = p.adjust.method,
                pval = PROV$pval, adj.pval = PROV$adjusted.pval, df = PROV$df,
                adjusted.p = NULL, test = test,
                group = GROUP, Data = DATA,
                conv.fail = PROV$conv.fail, conv.fail.which = PROV$conv.fail.which)
    # , thr = Q
    class(RES) <- "difNLR"
    return(RES)
  }


  resToReturn <- internalNLR()
  return(resToReturn)
}


#' @rdname difNLR
#' @export
print.difNLR <- function (x, ...){
  title <- switch(x$type,
                  both = "Detection of both types of Differential Item Functioning using Generalized Logistic Regression Model",
                  udif = "Detection of uniform Differential Item Functioning using Generalized Logistic Regression Model",
                  nudif = "Detection of non-uniform Differential Item Functioning using Generalized Logistic Regression Model")
  cat(paste(strwrap(title, width = 60), collapse = "\n"))
  cat(paste(switch(x$test,
                   "F" = "\n\nGeneralized Logistic Regression F-test statistics based on",
                   "LR" = "\n\nGeneralized Logistic Regression Likelihood Ratio chi-square statistics based on\n"),
            switch(x$model,
                   "Rasch" = "Rasch model", "1PL" = "1PL model", "2PL" = "2PL model",
                   "3PL" = "3PL model", "3PLcg" = "3PL model with fixed guessing for groups",
                   "3PLdg" = "3PL model with fixed inattention parameter for groups",
                   "3PLc" = "3PL model", "3PLd" = "3PL model with inattention parameter",
                   "4PLcgdf" = "4PL model with fixed guessing and inattention parameter for groups",
                   "4PLcg" = "4PL model with fixed guessing for groups",
                   "4PLdg" = "4PL model with fixed inattention parameter for groups",
                   "4PL" = "4PL model")), "\n")
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
                 ifelse(x$adj.pval < 0.001, "***",
                        ifelse(x$adj.pval < 0.01, "**",
                               ifelse(x$adj.pval < 0.05, "*",
                                      ifelse(x$adj.pval < 0.1, ".", " ")))))
  if (x$p.adjust.method == "none"){
    tab <- format(round(cbind(x$Sval, x$pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 3)
    colnames(tab) <- switch(x$test,
                            "F" = c("F-value", "P-value", ""),
                            "LR" = c("Chisq-value", "P-value", ""))
  } else {
    tab <- format(round(cbind(x$Sval, x$pval, x$adj.pval), 4))
    tab <- matrix(cbind(tab, sign), ncol = 4)
    colnames(tab) <- switch(x$test,
                            "F" = c("F-value", "P-value", "Adj. P-value", ""),
                            "LR" = c("Chisq-value", "P-value", "Adj. P-value", ""))
  }

  rownames(tab) <- paste("Item", 1:length(x$adj.pval))

  print(tab, quote = F, digits = 4, zero.print = F)
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  critical <- ifelse(x$test == "F", qf(1 - x$alpha, x$df[1], x$df[2]), qchisq(1 - x$alpha, x$df))
  cat(paste("\nDetection thresholds: ", round(critical, 4), " (significance level: ", x$alpha, ")", sep = ""))
  if (is.character(x$DIFitems)) {
    switch(x$type, both = cat("\nNone of items is detected as DIF"),
           udif = cat("\nNone of items is detected as uniform DIF"),
           nudif = cat("\nNone of items is detected as non-uniform DIF"))
  }
  else {
    switch(x$type, both = cat("\n\nItems detected as DIF items:"),
           udif = cat("\n\nItems detected as uniform DIF items:"),
           nudif = cat("\n\nItems detected as non-uniform DIF items:"))
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
        switch(x$test, "F" = stop("None of items does converge.
                                  F-statistic values not plotted", call. = FALSE),
                       "LR" = stop("None of items does converge.
                                   LR-statistic values not plotted", call. = FALSE))
      } else {
        switch(x$test, "F" = warning(paste("Item ", x$conv.fail.which,
                                           " does not converge. F-statistic value not plotted",
                                           sep = "", collapse = "\n"), call. = FALSE),
                       "LR" = warning(paste("Item ", x$conv.fail.which,
                                            " does not converge. LR-statistic value not plotted",
                                           sep = "", collapse = "\n"), call. = FALSE))
      }
    }

    if(missing(title)){
      title <- "Non-Linear Regression DIF Detection \n with None Multiple Comparison Correction"
    }
    n <- nrow(x$Data)
    Sval_critical <- switch(x$test,
                            "F" = qf(1 - x$alpha, x$df[1], x$df[2]),
                            "LR" = qchisq(1 - x$alpha, x$df))
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
                  labs(x = "Item", y = switch(x$test,
                                              "F" = "F-statistic",
                                              "LR" = "Chisq-statistic")) +
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
    m <- nrow(x$nlrPAR)
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
    gNLR <- deriv3( ~ (c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) /
                      (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))),
                    namevec = c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif"),
                    function.arg = function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){})

    ### Data
    xR <- c(scale(apply(x$Data[x$group == 0, ], 1, sum)))
    xF <- c(scale(apply(x$Data[x$group == 1, ], 1, sum)))
    max_sts <- max(as.numeric(levels(as.factor(xR))),
                   as.numeric(levels(as.factor(xF))))
    min_sts <- min(as.numeric(levels(as.factor(xR))),
                   as.numeric(levels(as.factor(xF))))
    alpha <- 0.5
    plot_CC <- list()

    for (i in items){
      hv_R <- data.frame(cbind(as.numeric(levels(as.factor(xR))),
                               tapply(x$Data[x$group == 0, i],
                                      as.factor(xR), mean)))
      hv_F <- data.frame(cbind(as.numeric(levels(as.factor(xF))),
                               tapply(x$Data[x$group == 1, i],
                                      as.factor(xF), mean)))
      hv   <- data.frame(rbind(cbind(hv_R, Group = "Reference"),
                               cbind(hv_F, Group = "Focal")))
      rownames(hv) <- 1:dim(hv)[1]
      hv$size <- c(table(xR), table(xF))

      if (missing(title)){
        TITLE <- paste("Item", i)
      }

      if (dim(x$nlrPAR)[2] != 8){
        PAR <- data.frame(a = rep(1, m), b = 0, c = 0, d = 1, aDif = 0, bDif = 0, cDif = 0, dDif = 0)
        PAR[, colnames(x$nlrPAR)] <- x$nlrPAR
      } else {
        PAR <- x$nlrPAR
      }
      plot_CC[[i]] <- ggplot(hv, aes_string("X1", "X2")) +
                      ### points
                      geom_point(aes_string(colour = "Group", fill = "Group",
                                            size = "size"),
                                 alpha = alpha, shape = shape) +
                      ### lines
                      stat_function(aes(colour = "Reference", linetype = "Reference"),
                                    fun = gNLR,
                                    args = list(g = 0,
                                                a = PAR[i, "a"], b = PAR[i, "b"],
                                                c = PAR[i, "c"], d = PAR[i, "d"],
                                                aDif = PAR[i, "aDif"], bDif = PAR[i, "bDif"],
                                                cDif = PAR[i, "cDif"], dDif = PAR[i, "dDif"]),
                                    size = size,
                                    geom = "line") +
                      stat_function(aes(colour = "Focal", linetype = "Focal"),
                                    fun = gNLR,
                                    args = list(g = 1,
                                                a = PAR[i, "a"], b = PAR[i, "b"],
                                                c = PAR[i, "c"], d = PAR[i, "d"],
                                                aDif = PAR[i, "aDif"], bDif = PAR[i, "bDif"],
                                                cDif = PAR[i, "cDif"], dDif = PAR[i, "dDif"]),
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
                            # legend.margin = margin(0, "lines"),
                            legend.box = "vertical",
                            # legend.key.size = unit(0.9, "cm"),
                            # legend.key.height = unit(0.8, "line"),
                            legend.text.align = 0,
                            legend.title.align = 0,
                            legend.key = element_rect(colour = "white"))
    }

    plot_CC <- Filter(Negate(function(i) is.null(unlist(i))), plot_CC)
    # names(plot_CC) <- paste("Item", items)
    # class(plot_CC)

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
  m <- nrow(object$nlrPAR)
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
  gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){
    return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
  }

  if (dim(object$nlrPAR)[2] != 8){
    PAR <- data.frame(a = rep(1, m), b = 0, c = 0, d = 1, aDif = 0, bDif = 0, cDif = 0, dDif = 0)
    PAR[, colnames(object$nlrPAR)] <- object$nlrPAR
  } else {
    PAR <- object$nlrPAR
  }
  ### data
  STS <- c(scale(apply(object$Data, 1, sum)))

  ### fitted values
  FV <- lapply(items, function(i) gNLR(STS, object$group, PAR[i, "a"], PAR[i, "b"],
                                       PAR[i, "c"], PAR[i, "d"],
                                       PAR[i, "aDif"], PAR[i, "bDif"],
                                       PAR[i, "cDif"], PAR[i, "dDif"]))
  FV <- lapply(FV, setNames, NULL)
  names(FV) <- paste("Item", items)
  return(FV)

}

#' @rdname difNLR
#' @export
predict.difNLR <- function(object, item = "all",
                           score, group, ...){

  ### checking input
  m <- nrow(object$nlrPAR)
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
    score <- c(scale(apply(object$Data, 1, sum)))
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
  gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){
    return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
  }


  if (dim(object$nlrPAR)[2] != 8){
    PAR <- data.frame(a = rep(1, m), b = 0, c = 0, d = 1, aDif = 0, bDif = 0, cDif = 0, dDif = 0)
    PAR[, colnames(object$nlrPAR)] <- object$nlrPAR
  } else {
    PAR <- object$nlrPAR
  }
  ### data
  STS <- score


  ### predicted values
  PV <- lapply(items, function(i) NLR(STS, group, PAR[i, "a"], PAR[i, "b"],
                                      PAR[i, "c"], PAR[i, "d"],
                                      PAR[i, "aDif"], PAR[i, "bDif"],
                                      PAR[i, "cDif"], PAR[i, "dDif"]))
  PV <- lapply(PV, setNames, NULL)
  names(PV) <- paste("Item", items)
  return(predict = PV)
}

#' @rdname difNLR
#' @export
coef.difNLR <- function(object, ...){
  return(object$nlrPAR)
}
