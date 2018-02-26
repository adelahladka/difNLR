#' Detection of Differential Item Functioning (DIF) and Differential Distractor Functioning (DDF) by Non-Linear Regression Models
#'
#' @description The difNLR package containts DIF detection method based on non-linear
#' regression. Both uniform and non-uniform DIF effects can be detected when considering
#' one focal group. DIF detection method is based either on likelihood-ratio test, or on F-test
#' of submodel. Package also offers DDF detection method based on multinomial log-linear regression
#' model.
#'
#' @aliases difNLR-package
#'
#' @import ggplot2
#' @importFrom CTT score
#' @importFrom methods is
#' @importFrom nnet multinom
#' @importFrom reshape2 melt
#' @importFrom stats AIC BIC as.formula anova complete.cases deriv3 deviance na.omit nls p.adjust pf qf pchisq qchisq quantile setNames logLik rbinom relevel rnorm
#' @importFrom msm deltamethod
#'
#' @details
#' Package: difNLR\cr
#' Type: Package\cr
#' Version: 1.2.0\cr
#' Date: 2018-02-26\cr
#' Depends: R (>= 3.4.3), CTT, ggplot2 (>= 2.2.1), methods, msm, nnet, reshape2, stats\cr
#' License: GPL-3\cr
#' BugReports: https://github.com/drabinova/difNLR/issues
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' drabinova@cs.cas.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' Karel Zvara \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#'
#' @references
#' Drabinova, A. & Martinkova P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517.
#'
#' Kingston, N., Leary, L., & Wightman, L. (1985). An Exploratory Study of the Applicability of Item Response Theory
#' Methods to the Graduate Management Admission Test. ETS Research Report Series, 1985(2) : 1-64.
#'
#' Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M. (2017).
#' Checking equity: Why Differential Item Functioning Analysis Should Be a Routine Part of Developing Conceptual
#' Assessments. CBE-Life Sciences Education, 16(2), \url{http://www.lifescied.org/content/16/2/rm2}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting Differential Item Functioning Using Logistic Regression Procedures.
#' Journal of Educational Measurement, 27, 361-370.
#'
#' Vlckova, K. (2014). Test and Item Fairness (Unpublished master's thesis).
#'
#' @note This package was supported by grant funded by Czech Science foundation under number GJ15-15856Y.
#' @docType package
"_PACKAGE"
#> [1] "_PACKAGE"
