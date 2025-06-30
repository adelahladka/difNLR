#' DIF and DDF Detection by Non-Linear Regression Models.
#'
#' @description The difNLR package provides methods for detecting differential
#'   item functioning (DIF) using non-linear regression models. Both uniform and
#'   non-uniform DIF effects can be detected when considering a single focal
#'   group. Additionally, the method allows for testing differences in guessing
#'   or inattention parameters between the reference and focal group. DIF
#'   detection is performed using either a likelihood-ratio test, an F-test, or
#'   Wald's test of a submodel. The software offers a variety of algorithms for
#'   estimating item parameters.
#'
#'   Furthermore, the difNLR package includes methods for detecting differential
#'   distractor functioning (DDF) using multinomial log-linear regression model.
#'   It also introduces DIF detection approaches for ordinal data via adjacent
#'   category logit and cumulative logit regression models.
#'
#' @aliases difNLR-package
#'
#' @importFrom calculus derivative hessian
#' @import ggplot2
#' @importFrom msm deltamethod
#' @importFrom nnet multinom
#' @importFrom plyr ldply
#' @importFrom stats AIC as.formula anova BIC binomial coef complete.cases deriv3 deviance fitted glm logLik na.omit nls optim p.adjust p.adjust.methods pf pchisq predict rbinom relevel reshape residuals rnorm sd setNames symnum qf qchisq qnorm qt quantile quasibinomial vcov
#' @importFrom VGAM acat AICvlm BICvlm cumulative lrtest_vglm
#'
#' @section Functions:
#' \itemize{
#'   \item \code{\link{ddfMLR}}
#'   \item \code{\link{difNLR}}
#'   \item \code{\link{difORD}}
#'   \item \code{\link{estimNLR}}
#'   \item \code{\link{formulaNLR}}
#'   \item \code{\link{MLR}}
#'   \item \code{\link{NLR}}
#'   \item \code{\link{ORD}}
#'   \item \code{\link{startNLR}}
#' }
#'
#' @section Datasets:
#' \itemize{
#'   \item \code{\link{GMAT}}
#'   \item \code{\link{GMAT2}}
#'   \item \code{\link{MSATB}}
#' }
#'
#' @details
#' Package: difNLR\cr
#' Type: Package\cr
#' Version: 1.5.1-4\cr
#' Date: 2025-06-30\cr
#' Depends: R (>= 4.0.0)\cr
#' Imports: calculus, ggplot2 (>= 3.4.0), msm, nnet, plyr, stats, VGAM\cr
#' Suggests: ShinyItemAnalysis, testthat (>= 3.0.0), vdiffr\cr
#' License: GPL-3\cr
#' BugReports: \url{https://github.com/adelahladka/difNLR/issues}\cr
#' Encoding: UTF-8\cr
#'
#' @author
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references
#' Agresti, A. (2010). Analysis of ordinal categorical data. Second edition. John Wiley & Sons.
#'
#' Drabinova, A. & Martinkova, P. (2017). Detection of differential item functioning with nonlinear regression:
#' A non-IRT approach accounting for guessing. Journal of Educational Measurement, 54(4), 498--517,
#' \doi{10.1111/jedm.12158}.
#'
#' Hladka, A. (2021). Statistical models for detection of differential item functioning. Dissertation thesis.
#' Faculty of Mathematics and Physics, Charles University.
#'
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression models for DIF and DDF detection.
#' The R Journal, 12(1), 300--323, \doi{10.32614/RJ-2020-014}.
#'
#' Hladka, A., Martinkova, P., & Brabec, M. (2025). New iterative algorithms for estimation of item functioning.
#' Journal of Educational and Behavioral Statistics. Online first, \doi{10.3102/10769986241312354}.
#'
#' Kingston, N., Leary, L., & Wightman, L. (1985). An exploratory study of the applicability of item response theory
#' methods to the Graduate Management Admission Test. ETS Research Report Series, 1985(2): 1--64.
#'
#' Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M. (2017).
#' Checking equity: Why differential item functioning analysis should be a routine part of developing conceptual
#' assessments. CBE--Life Sciences Education, 16(2), rm2, \doi{10.1187/cbe.16-10-0307}.
#'
#' Swaminathan, H. & Rogers, H. J. (1990). Detecting differential item functioning using logistic regression procedures.
#' Journal of Educational Measurement, 27(4), 361--370, \doi{10.1111/j.1745-3984.1990.tb00754.x}
#'
#' Vlckova, K. (2014). Test and item fairness. Master's thesis. Faculty of Mathematics and Physics, Charles University.
#'
#' @note This package was supported by grant funded by Czech Science foundation under number GJ15-15856Y.
#'
#' @docType package
"_PACKAGE"
# > [1] "_PACKAGE"
