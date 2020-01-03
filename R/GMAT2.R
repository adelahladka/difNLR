#' Dichotomous Data Set Based on Graduate Management Admission Test
#'
#' @description The \code{GMAT2} data set is generated data set based on parameters from Graduate Management Admission Test (GMAT) data set (Kingston et al., 1985). First two items were considered to function differently in uniform and non-uniform way respectively. The data set represents responses of 1,000 subjects to multiple-choice test of 20 items. A correct answer is coded as 1 and incorrect answer as 0. The column \code{group} represents group membership, where 0 represents reference group and 1 represents focal group. Groups are the same size (i.e. 500 per group).
#'
#' @usage data(GMAT2)
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
#' @references
#' Kingston, N., Leary, L., & Wightman, L. (1985). An Exploratory Study of the Applicability of Item Response
#' Theory Methods to the Graduate Management Admission Test. ETS Research Report Series, 1985(2) : 1-64.
#'
#' Drabinova, A. & Martinkova P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{GMAT2test}}, \code{\link{GMAT2key}}
#'
#' @format A \code{GMAT2} data frame consists of 1,000 observations on the following 21 variables. The first 20 columns represent
#' dichotomously scored items of the test. The 21st column is vector of group membership; values 0 and 1 refer to reference and
#' focal group.
"GMAT2"
# > [1] "GMAT2"
