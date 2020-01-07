#' Dichotomous dataset of Medical School Admission Test in Biology.
#'
#' @description The \code{MSATB} dataset consists of the responses of 1,407 subjects
#' (484 males, 923 females) to admission test to medical school in the Czech republic.
#' It contains 20 selected items from original test while first item was previously detected
#' as differently functioning (Vlckova, 2014). A correct answer is coded as 1 and incorrect
#' answer as 0. The column \code{gender} represents gender of students, where 0 indicates
#' males (reference group) and 1 indicates females (focal group).
#'
#' @usage data(MSATB)
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
#' Drabinova, A. & Martinkova P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Vlckova, K. (2014). Test and Item Fairness (Unpublished master's thesis).
#'
#' @keywords datasets
#'
#' @seealso \code{\link{MSATBtest}}, \code{\link{MSATBkey}}
#'
#' @format A \code{MSATB} data frame consists of 1,407 observations on the following 21 variables:
#' \describe{
#' \item{Item}{dichotomously scored items of the test}
#' \item{gender}{gender of respondents, \code{"0"} males, \code{"1"} females}
#' }
"MSATB"
