#' Data Set of School Admission Test in Biology
#'
#' @description The \code{MSATBtest} data set consists of the responses of 1,407 subjects (484 males, 923 females)
#' to multiple-choice admission test to medical school in Czech republic. It contains 20 selected items from original
#' test while first item was previously detected detected as differently functioning (Vlckova, 2014).
#' Possible answers were A, B, C, D, while any combination of these can be correct. The column \code{gender} represents
#' gender of students, where 0 represents males (reference group) and 1 represents females (focal group).
#'
#' @usage data(MSATBtest)
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
#' @references
#' Drabinova, A. & Martinkova P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' Vlckova, K. (2014). Test and Item Fairness (Unpublished master's thesis).
#'
#' @keywords datasets
#'
#' @seealso \code{\link{MSATB}}, \code{\link{MSATBkey}}
#'
#' @format A \code{MSATBtest} data frame consists of 1,407 observations on the following 21 variables. The first 20
#' columns represent answers of subject to an items of the test. The 21st column is vector subjects' gender;
#' values 0 and 1 refer to males (reference group) and females (focal group).
#' @docType data
"MSATBtest"

