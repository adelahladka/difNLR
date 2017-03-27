#' Dichotomous Data Set of Admission Test to Medical School
#'
#' @description The \code{difMedical} data set consists of the responses of 1,407 subjects
#' (484 males, 923 females) to admission test to medical school in Czech republic.
#' It contains 20 selected items from original test while first item was previously detected
#' as differently functioning (Vlckova, 2014). A correct answer is coded as 1 and incorrect
#' answer as 0. The column \code{gender} represents gender of students, where 0 represents
#' males (reference group) and 1 represents females (focal group).
#'
#' @usage data(difMedical)
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
#' @references
#' Vlckova, K. (2014). Test and Item Fairness (Unpublished master's thesis).
#'
#' Drabinova, A. and Martinkova P. (2016). Detection of Differenctial Item Functioning Based on
#' Non-Linear Regression, Technical Report, V-1229, \url{http://hdl.handle.net/11104/0259498}.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{difMedicaltest}}, \code{\link{difMedicalkey}}
#'
#' @format A \code{difMedical} data frame consists of 1,407 observations on the following
#' 21 variables. The first 20 columns represent dichotomously scored items of the test.
#' The 21st column is vector of group membership; values 0 and 1 refer to males (reference group)
#' and females (focal group).
"difMedical"

