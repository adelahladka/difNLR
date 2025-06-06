#' Dataset based on GMAT.
#'
#' @description The \code{GMAT2test} is a generated dataset based on parameters from Graduate Management
#' Admission Test (GMAT, Kingston et al., 1985). First two items were considered to function differently
#' in uniform and non-uniform way respectively. The data set represents responses of 1,000 subjects to
#' multiple-choice test of 20 items. Additionally, 4 possible answers on all items were generated,
#' coded A, B, C, and D. The column \code{group} represents group membership, where 0 indicates reference group
#' and 1 indicates focal group. Groups are the same size (i.e. 500 per group).
#'
#' @usage data(GMAT2test)
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
#' Kingston, N., Leary, L., & Wightman, L. (1985). An exploratory study of the applicability of item response theory
#' methods to the Graduate Management Admission Test. ETS Research Report Series, 1985(2): 1--64.
#'
#' Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M. (2017).
#' Checking equity: Why differential item functioning analysis should be a routine part of developing conceptual
#' assessments. CBE--Life Sciences Education, 16(2), rm2, \doi{10.1187/cbe.16-10-0307}.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{GMAT2}}, \code{\link{GMAT2key}}
#'
#' @format A \code{GMAT2test} data frame consists of 1,000 observations on the following 21 variables:
#' \describe{
#' \item{Item1-Item20}{nominal items of the test coded A, B, C, and D}
#' \item{group}{group membership vector, \code{"0"} reference group, \code{"1"} focal group}
#' }
#' Correct answers are presented in \code{\link{GMAT2key}} data set.
"GMAT2test"
