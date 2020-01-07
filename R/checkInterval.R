#' Checks interval bounds.
#'
#' @param x numeric.
#' @param vec numeric: increasingly sorted bounds of interval.
#'
#' @description Checks whether \code{x} lies in interval defined by bounds in \code{vec}. If it does,
#' it returns value of \code{x}. In case that value of \code{x} is lower than lower bound specified
#' in \code{vec}, it returns its value. In case that value of \code{x} is greater than upper bound specified
#' in \code{vec}, it returns its value.
#'
#' @usage checkInterval(x, vec)
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
#' @examples
#' \dontrun{
#' checkInterval(x = 0.5, vec = c(0, 1))
#' checkInterval(x = 1.5, vec = c(0, 1))
#' checkInterval(x = -0.5, vec = c(0, 1))
#' }
#'
#' @export
checkInterval <- function(x, vec) {
  ifelse(x >= vec[1] & x <= vec[2], x,
    ifelse(x > vec[2], vec[2], vec[1])
  )
}
