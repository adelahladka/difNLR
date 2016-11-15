#' Generates binary data set based on \code{difNLR} model.
#'
#' @param N numeric: number of rows representing correspondents.
#' @param ratio numeric: ratio of number of rows for reference and focal group.
#' @param parameters numeric: matrix representing true parameters of \code{difNLR} model. See \strong{Details}.
#'
#' @description Generates binary data set based on \code{difNLR} model with the same guessing parameter for both groups, reference and focal.
#'
#' @details
#' The \code{parameters} is a numeric matrix with 5 columns. First column represents discrimination parameter (a), second represents difficulty (b), third guessing (c), fourth difference in discrimination between reference and focal group (aDif) and fifth difference in difficulty between reference and focal group (bDif). Rows represent items and their number (n) corresponds to number of columns of resultant data set.
#'
#' @usage genNLR(N = 1000, ratio = 1, parameters)
#'
#' @return
#' A data.frame containing \code{N} rows representing correspondents and n+1 columns representing n items. Last column is group membership variable with coding 0 for reference group and 1 for focal group.
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
#' @examples
#' \dontrun{
#' # seed
#' set.seed(123)
#' # generating parameters without DIF, 10 items
#' parameters <- data.frame(a = runif(10, 0.8, 2),
#'                          b = rnorm(10),
#'                          c = runif(10, 0, 0.4),
#'                          aDif = 0, bDif = 0)
#'
#' # generating data set with 300 observations (150 each group)
#' genNLR(N = 300, parameters = parameters)
#'
#' # generating data set with 300 observations (250 reference group, 50 focal)
#' genNLR(N = 300, ratio = 5, parameters = parameters)
#'
#' }
#' @export


genNLR <- function(N = 1000, ratio = 1, parameters){

  n <- nrow(parameters)
  N_R   <- round(N/(ratio + 1)*ratio)
  N_F   <- round(N/(ratio + 1))
  group <- c(rep(0, N_R), rep(1, N_F))

  theta <- rnorm(N, mean = 0, sd = 1)
  a <- parameters[1:n, 1]
  b <- parameters[1:n, 2]
  c <- parameters[1:n, 3]
  aF <- parameters[1:n, 1] + parameters[1:n, 4]
  bF <- parameters[1:n, 2] + parameters[1:n, 5]

  n <- nrow(parameters)

  p <- matrix(NA, nrow = N, ncol = n)
  for (i in 1:n){
    p[1:N_R, i] <- c[i] + (1 - c[i]) / ( 1 + exp(- a[i] * (theta[1:N_R] - b[i])))
    p[(N_R + 1):N, i] <- c[i] + (1 - c[i]) / ( 1 + exp( - aF[i] * (theta[(N_R + 1):N] - bF[i])))
  }

  answer <- matrix(NA, nrow = N, ncol = n)
  for (j in 1:n) {
    for (i in 1:N) {
      answer[i, j] <- rbinom(1, 1, p[i, j])
    }
  }

  data <- data.frame(answer, group)

  colnames(data) <- c(paste("Item", seq_along(1:n), sep=""), "group")
  return(data)
}

