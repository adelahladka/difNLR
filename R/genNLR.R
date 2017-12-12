#' Generates binary data set based on Non-Linear Regression model (generalized logistic regression).
#'
#' @param N numeric: number of rows representing correspondents.
#' @param ratio numeric: ratio of number of rows for reference and focal group.
#' @param parameters numeric: matrix representing with n rows (where n is number of items) and 8 columns
#' representing true parameters of generalized logistic regression model . See \strong{Details}.
#'
#' @description Generates binary data set based on generalized logistic model.
#'
#' @details
#' The \code{parameters} is a numeric matrix with 8 columns. First 4 columns represent parameters (a, b, c, d)
#' of generalized logistic regression model for reference group. Last 4 columns represent differences of
#' parameters (aDif, bDif, cDif, dDif) of generalized logistic regression model between reference and focal
#' group. Rows represent items and their number (n) corresponds to number of columns of resultant data set.
#'
#' @usage genNLR(N = 1000, ratio = 1, parameters)
#'
#' @return
#' A data.frame containing \code{N} rows representing correspondents and n+1 columns representing n items.
#' Last column is group membership variable with coding 0 for reference group and 1 for focal group.
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
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517.
#'
#' @examples
#' \dontrun{
#' # seed
#' set.seed(123)
#' # generating parameters without DIF, 10 items
#' parameters <- data.frame(a = runif(10, 0.8, 2),
#'                          b = rnorm(10),
#'                          c = runif(10, 0, 0.4),
#'                          d = 1,
#'                          aDif = 0, bDif = 0,
#'                          cDif = 0, dDif = 0)
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

  gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif){
    return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
  }

  theta <- rnorm(N, mean = 0, sd = 1)


  pR <- sapply(1:nrow(parameters), function(i)
    do.call(gNLR, c(list(x = theta[1:N_R], g  = 0), parameters[i, ])))
  pF <- sapply(1:nrow(parameters), function(i)
    do.call(gNLR, c(list(x = theta[(N_R + 1):N], g  = 1), parameters[i, ])))

  p <- rbind(pR, pF)

  answer <- matrix(NA, nrow = N, ncol = n)
  for (j in 1:n) {
    for (i in 1:N) {
      answer[i, j] <- rbinom(1, 1, p[i, j])
    }
  }

  data <- data.frame(answer, group)

  colnames(data) <- c(paste("Item", seq_along(1:n), sep = ""), "group")
  return(data)
}

