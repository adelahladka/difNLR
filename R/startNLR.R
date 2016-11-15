#' Calculates starting values for \code{difNLR} function.
#'
#' @param data numeric: a matrix or data.frame of binary data.
#' @param group numeric: binary vector of group membership. "0" for reference group, "1" for focal group.
#' @param parameterization character: parameterization of regression coefficients. Possible options are \code{"IRT"} (Item response theory, default option) and \code{"LR"} (Logistic regression)
#'
#' @description Calculates starting values for \code{difNLR} function based on linear approximation.
#'
#' @usage startNLR(data, group, parameterization = "IRT")
#'
#' @return
#' A data.frame containing rows representing items and 5 columns representing parameters of \code{difNLR} model. First column represents discrimination (a), second difficulty (b), third guessing (c), fourth difference in discrimination between reference and focal group (aDif) and fifth difference in difficulty between reference and focal group (bDif).
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
#' # loading data based on GMAT
#' data(GMAT)
#'
#' data  <- GMAT[, colnames(GMAT) != "group"]
#' group <- GMAT[, "group"]
#'
#' # starting values
#' startNLR(data, group)
#' }
#' @export
startNLR <- function(data, group, parameterization = "IRT"){

  startNLR_line <- function(DATA){
    stand_total_score <- c(scale(apply(DATA, 1, sum)))

    Q3 <- cut(stand_total_score, quantile(stand_total_score, (0:3) / 3),
              c("I", "II", "III"),
              include.lowest = TRUE)

    x <- cbind(mean(stand_total_score[Q3 == "I"]), apply(DATA[Q3 == "I", ], 2, mean))
    y <- cbind(mean(stand_total_score[Q3 == "III"]), apply(DATA[Q3 == "III", ], 2, mean))
    u1 <- y[, 1] - x[, 1]
    u2 <- y[, 2] - x[, 2]

    c <- -(-u1 * y[, 2] + u2 * y[, 1]) / u1

    t <- u2 / u1

    results <- as.data.frame(cbind(t, c))
    return(results)
  }
  line <- startNLR_line(data)
  g <- apply(cbind(0, line$t * (-4) + line$c), 1, max)

  group <- as.factor(group)
  levels(group) <- c("R", "F")

  data_R <- data[group == "R", ] ### reference group
  data_F <- data[group == "F", ] ### foal group
  line_R <- startNLR_line(data_R)
  line_F <- startNLR_line(data_F)

  b_R <- ((1 + g)/2 - line_R$c)/line_R$t
  b_F <- ((1 + g)/2 - line_F$c)/line_F$t

  a_R <- 4 * line_R$t/(1 - g)
  a_F <- 4 * line_F$t/(1 - g)

  results <- switch(parameterization,
                    IRT = data.frame(a_R, b_R, g, a_R - a_F, b_R - b_F),
                    logistic = data.frame(a_R, -a_R * b_R, g, a_R - a_F, -a_R * b_R + a_F * b_F))
  colnames(results) <- c("a", "b", "c", "aDif", "bDif")
  return(results)
}

