#' Calculates starting values for \code{difNLR} function.
#'
#' @param Data numeric: a matrix or data.frame of binary data.
#' @param group numeric: binary vector of group membership.
#' "0" for reference group, "1" for focal group.
#' @param model character: generalized logistic regression model
#' to be fitted. See \strong{Details}.
#' @param parameterization character: parameterization of regression
#' coefficients. Possible options are \code{"IRT"} (Item response theory,
#' default option) and \code{"LR"} (Logistic regression)
#'
#' @description Calculates starting values for \code{difNLR} function based
#' on linear approximation.
#'
#' @usage startNLR(Data, group, model, parameterization = "IRT")
#'
#' @details
#' The options of \code{model} are as follows: \code{Rasch} for one-parameter logistic model with
#' discrimination parameter fixed on value 1 for both groups, \code{1PL} for one-parameter logistic
#' model with discrimination parameter fixed for both groups, \code{2PL} for logistic regression model,
#' \code{3PLcg} for three-parameter logistic regression model with fixed guessing for both groups,
#' \code{3PLdg} for three-parameter logistic regression model with fixed inattention for both groups, or
#' \code{4PLcgdg} for four-parameter logistic regression model with fixed guessing and inattention
#' parameter for both groups.
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
#' Data  <- GMAT[, colnames(GMAT) != "group"]
#' group <- GMAT[, "group"]
#'
#' # starting values for 3PL model
#' startNLR(Data, group, model = "3PL")
#' }
#' @export
#'
startNLR <- function(Data, group, model, parameterization = "IRT"){
  if (missing(model)) {
    stop("'model' is missing",
         call. = FALSE)
  } else {
    if (!(model %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLdg", "4PLcgdg"))){
      stop("Invalid value for 'model'",
           call. = FALSE)
    }
  }
  startNLR_line <- function(DATA){
    stand_total_score <- c(scale(apply(DATA, 1, sum)))

    Q3 <- cut(stand_total_score, quantile(stand_total_score, (0:3) / 3),
              c("I", "II", "III"),
              include.lowest = TRUE)

    x <- cbind(mean(stand_total_score[Q3 == "I"]), apply(DATA[Q3 == "I", ], 2, mean))
    y <- cbind(mean(stand_total_score[Q3 == "III"]), apply(DATA[Q3 == "III", ], 2, mean))
    u1 <- y[, 1] - x[, 1]
    u2 <- y[, 2] - x[, 2]

    q <- -(-u1 * y[, 2] + u2 * y[, 1]) / u1

    k <- u2 / u1

    results <- as.data.frame(cbind(k, q))
    return(results)
  }

  line <- startNLR_line(Data)

  data_R <- Data[group == 0, ] ### reference group
  data_F <- Data[group == 1, ] ### foal group
  line_R <- startNLR_line(data_R)
  line_F <- startNLR_line(data_F)

  if (model %in% c("Rasch", "1PL", "2PL", "3PLdg", "3PLd")){
    c_R <- c_F <- 0
  } else {
    if (grepl("cg", model)){
      c_R <- c_F <- apply(cbind(0, line$k * (-4) + line$q), 1, max)
    } else {
      c_R <- apply(cbind(0, line_R$k * (-4) + line_R$q), 1, max)
      c_F <- apply(cbind(0, line_F$k * (-4) + line_F$q), 1, max)
    }
  }


  if (model %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLc")){
    d_R <- d_F <- 1
  } else {
    if (grepl("dg", model)){
      d_R <- d_F <- apply(cbind(1, line$k * 4 + line$q), 1, min)
    } else {
      d_R <- apply(cbind(1, line_R$k * 4 + line_R$q), 1, min)
      d_F <- apply(cbind(1, line_F$k * 4 + line_F$q), 1, min)
    }
  }

  if (model == "Rasch"){
    a_R <- a_F <- 1
  } else {
    if (model == "1PL") {
      a_R <- a_F <- 4 * line$k/(1 - c_R)
    } else {
      a_R <- 4 * line_R$k/(1 - c_R)
      a_F <- 4 * line_F$k/(1 - c_F)
    }
  }

  b_R <- ((1 + c_R)/2 - line_R$q)/line_R$k
  b_F <- ((1 + c_F)/2 - line_F$q)/line_F$k


  results <- switch(parameterization,
                    IRT = data.frame(a_R, b_R, c_R, d_R,
                                     a_F - a_R, b_F - b_R, c_F - c_R, d_F - d_R),
                    logistic = data.frame(a_R, -a_R * b_R, c_R, d_R,
                                          a_F - a_R, -a_R * b_R + a_F * b_F, c_F - c_R, d_F - d_R))
  colnames(results) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
  return(results)
}

