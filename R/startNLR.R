#' Calculates starting values for \code{difNLR} function.
#'
#' @param Data numeric: a matrix or data.frame of binary data.
#' @param group numeric: binary vector of group membership.
#' "0" for reference group, "1" for focal group.
#' @param model character: generalized logistic regression model
#' to be fitted. See \strong{Details}.
#' @param match vector of matching criterion. Its length need to be the same as number
#' of observations in \code{Data}.
#' @param parameterization character: parameterization of regression
#' coefficients. Possible options are \code{"classic"} (IRT parameterization),
#' \code{"alternative"} (default) and \code{"logistic"} (logistic regression).
#' See \strong{Details}.
#'
#' @description Calculates starting values for \code{difNLR} function based
#' on linear approximation.
#'
#' @usage startNLR(Data, group, model, match = "zscore", parameterization = "alternative")
#'
#' @details
#' The \code{model} argument offers several predefined models. The options are as follows:
#' \code{Rasch} for 1PL model with discrimination parameter fixed on value 1 for both groups,
#' \code{1PL} for 1PL model with discrimination parameter fixed for both groups,
#' \code{2PL} for logistic regression model,
#' \code{3PLcg} for 3PL model with fixed guessing for both groups,
#' \code{3PLdg} for 3PL model with fixed inattention for both groups,
#' \code{3PLc} (alternatively also \code{3PL}) for 3PL regression model with guessing parameter,
#' \code{3PLd} for 3PL model with inattention parameter,
#' \code{4PLcgdg} for 4PL model with fixed guessing and inattention parameter for both groups,
#' \code{4PLcgd} (alternatively also \code{4PLd}) for 4PL model with fixed guessing for both groups,
#' \code{4PLcdg} (alternatively also \code{4PLc}) for 4PL model with fixed inattention for both groups,
#' or \code{4PL} for 4PL model.
#'
#' Three possible parameterization can be specified in \code{"parameterization"} argument: \code{"classic"}
#' returns IRT parameters of reference group and differences in these parameters between reference and focal group.
#' \code{"alternative"} returns IRT parameters of reference group, the differences in parameters a and b between
#' two groups and parameters c and d for focal group. \code{"logistic"} returns parameters in logistic regression
#' parameterization.
#'
#' @return
#' A data.frame containing rows representing items and 5 columns representing parameters of \code{difNLR} model. First column represents discrimination (a), second difficulty (b), third guessing (c), fourth difference in discrimination between reference and focal group (aDif) and fifth difference in difficulty between reference and focal group (bDif).
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
#' # loading data based on GMAT
#' data(GMAT)
#'
#' Data  <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # starting values for 3PL model
#' startNLR(Data, group, model = "3PL")
#'
#' # starting values for 3PL model
#' # with score as matching criterion
#' startNLR(Data, group, model = "3PL", match = "score")
#'
#' }
#' @export

startNLR <- function(Data, group, model, match = "zscore", parameterization = "alternative"){
  if (missing(model)) {
    stop("'model' is missing",
         call. = FALSE)
  } else {
    if (!(model %in% c("Rasch", "1PL", "2PL",
                       "3PLcg", "3PLdg", "3PLc", "3PL", "3PLd",
                       "4PLcgdg", "4PLcgd", "4PLd", "4PLcdg", "4PLc", "4PL"))){
      stop("Invalid value for 'model'",
           call. = FALSE)
    }
  }

  startNLR_line <- function(match, DATA){

    covar <- match

    breaks <- unique(quantile(covar, (0:3) / 3, na.rm = T))
    lb <- length(breaks) - 1
    Q3 <- cut(covar, breaks, include.lowest = TRUE)
    levels(Q3) <- LETTERS[1:lb]

    x <- cbind(mean(covar[Q3 == LETTERS[1]], na.rm = T),
               colMeans(data.frame(DATA[Q3 == LETTERS[1], ]), na.rm = T))
    y <- cbind(mean(covar[Q3 == LETTERS[lb]], na.rm = T),
               colMeans(data.frame(DATA[Q3 == LETTERS[lb], ]), na.rm = T))
    u1 <- y[, 1] - x[, 1]
    u2 <- y[, 2] - x[, 2]

    q <- -(-u1 * y[, 2] + u2 * y[, 1]) / u1

    k <- u2 / u1

    results <- as.data.frame(cbind(k, q))
    return(results)
  }

  if (match[1] == "zscore"){
    MATCH <- scale(apply(Data, 1, sum))
  } else {
    if (match[1] == "score"){
      MATCH <- as.numeric(apply(Data, 1, sum))
    } else {
      if (length(match) == dim(Data)[1]){
        MATCH <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore' or vector of
                the same length as number of observations in 'Data'!")
      }
    }
  }

  line <- startNLR_line(MATCH, DATA = Data)

  data_R <- data.frame(Data[group == 0, ]) ### reference group
  data_F <- data.frame(Data[group == 1, ]) ### foal group
  line_R <- startNLR_line(MATCH[group == 0], DATA = data_R)
  line_F <- startNLR_line(MATCH[group == 1], DATA = data_F)

  if (model %in% c("Rasch", "1PL", "2PL", "3PLdg", "3PLd")){
    c_R <- c_F <- 0
  } else {
    if (grepl("cg", model)){
      c_R <- c_F <- checkInterval(line$k * (-4) + line$q, c(0, 0.99))
    } else {
      c_R <- checkInterval(line_R$k * (-4) + line_R$q, c(0, 0.99))
      c_F <- checkInterval(line_F$k * (-4) + line_F$q, c(0, 0.99))
    }
  }


  if (model %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLc", "3PL")){
    d_R <- d_F <- 1
  } else {
    if (grepl("dg", model)){
      d_R <- d_F <- checkInterval(line$k * 4 + line$q, c(0.01, 1))
    } else {
      d_R <- checkInterval(line_R$k * 4 + line_R$q, c(0.01, 1))
      d_F <- checkInterval(line_F$k * 4 + line_F$q, c(0.01, 1))
    }
  }

  if (model == "Rasch"){
    a_R <- a_F <- 1
  } else {
    if (model == "1PL") {
      a_R <- a_F <- 4 * line$k/(d_R - c_R)
    } else {
      a_R <- 4 * line_R$k/(d_R - c_R)
      a_F <- 4 * line_F$k/(d_F - c_F)
    }
  }

  b_R <- ((d_R + c_R)/2 - line_R$q)/line_R$k
  b_F <- ((d_F + c_F)/2 - line_F$q)/line_F$k

  results <- switch(parameterization,
                    classic = data.frame("a" = a_R, "b" = b_R, "c" = c_R, "d" = d_R,
                                         "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cDif" = c_F - c_R, "dDif" = d_F - d_R),
                    alternative = data.frame("a" = a_R, "b" = b_R, "cR" = c_R, "dR" = d_R,
                                             "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cF" = c_F, "dF" = d_F),
                    logistic = data.frame("b1" = a_R, "b0" = -a_R * b_R, "c" = c_R, "d" = d_R,
                                          "b3" = a_F - a_R, "b2" = -a_R * b_R + a_F * b_F, "cDif" = c_F - c_R, "dDif" = d_F - d_R))

  return(results)
}

