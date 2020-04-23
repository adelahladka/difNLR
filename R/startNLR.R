#' Calculates starting values for non-linear regression DIF models.
#'
#' @param Data Data data.frame or matrix: dataset which rows represent scored examinee answers (\code{"1"}
#' correct, \code{"0"} incorrect) and columns correspond to the items.
#' @param group numeric: binary vector of group membership. \code{"0"} for reference group, \code{"1"} for
#' focal group.
#' @param model character: generalized logistic regression model for which starting values should be
#' estimated. See \strong{Details}.
#' @param match character or numeric: matching criterion to be used as estimate of trait. Can be
#' either \code{"zscore"} (default, standardized total score), \code{"score"} (total test score),
#' or numeric vector of the same length as number of observations in \code{Data}.
#' @param parameterization character: parameterization of regression
#' coefficients. Possible options are \code{"classic"} (IRT parameterization),
#' \code{"alternative"} (default) and \code{"logistic"} (logistic regression).
#' See \strong{Details}.
#' @param simplify logical: should initial values be simplified into the matrix?
#' This is only applicable when parameterization is the same for all items.
#'
#' @description Calculates starting values for \code{difNLR()} function based
#' on linear approximation.
#'
#' @usage
#' startNLR(Data, group, model, match = "zscore", parameterization = "alternative",
#'          simplify = FALSE)
#'
#' @details
#' The unconstrained form of 4PL generalized logistic regression model for probability of correct
#' answer (i.e., \eqn{y = 1}) is
#' \deqn{P(y = 1) = (c + cDif*g) + (d + dDif*g - c - cDif*g)/(1 + exp(-(a + aDif*g)*(x - b - bDif*g))), }
#' where \eqn{x} is by default standardized total score (also called Z-score) and \eqn{g} is a group membership.
#' Parameters \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{d} are discrimination, difficulty, guessing, and inattention.
#' Terms \eqn{aDif}, \eqn{bDif}, \eqn{cDif}, and \eqn{dDif} then represent differences between two groups
#' (reference and focal) in relevant parameters.
#'
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
#' \code{"alternative"} returns IRT parameters of reference group, the differences in parameters \code{"a"} and
#' \code{"b"} between two groups and parameters \code{"c"} and \code{"d"} for focal group.
#' \code{"logistic"} returns parameters in logistic regression parameterization.
#'
#' @return
#' A list containing elements representing items. Each element is a named numeric vector of length 8
#' with initial values for generalized logistic regression model.
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
#' @seealso \code{\link[difNLR]{difNLR}}
#'
#' @examples
#' # loading data based on GMAT
#' data(GMAT)
#'
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, "group"]
#'
#' # starting values for 3PL model
#' startNLR(Data, group, model = "3PL")
#'
#' # starting values for 3PL model
#' # simplified into single table
#' startNLR(Data, group, model = "3PL", simplify = TRUE)
#'
#' # starting values for 3PL model
#' # with score as matching criterion
#' startNLR(Data, group, model = "3PL", match = "score")
#'
#' # starting values for model specified for each item
#' startNLR(Data, group,
#'   model = c(
#'     rep("1PL", 5), rep("2PL", 5),
#'     rep("3PL", 5), rep("4PL", 5)
#'   )
#' )
#' @export
startNLR <- function(Data, group, model, match = "zscore", parameterization = "alternative",
                     simplify = FALSE) {
  if (missing(model)) {
    stop("'model' is missing.",
      call. = FALSE
    )
  } else {
    if (!all(model %in% c(
      "Rasch", "1PL", "2PL",
      "3PLcg", "3PLdg", "3PLc", "3PL", "3PLd",
      "4PLcgdg", "4PLcgd", "4PLd", "4PLcdg", "4PLc", "4PL"
    ))) {
      stop("Invalid value for 'model'.",
        call. = FALSE
      )
    }
  }
  Data <- as.data.frame(Data)
  if (length(model) == 1) {
    model <- rep(model, ncol(Data))
  } else {
    if (length(model) != ncol(Data)) {
      stop("Invalid length of 'model'. Model needs to be specified for each item or
           by single string.", call. = FALSE)
    }
  }
  if (length(parameterization) == 1) {
    parameterization <- rep(parameterization, ncol(Data))
  } else {
    if (length(parameterization) != ncol(Data)) {
      stop("Invalid length of 'parameterization'. Parameterization for initial values needs to be specified
           for each item or by single string.", call. = FALSE)
    }
  }

  startNLR_line <- function(match, DATA) {
    covar <- match

    breaks <- unique(quantile(covar, (0:3) / 3, na.rm = TRUE))
    lb <- length(breaks) - 1
    Q3 <- cut(covar, breaks, include.lowest = TRUE)
    levels(Q3) <- LETTERS[1:lb]

    x <- cbind(
      mean(covar[Q3 == LETTERS[1]], na.rm = TRUE),
      colMeans(data.frame(DATA[Q3 == LETTERS[1], ]), na.rm = TRUE)
    )
    y <- cbind(
      mean(covar[Q3 == LETTERS[lb]], na.rm = TRUE),
      colMeans(data.frame(DATA[Q3 == LETTERS[lb], ]), na.rm = TRUE)
    )
    u1 <- y[, 1] - x[, 1]
    u2 <- y[, 2] - x[, 2]

    q <- -(-u1 * y[, 2] + u2 * y[, 1]) / u1

    k <- u2 / u1

    results <- as.data.frame(cbind(k, q))
    return(results)
  }

  if (match[1] == "zscore") {
    MATCH <- scale(apply(Data, 1, sum))
  } else {
    if (match[1] == "score") {
      MATCH <- as.numeric(apply(Data, 1, sum))
    } else {
      if (length(match) == dim(Data)[1]) {
        MATCH <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore', or vector of
                the same length as number of observations in 'Data'.", call. = FALSE)
      }
    }
  }

  line <- startNLR_line(MATCH, DATA = Data)

  data_R <- data.frame(Data[group == 0, ]) ### reference group
  data_F <- data.frame(Data[group == 1, ]) ### foal group
  line_R <- startNLR_line(MATCH[group == 0], DATA = data_R)
  line_F <- startNLR_line(MATCH[group == 1], DATA = data_F)

  a_R <- a_F <- b_R <- b_F <- c_R <- c_F <- d_R <- d_F <- c()

  c <- sapply(
    1:ncol(Data),
    function(i) {
      if (model[i] %in% c("Rasch", "1PL", "2PL", "3PLdg", "3PLd")) {
        c_R[i] <- c_F[i] <- 0
      } else {
        if (grepl("cg", model[i])) {
          c_R[i] <- c_F[i] <- checkInterval(line$k * (-4) + line$q, c(0, 0.99))[i]
        } else {
          c_R[i] <- checkInterval(line_R$k * (-4) + line_R$q, c(0, 0.99))[i]
          c_F[i] <- checkInterval(line_F$k * (-4) + line_F$q, c(0, 0.99))[i]
        }
      }
      return(c(c_R[i], c_F[i]))
    }
  )
  c_R <- t(c)[, 1]
  c_F <- t(c)[, 2]

  d <- sapply(
    1:ncol(Data),
    function(i) {
      if (model[i] %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLc", "3PL")) {
        d_R[i] <- d_F[i] <- 1
      } else {
        if (grepl("dg", model[i])) {
          d_R[i] <- d_F[i] <- checkInterval(line$k * 4 + line$q, c(0.01, 1))[i]
        } else {
          d_R[i] <- checkInterval(line_R$k * 4 + line_R$q, c(0.01, 1))[i]
          d_F[i] <- checkInterval(line_F$k * 4 + line_F$q, c(0.01, 1))[i]
        }
      }
      return(c(d_R[i], d_F[i]))
    }
  )
  d_R <- t(d)[, 1]
  d_F <- t(d)[, 2]

  a <- sapply(
    1:ncol(Data),
    function(i) {
      if (model[i] == "Rasch") {
        a_R[i] <- a_F[i] <- 1
      } else {
        if (model[i] == "1PL") {
          a_R[i] <- a_F[i] <- (4 * line$k / (d_R - c_R))[i]
        } else {
          a_R[i] <- (4 * line_R$k / (d_R - c_R))[i]
          a_F[i] <- (4 * line_F$k / (d_F - c_F))[i]
        }
      }
      return(c(a_R[i], a_F[i]))
    }
  )
  a_R <- t(a)[, 1]
  a_F <- t(a)[, 2]

  b_R <- ((d_R + c_R) / 2 - line_R$q) / line_R$k
  b_F <- ((d_F + c_F) / 2 - line_F$q) / line_F$k

  if (length(unique(parameterization)) == 1 & simplify) {
    results <- switch(unique(parameterization),
      classic = data.frame(
        "a" = a_R, "b" = b_R, "c" = c_R, "d" = d_R,
        "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cDif" = c_F - c_R, "dDif" = d_F - d_R
      ),
      alternative = data.frame(
        "a" = a_R, "b" = b_R, "cR" = c_R, "dR" = d_R,
        "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cF" = c_F, "dF" = d_F
      ),
      logistic = data.frame(
        "b1" = a_R, "b0" = -a_R * b_R, "c" = c_R, "d" = d_R,
        "b3" = a_F - a_R, "b2" = -a_R * b_R + a_F * b_F, "cDif" = c_F - c_R, "dDif" = d_F - d_R
      )
    )
  } else {
    results <- lapply(1:ncol(Data), function(i) {
      switch(parameterization[i],
        classic = data.frame(
          "a" = a_R, "b" = b_R, "c" = c_R, "d" = d_R,
          "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cDif" = c_F - c_R, "dDif" = d_F - d_R
        )[i, ],
        alternative = data.frame(
          "a" = a_R, "b" = b_R, "cR" = c_R, "dR" = d_R,
          "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cF" = c_F, "dF" = d_F
        )[i, ],
        logistic = data.frame(
          "b1" = a_R, "b0" = -a_R * b_R, "c" = c_R, "d" = d_R,
          "b3" = a_F - a_R, "b2" = -a_R * b_R + a_F * b_F, "cDif" = c_F - c_R, "dDif" = d_F - d_R
        )[i, ]
      )
    })
  }

  return(results)
}
