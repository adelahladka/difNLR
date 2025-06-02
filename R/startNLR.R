#' Calculates starting values for non-linear regression DIF models.
#'
#' @description
#' Calculates starting values for the \code{difNLR()} function based on linear
#' approximation.
#'
#' @usage
#' startNLR(Data, group, model, constraints = NULL, match = "zscore",
#'          parameterization = "irt", simplify = FALSE)
#'
#' @param Data data.frame or matrix: dataset in which rows represent
#'   scored examinee answers (\code{"1"} correct, \code{"0"}
#'   incorrect) and columns correspond to the items.
#' @param group numeric: a binary vector of a group membership (\code{"0"}
#'   for the reference group, \code{"1"} for the focal group).
#' @param model character: generalized logistic regression model for
#'   which starting values should be estimated. See \strong{Details}.
#' @param constraints character: which parameters should be the same for both
#'   groups. Possible values are any combinations of parameters \code{"a"},
#'   \code{"b"}, \code{"c"}, and \code{"d"}. Default value is \code{NULL}.
#' @param match character or numeric: matching criterion to be used as
#'   an estimate of the trait. It can be either \code{"zscore"} (default,
#'   standardized total score), \code{"score"} (total test score), or
#'   a numeric vector of the same length as a number of observations in
#'   the \code{Data}.
#' @param parameterization character: parameterization of regression
#'   coefficients. Possible options are \code{"irt"} (IRT parameterization,
#'   default), \code{"is"} (intercept-slope), and \code{"logistic"} (logistic
#'   regression as in the \code{\link[stats]{glm}} function, available for
#'   the \code{"2PL"} model only). See \strong{Details}.
#' @param simplify logical: should initial values be simplified into
#'   the matrix? It is only applicable when parameterization is the
#'   same for all items.
#'
#' @details
#' The unconstrained form of the 4PL generalized logistic regression model for
#' probability of correct answer (i.e., \eqn{Y_{pi} = 1}) using IRT parameterization
#' is
#' \deqn{P(Y_{pi} = 1|X_p, G_p) = (c_{iR} \cdot G_p + c_{iF} \cdot (1 - G_p)) +
#' (d_{iR} \cdot G_p + d_{iF} \cdot (1 - G_p) - c_{iR} \cdot G_p - c_{iF} \cdot
#' (1 - G_p)) / (1 + \exp(-(a_i + a_{i\text{DIF}} \cdot G_p) \cdot
#' (X_p - b_p - b_{i\text{DIF}} \cdot G_p))), }
#' where \eqn{X_p} is the matching criterion (e.g., standardized total score) and
#' \eqn{G_p} is a group membership variable for respondent \eqn{p}.
#' Parameters \eqn{a_i}, \eqn{b_i}, \eqn{c_{iR}}, and \eqn{d_{iR}}
#' are discrimination, difficulty, guessing, and inattention for the reference
#' group for item \eqn{i}. Terms \eqn{a_{i\text{DIF}}} and \eqn{b_{i\text{DIF}}}
#' then represent differences between the focal and reference groups in
#' discrimination and difficulty for item \eqn{i}. Terms \eqn{c_{iF}}, and
#' \eqn{d_{iF}} are guessing and inattention parameters for the focal group for
#' item \eqn{i}. In the case that there is no assumed difference between the
#' reference and focal group in the guessing or inattention parameters, the terms
#' \eqn{c_i} and \eqn{d_i} are used.
#'
#' Alternatively, intercept-slope parameterization may be applied:
#' \deqn{P(Y_{pi} = 1|X_p, G_p) = (c_{iR} \cdot G_p + c_{iF} \cdot (1 - G_p)) +
#' (d_{iR} \cdot G_p + d_{iF} \cdot (1 - G_p) - c_{iR} \cdot G_p - c_{iF} \cdot
#' (1 - G_p)) / (1 + \exp(-(\beta_{i0} + \beta_{i1} \cdot X_p +
#' \beta_{i2} \cdot G_p + \beta_{i3} \cdot X_p \cdot G_p))), }
#' where parameters \eqn{\beta_{i0}, \beta_{i1}, \beta_{i2}, \beta_{i3}} are
#' intercept, effect of the matching criterion, effect of the group membership,
#' and their mutual interaction, respectively.
#'
#' The \code{model} argument offers several predefined models. The options are as follows:
#' \code{Rasch} for 1PL model with discrimination parameter fixed on value 1 for both groups,
#' \code{1PL} for 1PL model with discrimination parameter set the same for both groups,
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
#' Three possible parameterizations can be specified in the
#' \code{"parameterization"} argument: \code{"irt"} returns the IRT parameters
#' of the reference group and differences in these parameters between the
#' reference and focal group. Parameters of asymptotes are printed separately
#' for the reference and focal groups. \code{"is"} returns intercept-slope
#' parameterization. Parameters of asymptotes are again printed separately for
#' the reference and focal groups. \code{"logistic"} returns parameters in
#' logistic regression parameterization as in the \code{\link[stats]{glm}}
#' function, and it is available only for the 2PL model.
#'
#' @return
#' A list containing elements representing items. Each element is a named
#' numeric vector with initial values for the chosen generalized logistic
#' regression model.
#'
#' @author
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @references
#' Drabinova, A. & Martinkova, P. (2017). Detection of differential item functioning with nonlinear regression:
#' A non-IRT approach accounting for guessing. Journal of Educational Measurement, 54(4), 498--517,
#' \doi{10.1111/jedm.12158}.
#'
#' Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression
#' models for DIF and DDF detection. The R Journal, 12(1), 300--323,
#' \doi{10.32614/RJ-2020-014}.
#'
#' Hladka, A. (2021). Statistical models for detection of differential item functioning. Dissertation thesis.
#' Faculty of Mathematics and Physics, Charles University.
#'
#' @seealso \code{\link[difNLR]{difNLR}}
#'
#' @examples
#' # loading data
#' data(GMAT)
#' Data <- GMAT[, 1:20] # items
#' group <- GMAT[, "group"] # group membership variable
#'
#' # 3PL model with the same guessing for both groups
#' startNLR(Data, group, model = "3PLcg")
#' startNLR(Data, group, model = "3PLcg", parameterization = "is")
#' # simplified into a single table
#' startNLR(Data, group, model = "3PLcg", simplify = TRUE)
#' startNLR(Data, group, model = "3PLcg", parameterization = "is", simplify = TRUE)
#'
#' # 2PL model
#' startNLR(Data, group, model = "2PL")
#' startNLR(Data, group, model = "2PL", parameterization = "is")
#' startNLR(Data, group, model = "2PL", parameterization = "logistic")
#'
#' # 4PL model with a total score as the matching criterion
#' startNLR(Data, group, model = "4PL", match = "score")
#' startNLR(Data, group, model = "4PL", match = "score", parameterization = "is")
#'
#' # starting values for model specified for each item
#' startNLR(Data, group,
#'   model = c(
#'     rep("1PL", 5), rep("2PL", 5),
#'     rep("3PL", 5), rep("4PL", 5)
#'   )
#' )
#'
#' # 4PL model with fixed a and c parameters
#' startNLR(Data, group, model = "4PL", constraints = "ac", simplify = TRUE)
#'
#' @export
startNLR <- function(Data, group, model, constraints = NULL, match = "zscore",
                     parameterization = "irt", simplify = FALSE) {
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
  m <- ncol(Data)

  if (length(model) == 1) {
    model <- rep(model, m)
  } else {
    if (length(model) != m) {
      stop("Invalid length of 'model'. Model needs to be specified for each item or
           by single string.", call. = FALSE)
    }
  }
  if (!is.null(constraints) & length(constraints) == 1) {
    constraints <- rep(constraints, m)
  } else if (!is.null(constraints) & length(constraints) != m) {
      stop("Invalid length of 'constraints'. Constraints for initial values needs to be specified
           for each item or by a single string.", call. = FALSE)
  }
  if (length(parameterization) == 1) {
    parameterization <- rep(parameterization, m)
  } else {
    if (length(parameterization) != m) {
      stop("Invalid length of 'parameterization'. Parameterization for initial values needs to be specified
           for each item or by a single string.", call. = FALSE)
    }
  }

  startNLR_line <- function(match, DATA) {
    covar <- match

    breaks <- unique(quantile(covar, (0:3) / 3, na.rm = TRUE))
    lb <- length(breaks) - 1
    if (lb < 2) {
      stop("Not enough complete observations to compute starting values.",
        call. = FALSE
      )
    }
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
    MATCH <- as.vector(scale(apply(Data, 1, sum, na.rm = TRUE)))
  } else {
    if (match[1] == "score") {
      MATCH <- as.numeric(apply(Data, 1, sum, na.rm = TRUE))
    } else {
      if (length(match) == dim(Data)[1]) {
        MATCH <- match
      } else {
        stop("Invalid value for 'match'. Possible values are 'score', 'zscore', or a vector of
              the same length as a number of observations in 'Data'.", call. = FALSE)
      }
    }
  }

  M_R <- mean(MATCH[group == 0], na.rm = TRUE)
  M_F <- mean(MATCH[group == 1], na.rm = TRUE)
  M <- mean(MATCH, na.rm = TRUE)
  SD_R <- sd(MATCH[group == 0], na.rm = TRUE)
  SD_F <- sd(MATCH[group == 1], na.rm = TRUE)
  SD <- sd(MATCH, na.rm = TRUE)
  MATCH <- as.vector(scale(MATCH))

  line <- startNLR_line(MATCH, DATA = Data)

  data_R <- data.frame(Data[group == 0, ]) ### reference group
  data_F <- data.frame(Data[group == 1, ]) ### foal group
  line_R <- startNLR_line(MATCH[group == 0], DATA = data_R)
  line_F <- startNLR_line(MATCH[group == 1], DATA = data_F)

  a_R <- a_F <- b_R <- b_F <- c_R <- c_F <- d_R <- d_F <- c()

  c <- t(sapply(
    1:ncol(Data),
    function(i) {
      if (model[i] %in% c("Rasch", "1PL", "2PL", "3PLdg", "3PLd")) {
        c_R[i] <- c_F[i] <- 0
      } else {
        if (grepl("cg", model[i]) || (!is.null(constraints) && grepl("c", constraints[i]))) {
          c_R[i] <- c_F[i] <- .checkInterval(line$k * (-4) + line$q, c(0, 0.99))[i]
        } else {
          c_R[i] <- .checkInterval(line_R$k * (-4) + line_R$q, c(0, 0.99))[i]
          c_F[i] <- .checkInterval(line_F$k * (-4) + line_F$q, c(0, 0.99))[i]
        }
      }
      return(c(c_R[i], c_F[i]))
    }
  ))
  c_R <- c[, 1]
  c_F <- c[, 2]

  d <- t(sapply(
    1:ncol(Data),
    function(i) {
      if (model[i] %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLc", "3PL")) {
        d_R[i] <- d_F[i] <- 1
      } else {
        if (grepl("dg", model[i]) || (!is.null(constraints) && grepl("d", constraints[i]))) {
          d_R[i] <- d_F[i] <- .checkInterval(line$k * 4 + line$q, c(0.01, 1))[i]
        } else {
          d_R[i] <- .checkInterval(line_R$k * 4 + line_R$q, c(0.01, 1))[i]
          d_F[i] <- .checkInterval(line_F$k * 4 + line_F$q, c(0.01, 1))[i]
        }
      }
      return(c(d_R[i], d_F[i]))
    }
  ))
  d_R <- d[, 1]
  d_F <- d[, 2]

  a <- t(sapply(
    1:ncol(Data),
    function(i) {
      if (model[i] == "Rasch") {
        a_R[i] <- a_F[i] <- 1
      } else {
        if (model[i] == "1PL" || (!is.null(constraints) && grepl("a", constraints[i]))) {
          a_R[i] <- a_F[i] <- ((4 * line$k / (d_R - c_R))[i]) / SD
        } else {
          a_R[i] <- ((4 * line_R$k / (d_R - c_R))[i]) / SD_R
          a_F[i] <- ((4 * line_F$k / (d_F - c_F))[i]) / SD_F
        }
      }
      return(c(a_R[i], a_F[i]))
    }
  ))
  a_R <- a[, 1]
  a_F <- a[, 2]

  b <- t(sapply(
    1:ncol(Data),
    function(i) {
      if (!is.null(constraints) && grepl("b", constraints[i])) {
        c_tmp <- mean(c_R[i], c_F[i])
        d_tmp <- mean(d_R[i], d_F[i])

        b_R[i] <- b_F[i] <- (((d_tmp + c_tmp) / 2 - line$q[i]) / line$k[i]) * SD + M
      } else {
        b_R[i] <- (((d_R[i] + c_R[i]) / 2 - line_R$q[i]) / line_R$k[i]) * SD_R + M_R
        b_F[i] <- (((d_F[i] + c_F[i]) / 2 - line_F$q[i]) / line_F$k[i]) * SD_F + M_F
      }
      return(c(b_R[i], b_F[i]))
    }
  ))
  b_R <- b[, 1]
  b_F <- b[, 2]

  mod <- lapply(1:ncol(Data), function(i) {
    tmp <- switch(model[i],
      # 1PL models
      "Rasch"   = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
      "1PL"     = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
      # 2PL models
      "2PL"     = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
      # 3PL models
      "3PLcg"   = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
      "3PLdg"   = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      "3PLc"    = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
      "3PL"     = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
      "3PLd"    = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE),
      # 4PL models
      "4PLcgdg" = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
      "4PLcgd"  = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
      "4PLd"    = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
      "4PLcdg"  = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
      "4PLc"    = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
      "4PL"     = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
    )
    names(tmp) <- c("a", "b", "cR", "dR", "aDif", "bDif", "cF", "dF")
    if (!is.null(constraints) && grepl("a", constraints[i])) {
      tmp["aDif"] <- FALSE
    }
    if (!is.null(constraints) && grepl("b", constraints[i])) {
      tmp["bDif"] <- FALSE
    }
    if (!is.null(constraints) && grepl("c", constraints[i])) {
      tmp["cF"] <- FALSE
    }
    if (!is.null(constraints) && grepl("d", constraints[i])) {
      tmp["dF"] <- FALSE
    }
    return(tmp)
  })

  if (length(unique(parameterization)) == 1 & simplify & (is.null(constraints) || length(unique(constraints)) == 1)) {
    results <- switch(unique(parameterization),
      irt = data.frame(
        "a" = a_R, "b" = b_R, "cR" = c_R, "dR" = d_R,
        "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cF" = c_F, "dF" = d_F
      )[mod[[1]]],
      is = data.frame(
        "b1" = a_R, "b0" = -a_R * b_R, "cR" = c_R, "dR" = d_R,
        "b3" = a_F - a_R, "b2" = a_R * b_R - a_F * b_F, "cF" = c_F, "dF" = d_F
      )[mod[[1]]],
      logistic = data.frame(
        "b1" = a_R, "b0" = -a_R * b_R, "c" = c_R, "d" = d_R,
        "b3" = a_F - a_R, "b2" = a_R * b_R - a_F * b_F, "cF" = c_F, "dF" = d_F
      )[mod[[1]]]
    )

    if (!("cF" %in% colnames(results)) & ("cR" %in% colnames(results)) || (!is.null(constraints) && grepl("c", constraints[1]))) {
      colnames(results)[colnames(results) == "cR"] <- "c"
    }
    if (!("dF" %in% colnames(results)) & ("dR" %in% colnames(results)) || (!is.null(constraints) && grepl("d", constraints[1]))) {
      colnames(results)[colnames(results) == "dR"] <- "d"
    }
  } else {
    results <- lapply(1:ncol(Data), function(i) {
      tmp <- switch(parameterization[i],
        irt = data.frame(
          "a" = a_R, "b" = b_R, "cR" = c_R, "dR" = d_R,
          "aDif" = a_F - a_R, "bDif" = b_F - b_R, "cF" = c_F, "dF" = d_F
        )[i, ][mod[[i]]],
        is = data.frame(
          "b1" = a_R, "b0" = -a_R * b_R, "cR" = c_R, "dR" = d_R,
          "b3" = a_F - a_R, "b2" = -a_R * b_R + a_F * b_F, "cF" = c_F, "dF" = d_F
        )[i, ][mod[[i]]],
        logistic = data.frame(
          "b1" = a_R, "b0" = -a_R * b_R, "c" = c_R, "d" = d_R,
          "b3" = a_F - a_R, "b2" = -a_R * b_R + a_F * b_F, "cF" = c_F, "dF" = d_F
        )[i, ][mod[[i]]]
      )
      if (!("cF" %in% colnames(tmp)) & ("cR" %in% colnames(tmp)) || (!is.null(constraints) && grepl("c", constraints[i]))) {
        colnames(tmp)[colnames(tmp) == "cR"] <- "c"
      }
      if (!("dF" %in% colnames(tmp)) & ("dR" %in% colnames(tmp)) || (!is.null(constraints) && grepl("d", constraints[i]))) {
        colnames(tmp)[colnames(tmp) == "dR"] <- "d"
      }

      return(tmp)
    })
  }

  if (any(sapply(results, function(x) sum(is.na(x))) > 0)) {
    stop(paste(
      "Not enough complete observations to compute starting values for items:",
      paste(colnames(Data)[sapply(results, function(x) sum(is.na(x))) > 0], collapse = ", ")
    ), call. = FALSE)
  }

  return(results)
}
