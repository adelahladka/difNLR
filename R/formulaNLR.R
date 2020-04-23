#' Formula for non-linear regression DIF model.
#'
#' @aliases formulaNLR
#'
#' @description Function returns the formula of the non-linear regression DIF model
#' based on model specification and DIF type to be tested.
#'
#' @param model character: generalized logistic regression model for which starting values should be
#' estimated. See \strong{Details}.
#' @param type character: type of DIF to be tested. Possible values are \code{"all"} for detecting
#' difference in any parameter (default), \code{"udif"} for uniform DIF only (i.e., difference in
#' difficulty parameter \code{"b"}), \code{"nudif"} for non-uniform DIF only (i.e., difference in
#' discrimination parameter \code{"a"}), \code{"both"} for uniform and non-uniform DIF (i.e.,
#' difference in parameters \code{"a"} and \code{"b"}), or combination of parameters \code{"a"},
#' \code{"b"}, \code{"c"}, and \code{"d"}. Can be specified as a single value (for all items) or as
#' an item-specific vector.
#' @param constraints character: which parameters should be the same for both groups. Possible values
#' are any combinations of parameters \code{"a"}, \code{"b"}, \code{"c"}, and \code{"d"}. Default value
#' is \code{NULL}. See \strong{Details}.
#' @param parameterization character: parameterization of regression
#' coefficients. Possible options are \code{"classic"} (IRT parameterization),
#' \code{"alternative"} (default) and \code{"logistic"} (logistic regression).
#' See \strong{Details}.
#' @param outcome character: name of outcome to be printed in formula. If not specified \code{"y"} is used.
#'
#' @usage
#' formulaNLR(model, constraints = NULL, type = "all", parameterization = "classic", outcome)
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
#' @return A list of two models. Both includes formula, parameters to be estimated and their lower and upper constraints.
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
#' @seealso \code{\link[difNLR]{difNLR}}
#'
#' @examples
#' # 3PL model with the same guessing for both groups
#' # to test both types of DIF
#' formulaNLR(model = "3PLcg", type = "both")
#'
#' # 4PL model with the same guessing and inattention
#' # to test uniform DIF
#' formulaNLR(model = "4PLcgdg", type = "udif")
#'
#' # 2PL model to test non-uniform DIF
#' formulaNLR(model = "2PL", type = "nudif")
#'
#' # 4PL model to test all possible DIF
#' # with alternative parameterization
#' formulaNLR(model = "4PL", type = "all", parameterization = "alternative")
#'
#' # 4PL model with fixed a and c parameter
#' # to test difference in b with alternative parameterization
#' formulaNLR(model = "4PL", constraints = "ac", type = "b", parameterization = "alternative")
#' @export
#' @importFrom stats as.formula

formulaNLR <- function(model, constraints = NULL, type = "all", parameterization = "classic", outcome) {
  if (missing(model)) {
    stop("Argument 'model' is missing.", call. = FALSE)
  } else {
    if (!(model %in% c(
      "Rasch", "1PL", "2PL",
      "3PLcg", "3PLdg", "3PLc", "3PL", "3PLd",
      "4PLcgdg", "4PLcgd", "4PLd", "4PLcdg", "4PLc", "4PL"
    ))) {
      stop("Invalid value for 'model'.", call. = FALSE)
    }
  }

  # constraints for model
  cons <- rep(T, 8)
  names(cons) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")

  if (!is.null(constraints)) {
    if (!is.na(constraints)) {
      constr <- unlist(strsplit(constraints, split = ""))
      if (!all(constr %in% letters[1:4])) {
        warning("Constraints can be only 'a', 'b', 'c' or 'd'!", call. = F)
      }
      cons[paste(constr, "Dif", sep = "")] <- F
    }
  }

  # model
  mod <- logical(8)
  mod <- switch(model,
    # 1 PL models
    "Rasch"   = c(F, T, F, F, F, T, F, F),
    "1PL"     = c(T, T, F, F, F, T, F, F),
    # 2 PL models
    "2PL"     = c(T, T, F, F, T, T, F, F),
    # 3 PL models
    "3PLcg"   = c(T, T, T, F, T, T, F, F),
    "3PLdg"   = c(T, T, F, T, T, T, F, F),
    "3PLc"    = c(T, T, T, F, T, T, T, F),
    "3PL"     = c(T, T, T, F, T, T, T, F),
    "3PLd"    = c(T, T, F, T, T, T, F, T),
    # 4PL models
    "4PLcgdg" = c(T, T, T, T, T, T, F, F),
    "4PLcgd"  = c(T, T, T, T, T, T, F, T),
    "4PLd"    = c(T, T, T, T, T, T, F, T),
    "4PLcdg"  = c(T, T, T, T, T, T, T, F),
    "4PLc"    = c(T, T, T, T, T, T, T, F),
    "4PL"     = c(T, T, T, T, T, T, T, T)
  )

  names(mod) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
  mod <- apply(cbind(mod, cons), 1, all)

  # type of DIF to be tested
  typ <- typcons <- mod

  if (!(type %in% c("udif", "nudif", "both", "all"))) {
    types <- unlist(strsplit(type, split = ""))
    if (!all(types %in% letters[1:4])) {
      stop("Type of DIF to be tested not recognized. Only parameters 'a', 'b', 'c', or 'd' can be tested
           or 'type' must be one of predefined options: either 'udif', 'nudif', 'both', or 'all'", call. = FALSE)
    }
    typcons[paste(types, "Dif", sep = "")] <- F
    type <- "other"
  }

  if (type == "other") {
    if (!is.na(constraints)) {
      if (length(intersect(types, constr)) > 0) {
        stop("The difference in constrained parameters cannot be tested.", call. = FALSE)
      }
    }
  }

  switch(type,
    "udif" = {
      typ["bDif"] <- FALSE
      typ["aDif"] <- FALSE
      mod["aDif"] <- FALSE
    },
    "nudif" = {
      typ["aDif"] <- FALSE
    },
    "both" = {
      typ["bDif"] <- FALSE
      typ["aDif"] <- FALSE
    },
    "all" = {
      typ[c("aDif", "bDif", "cDif", "dDif")] <- FALSE
    },
    "other" = {
      typ <- typcons
    }
  )

  if (model %in% c("Rasch", "1PL")) {
    typ["aDif"] <- FALSE
    if (type == "both") {
      warning("Only uniform DIF can be tested with specified model.", call. = FALSE)
    }
    if (type == "nudif") {
      stop("It is not possible to test non-uniform DIF in specified model.", call. = FALSE)
    }
  }

  # model 0 and model 1
  mod0 <- typ
  mod1 <- mod

  # parameters in model 0 and model 1
  param0 <- param1 <- list()

  for (i in 1:4) {
    param0[[i]] <- paste(names(which(mod0[c(i, i + 4)])), collapse = " + g * ")
    param1[[i]] <- paste(names(which(mod1[c(i, i + 4)])), collapse = " + g * ")
  }

  if (parameterization == "alternative") {
    names(mod0)[3:4] <- names(mod1)[3:4] <- c("cR", "dR")
    names(mod0)[7:8] <- names(mod1)[7:8] <- c("cF", "dF")
    for (i in 3:4) {
      param0[[i]] <- ifelse(length(which(mod0[c(i, i + 4)])) == 1,
        names(which(mod0[c(i, i + 4)])),
        paste(names(which(mod0[c(i, i + 4)])), c("* (1 - g)", "* g")[mod0[c(i, i + 4)]], collapse = " + ")
      )
      param1[[i]] <- ifelse(length(which(mod1[c(i, i + 4)])) == 1,
        names(which(mod1[c(i, i + 4)])),
        paste(names(which(mod1[c(i, i + 4)])), c("* (1 - g)", "* g")[mod1[c(i, i + 4)]], collapse = " + ")
      )
    }
  }

  if (param0[[3]] == "") param0[[3]] <- "0"
  if (param0[[4]] == "") param0[[4]] <- "1"

  if (param1[[3]] == "") param1[[3]] <- "0"
  if (param1[[4]] == "") param1[[4]] <- "1"

  if (param0[[1]] == "") param0[[1]] <- "1"
  if (param1[[1]] == "") param1[[1]] <- "1"

  # formulas for model 0 and model 1
  param0[grepl(" ", param0)] <- paste("(", param0[grepl(" ", param0)], ")", sep = "")
  param1[grepl(" ", param1)] <- paste("(", param1[grepl(" ", param1)], ")", sep = "")

  ### (d - c)
  part_dc0 <- paste(c(param0[4], param0[3]), collapse = " - ")
  part_dc0[grepl(" ", part_dc0)] <- paste("(", part_dc0[grepl(" ", part_dc0)], ")", sep = "")
  part_dc1 <- paste(c(param1[4], param1[3]), collapse = " - ")
  part_dc1[grepl(" ", part_dc1)] <- paste("(", part_dc1[grepl(" ", part_dc1)], ")", sep = "")

  ### c + (d - c)
  part_cdc0 <- paste(c(param0[3], part_dc0), collapse = " + ")
  part_cdc1 <- paste(c(param1[3], part_dc1), collapse = " + ")

  ### x - b
  part_xb0 <- paste("(x - ", param0[2], ")", sep = "")
  part_xb1 <- paste("(x - ", param1[2], ")", sep = "")

  ### a*(x - b)
  part_axb0 <- paste(param0[1], "*", part_xb0)
  part_axb1 <- paste(param1[1], "*", part_xb1)

  ### 1 + exp(-a*(x - b))
  part_exp0 <- paste("1 + exp(-", part_axb0, ")", sep = "")
  part_exp0 <- paste("(", part_exp0, ")", sep = "")
  part_exp1 <- paste("1 + exp(-", part_axb1, ")", sep = "")
  part_exp1 <- paste("(", part_exp1, ")", sep = "")

  # model
  part_abcd0 <- paste(part_cdc0, "/", part_exp0)
  part_abcd1 <- paste(part_cdc1, "/", part_exp1)

  ### formula
  if (missing(outcome)) {
    outcome <- "y"
  }
  form0 <- paste(outcome, "~", part_abcd0)
  form1 <- paste(outcome, "~", part_abcd1)

  if (parameterization == "classic") {
    lower <- c(-Inf, -Inf, 0, 0, -Inf, -Inf, -1, -1)
    upper <- c(Inf, Inf, 1, 1, Inf, Inf, 1, 1)
  } else {
    lower <- c(-Inf, -Inf, 0, 0, -Inf, -Inf, 0, 0)
    upper <- c(Inf, Inf, 1, 1, Inf, Inf, 1, 1)
  }

  return(list(
    M0 = list(
      formula = as.formula(form0),
      parameters = names(mod0[mod0]),
      lower = lower[mod0],
      upper = upper[mod0]
    ),
    M1 = list(
      formula = as.formula(form1),
      parameters = names(mod1[mod1]),
      lower = lower[mod1],
      upper = upper[mod1]
    )
  ))
}
