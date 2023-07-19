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
#' \deqn{P(y = 1) = (c + cDif * g) + (d + dDif * g - c - cDif * g) / (1 + exp(-(a + aDif * g) * (x - b - bDif * g))), }
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
#' \code{"logistic"} returns parameters in logistic regression parameterization and it is available only for 2PL model.
#'
#' @return A list of two models. Each includes a formula, parameters to be estimated, and their lower and upper constraints.
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
#'
#' # 2PL model with logistic parameterization
#' formulaNLR(model = "2PL", parameterization = "logistic")
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
      stop("Invalid value for the 'model' argument.", call. = FALSE)
    }
  }

  if (parameterization == "logistic" & model != "2PL") {
    stop("Logistic parameterization is available only for the 2PL model", call. = FALSE)
  }

  # constraints for model
  cons <- rep(TRUE, 8)
  names(cons) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")

  if (!is.null(constraints)) {
    if (any(!is.na(constraints))) {
      constr <- unlist(strsplit(constraints, split = ""))
      if (!all(constr %in% letters[1:4])) {
        warning("Constraints can be only 'a', 'b', 'c', or 'd'!", call. = FALSE)
      }
      cons[paste0(constr, "Dif")] <- FALSE
    }
  }

  # model
  mod <- logical(8)
  mod <- switch(model,
    # 1 PL models
    "Rasch"   = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    "1PL"     = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    # 2 PL models
    "2PL"     = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    # 3 PL models
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

  names(mod) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
  mod <- apply(cbind(mod, cons), 1, all)

  # type of DIF to be tested
  typ <- typcons <- mod

  if (!(type %in% c("udif", "nudif", "both", "all"))) {
    types <- unlist(strsplit(type, split = ""))
    if (!all(types %in% letters[1:4])) {
      stop("Type of DIF to be tested not recognized. Only parameters 'a', 'b', 'c', or 'd' can be tested
           or 'type' must be one of predefined options: either 'udif', 'nudif', 'both', or 'all'.", call. = FALSE)
    }
    typcons[paste0(types, "Dif")] <- FALSE
    type <- "other"
  }

  if (type == "other") {
    if (any(!is.na(constraints))) {
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

  if (parameterization == "XXX") {
    mod0 <- c(
      "b0" = as.logical(mod0[["b"]]), # note, intercept always estimated, even if discrimination is not
      "b1" = as.logical(mod0[["a"]]),
      "b2" = as.logical(-mod0[["a"]] * mod0[["bDif"]] -
                   mod0[["aDif"]] * mod0[["b"]] -
                   mod0[["aDif"]] * mod0[["bDif"]]),
      "b3" = as.logical(mod0[["aDif"]]),
      "c" = as.logical(mod0[["c"]]),
      "cDif" = as.logical(mod0[["cDif"]]),
      "d" = as.logical(mod0[["d"]]),
      "dDif" = as.logical(mod0[["dDif"]])
    )
    mod1 <- c(
      "b0" = as.logical(mod1[["b"]]), # note, intercept always estimated, even if discrimination is not
      "b1" = as.logical(mod1[["a"]]),
      "b2" = as.logical(-mod1[["a"]] * mod1[["bDif"]] -
                   mod1[["aDif"]] * mod1[["b"]] -
                   mod1[["aDif"]] * mod1[["bDif"]]),
      "b3" = as.logical(mod1[["aDif"]]),
      "c" = as.logical(mod1[["c"]]),
      "cDif" = as.logical(mod1[["cDif"]]),
      "d" = as.logical(mod1[["d"]]),
      "dDif" = as.logical(mod1[["dDif"]])
    )
    # expit part
    mod0_expit <- mod0[c("b0", "b1", "b2", "b3")]
    part_expit0 <- paste0(paste0(names(mod0_expit), c("", " * x", " * g", " * x * g"))[mod0_expit], collapse = " + ")
    part_expit0 <- paste0(" / (1 + exp(-(", part_expit0, ")))")
    # c asymptote
    mod0_c0 <- mod0[c("c", "cDif")]
    part_c0 <- paste0(paste0(names(mod0_c0), c("", " * g"))[mod0_c0], collapse = " + ")
    # d asymptote
    mod0_d0 <- mod0[c("d", "dDif")]
    part_d0 <- paste0(paste0(names(mod0_d0), c("", " * g"))[mod0_d0], collapse = " + ")
    part_d0 <- ifelse(part_d0 == "", 1, part_d0)
    # d - c
    part_dc0 <- paste0("(", part_d0, " - ", ifelse(nchar(part_c0) == 1, part_c0, paste0("(", part_c0, ")")), ")")
    part_dc0 <- ifelse(part_dc0 == "", 1, part_dc0)
    # formula
    form0 <- paste0(paste(c(part_c0, part_dc0), collapse = " + "), part_expit0, collapse = " * ")

    # expit part
    mod1_expit <- mod1[c("b0", "b1", "b2", "b3")]
    part_expit1 <- paste0(paste0(names(mod1_expit), c("", " * x", " * g", " * x * g"))[mod1_expit], collapse = " + ")
    part_expit1 <- paste0(" / (1 + exp(-(", part_expit1, ")))")
    # c asymptote
    mod1_c1 <- mod1[c("c", "cDif")]
    part_c1 <- paste0(paste0(names(mod1_c1), c("", " * g"))[mod1_c1], collapse = " + ")
    # d asymptote
    mod1_d1 <- mod1[c("d", "dDif")]
    part_d1 <- paste0(paste0(names(mod1_d1), c("", " * g"))[mod1_d1], collapse = " + ")
    part_d1 <- ifelse(part_d1 == "", 1, part_d1)
    # d - c
    part_dc1 <- paste0("(", part_d1, " - ", ifelse(nchar(part_c1) == 1, part_c1, paste0("(", part_c1, ")")), ")")
    part_dc1 <- ifelse(part_dc1 == "", 1, part_dc1)
    # formula
    form1 <- paste0(paste(c(part_c1, part_dc1), collapse = " + "), part_expit1, collapse = " * ")

  } else if (parameterization == "logistic") {
    mod0 <- c(
      "(Intercept)" = as.logical(mod0[["b"]]), # note, intercept always estimated, even if discrimination is not
      "x" = as.logical(mod0[["a"]]),
      "g" = as.logical(-mod0[["a"]] * mod0[["bDif"]] -
        mod0[["aDif"]] * mod0[["b"]] -
        mod0[["aDif"]] * mod0[["bDif"]]),
      "x:g" = as.logical(mod0[["aDif"]])
    )
    mod1 <- c(
      "(Intercept)" = as.logical(mod1[["b"]]), # note, intercept always estimated, even if discrimination is not
      "x" = as.logical(mod1[["a"]]),
      "g" = as.logical(-mod1[["a"]] * mod1[["bDif"]] -
        mod1[["aDif"]] * mod1[["b"]] -
        mod1[["aDif"]] * mod1[["bDif"]]),
      "x:g" = as.logical(mod1[["aDif"]])
    )
    part_regr0 <- names(mod0[-1])[mod0[-1]]
    form0 <- paste(part_regr0[part_regr0 != ""], collapse = " + ")
    part_regr1 <- names(mod1[-1])[mod1[-1]]
    form1 <- paste(part_regr1[part_regr1 != ""], collapse = " + ")
  } else {

    # expit part - a
    mod0_expit_a0 <- mod0[c("a", "aDif")]
    part_a0 <- paste0(paste0(names(mod0_expit_a0), c("", " * g"))[mod0_expit_a0], collapse = " + ")
    part_a0 <- ifelse(nchar(part_a0) == 1, part_a0, paste0("(", part_a0, ")"))
    # expit part - b
    mod0_expit_b0 <- mod0[c("b", "bDif")]
    part_b0 <- paste0(paste0(names(mod0_expit_b0), c("", " * g"))[mod0_expit_b0], collapse = " + ")
    part_b0 <- ifelse(nchar(part_b0) == 1, part_b0, paste0("(", part_b0, ")"))
    # expit part
    part_expit0 <- paste0("/ (1 + exp(-", paste(c(part_a0, paste0("(x - ", part_b0, ")")), collapse = " * "), ")")
    # c asymptote
    mod0_c0 <- mod0[c("c", "cDif")]
    part_c0 <- paste0(paste0(names(mod0_c0), c("", " * g"))[mod0_c0], collapse = " + ")
    # d asymptote
    mod0_d0 <- mod0[c("d", "dDif")]
    part_d0 <- paste0(paste0(names(mod0_d0), c("", " * g"))[mod0_d0], collapse = " + ")
    part_d0 <- ifelse(part_d0 == "", 1, part_d0)
    # d - c
    part_dc0 <- paste0("(", part_d0, " - ", ifelse(nchar(part_c0) == 1, part_c0, paste0("(", part_c0, ")")), ")")
    part_dc0 <- ifelse(part_dc0 == "", 1, part_dc0)
    # formula
    form0 <- paste0(paste(c(part_c0, part_dc0), collapse = " + "), part_expit0, collapse = " * ")

    # expit part - a
    mod1_expit_a1 <- mod1[c("a", "aDif")]
    part_a1 <- paste0(paste0(names(mod1_expit_a1), c("", " * g"))[mod1_expit_a1], collapse = " + ")
    part_a1 <- ifelse(nchar(part_a1) == 1, part_a1, paste0("(", part_a1, ")"))
    # expit part - b
    mod1_expit_b1 <- mod1[c("b", "bDif")]
    part_b1 <- paste0(paste0(names(mod1_expit_b1), c("", " * g"))[mod1_expit_b1], collapse = " + ")
    part_b1 <- ifelse(nchar(part_b1) == 1, part_b1, paste0("(", part_b1, ")"))
    # expit part
    part_expit1 <- paste0("/ (1 + exp(-", paste(c(part_a1, paste0("(x - ", part_b1, ")")), collapse = " * "), ")")
    # c asymptote
    mod1_c1 <- mod1[c("c", "cDif")]
    part_c1 <- paste0(paste0(names(mod1_c1), c("", " * g"))[mod1_c1], collapse = " + ")
    # d asymptote
    mod1_d1 <- mod1[c("d", "dDif")]
    part_d1 <- paste0(paste0(names(mod1_d1), c("", " * g"))[mod1_d1], collapse = " + ")
    part_d1 <- ifelse(part_d1 == "", 1, part_d1)
    # d - c
    part_dc1 <- paste0("(", part_d1, " - ", ifelse(nchar(part_c1) == 1, part_c1, paste0("(", part_c1, ")")), ")")
    part_dc1 <- ifelse(part_dc1 == "", 1, part_dc1)
    # formula
    form1 <- paste0(paste(c(part_c1, part_dc1), collapse = " + "), part_expit1, collapse = " * ")
  }
  #   # parameters in model 0 and model 1
  #   param0 <- param1 <- list()
  #
  #   for (i in 1:4) {
  #     param0[[i]] <- paste(names(which(mod0[c(i, i + 4)])), collapse = " + g * ")
  #     param1[[i]] <- paste(names(which(mod1[c(i, i + 4)])), collapse = " + g * ")
  #   }
  #
  #   if (parameterization == "alternative") {
  #     names(mod0)[3:4] <- names(mod1)[3:4] <- c("cR", "dR")
  #     names(mod0)[7:8] <- names(mod1)[7:8] <- c("cF", "dF")
  #     for (i in 3:4) {
  #       param0[[i]] <- ifelse(length(which(mod0[c(i, i + 4)])) == 1,
  #         names(which(mod0[c(i, i + 4)])),
  #         paste(names(which(mod0[c(i, i + 4)])), c("* (1 - g)", "* g")[mod0[c(i, i + 4)]], collapse = " + ")
  #       )
  #       param1[[i]] <- ifelse(length(which(mod1[c(i, i + 4)])) == 1,
  #         names(which(mod1[c(i, i + 4)])),
  #         paste(names(which(mod1[c(i, i + 4)])), c("* (1 - g)", "* g")[mod1[c(i, i + 4)]], collapse = " + ")
  #       )
  #     }
  #   }
  #
  #   if (param0[[3]] == "") param0[[3]] <- "0"
  #   if (param0[[4]] == "") param0[[4]] <- "1"
  #
  #   if (param1[[3]] == "") param1[[3]] <- "0"
  #   if (param1[[4]] == "") param1[[4]] <- "1"
  #
  #   if (param0[[1]] == "") param0[[1]] <- "1"
  #   if (param1[[1]] == "") param1[[1]] <- "1"
  #
  #   # formulas for model 0 and model 1
  #   param0[grepl(" ", param0)] <- paste0("(", param0[grepl(" ", param0)], ")")
  #   param1[grepl(" ", param1)] <- paste0("(", param1[grepl(" ", param1)], ")")
  #
  #   # (d - c)
  #   part_dc0 <- paste(c(param0[4], param0[3]), collapse = " - ")
  #   part_dc0[grepl(" ", part_dc0)] <- paste0("(", part_dc0[grepl(" ", part_dc0)], ")")
  #   part_dc1 <- paste(c(param1[4], param1[3]), collapse = " - ")
  #   part_dc1[grepl(" ", part_dc1)] <- paste0("(", part_dc1[grepl(" ", part_dc1)], ")")
  #
  #   # c + (d - c)
  #   part_cdc0 <- paste(c(param0[3], part_dc0), collapse = " + ")
  #   part_cdc1 <- paste(c(param1[3], part_dc1), collapse = " + ")
  #
  #   # x - b
  #   part_xb0 <- paste0("(x - ", param0[2], ")")
  #   part_xb1 <- paste0("(x - ", param1[2], ")")
  #
  #   # a * (x - b)
  #   part_axb0 <- paste(param0[1], " * ", part_xb0)
  #   part_axb1 <- paste(param1[1], " * ", part_xb1)
  #
  #   # 1 + exp(- a * (x - b))
  #   if (parameterization == "logistic") {
  #     part_exp0 <- part_axb0
  #     part_exp1 <- part_axb1
  #   } else {
  #     part_exp0 <- paste0("1 + exp(-", part_axb0, ")")
  #     part_exp0 <- paste0("(", part_exp0, ")")
  #     part_exp1 <- paste0("1 + exp(-", part_axb1, ")")
  #     part_exp1 <- paste0("(", part_exp1, ")")
  #   }
  #
  #   # model
  #   part_regr0 <- paste(part_cdc0, "/", part_exp0)
  #   part_regr1 <- paste(part_cdc1, "/", part_exp1)
  # }

  # formula - adding outcome
  if (missing(outcome)) {
    outcome <- "y"
  }
  form0 <- paste(outcome, "~", form0)
  form1 <- paste(outcome, "~", form1)

  lower <- switch(parameterization,
    "classic" = c(-Inf, -Inf, 0, 0, -Inf, -Inf, -1, -1),
    "alternative" = c(-Inf, -Inf, 0, 0, -Inf, -Inf, 0, 0),
    "logistic" = NULL
  )
  upper <- switch(parameterization,
    "classic" = c(Inf, Inf, 1, 1, Inf, Inf, 1, 1),
    "alternative" = c(Inf, Inf, 1, 1, Inf, Inf, 1, 1),
    "logistic" = NULL
  )

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
