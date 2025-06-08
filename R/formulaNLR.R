#' Creates a formula for non-linear regression DIF models.
#'
#' @description
#' The function returns the formula of the non-linear regression DIF model based
#' on model specification and DIF type to be tested.
#'
#' @usage
#' formulaNLR(model, constraints = NULL, type = "all", parameterization = "irt",
#'            outcome)
#'
#' @param model character: generalized logistic regression model for which
#'   starting values should be estimated. See \strong{Details}.
#' @param constraints character: which parameters should be the same for both
#'   groups. Possible values are any combinations of parameters \code{"a"},
#'   \code{"b"}, \code{"c"}, and \code{"d"}. Default value is \code{NULL}.
#' @param type character: type of DIF to be tested. Possible values are
#'   \code{"all"} for detecting difference in any parameter (default),
#'   \code{"udif"} for uniform DIF only (i.e., difference in difficulty
#'   parameter \code{"b"}),
#'   \code{"nudif"} for non-uniform DIF only (i.e., difference in discrimination
#'   parameter \code{"a"}),
#'   \code{"both"} for uniform and non-uniform DIF (i.e., difference in
#'   parameters \code{"a"} and \code{"b"}),
#'   or any combination of parameters \code{"a"}, \code{"b"}, \code{"c"}, and
#'   \code{"d"}. Can be specified as a single value (for all items) or as an
#'   item-specific vector.
#' @param parameterization character: parameterization of regression
#'   coefficients. Possible options are \code{"irt"} (IRT parameterization,
#'   default), \code{"is"} (intercept-slope), and \code{"logistic"} (logistic
#'   regression as in the \code{\link[stats]{glm}} function, available for
#'   the \code{"2PL"} model only). See \strong{Details}.
#' @param outcome character: name of outcome to be printed in formula. If not
#'   specified \code{"y"} is used.
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
#' A list of two models. Each includes a formula, parameters to be estimated,
#' and their lower and upper constraints.
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
#' @seealso \code{\link[difNLR]{difNLR}}
#'
#' @examples
#' # 3PL model with the same guessing parameter for both groups
#' # to test both types of DIF
#' formulaNLR(model = "3PLcg", type = "both")
#' formulaNLR(model = "3PLcg", type = "both", parameterization = "is")
#'
#' # 4PL model with the same guessing and inattention parameters
#' # to test uniform DIF
#' formulaNLR(model = "4PLcgdg", type = "udif")
#' formulaNLR(model = "4PLcgdg", type = "udif", parameterization = "is")
#'
#' # 2PL model to test non-uniform DIF
#' formulaNLR(model = "2PL", type = "nudif")
#' formulaNLR(model = "2PL", type = "nudif", parameterization = "is")
#' formulaNLR(model = "2PL", type = "nudif", parameterization = "logistic")
#'
#' # 4PL model to test all possible DIF
#' formulaNLR(model = "4PL", type = "all", parameterization = "irt")
#' formulaNLR(model = "4PL", type = "all", parameterization = "is")
#'
#' # 4PL model with fixed a and c parameters
#' # to test difference in b
#' formulaNLR(model = "4PL", constraints = "ac", type = "b")
#' formulaNLR(model = "4PL", constraints = "ac", type = "b", parameterization = "is")
#' @export
#' @importFrom stats as.formula
formulaNLR <- function(model, constraints = NULL, type = "all", parameterization = "irt", outcome) {
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
  if (model %in% c("Rasch", "1PL")) {
    if (type == "both") {
      warning("Only uniform DIF can be tested with the Rasch or 1PL model.", call. = FALSE)
    }
    if (type == "nudif") {
      stop("It is not possible to test non-uniform DIF in the Rasch or 1PL model.", call. = FALSE)
    }
  }
  if (parameterization == "logistic" & model != "2PL") {
    stop("Logistic parameterization is available only for the 2PL model", call. = FALSE)
  }

  # model - largest possible model
  mod1 <- logical(8)
  mod1 <- switch(model,
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
  names(mod1) <- c("a", "b", "c", "d", "aDif", "bDif", "cF", "dF")

  # constraints for larger model
  cons <- rep(TRUE, 8)
  names(cons) <- c("a", "b", "c", "d", "aDif", "bDif", "cF", "dF")

  if (!is.null(constraints)) {
    if (any(!is.na(constraints))) {
      constr <- unlist(strsplit(constraints, split = ""))
      if (!all(constr %in% letters[1:4])) {
        stop("Constraints can be only 'a', 'b', 'c', or 'd'!", call. = FALSE)
      }
      cons <- apply(sapply(constr, function(x) {
        if (x %in% c("c", "d")) {
          cons[paste0(x, "F")] <- FALSE
        } else {
          cons[paste0(x, "Dif")] <- FALSE
        }
        cons
      }), 1, all)
    }
  }
  mod1 <- mod1 & cons

  # submodel
  mod0 <- mod1
  # type of DIF to be tested within submodel
  if (type == "udif") {
    mod0["bDif"] <- FALSE
    mod0["aDif"] <- FALSE
    mod1["aDif"] <- FALSE
  } else if (type == "nudif") {
    mod0["aDif"] <- FALSE
  } else if (type == "both") {
    mod0["bDif"] <- FALSE
    mod0["aDif"] <- FALSE
  } else if (type == "all") {
    mod0[c("aDif", "bDif", "cF", "dF")] <- FALSE
  } else {
    types <- unlist(strsplit(type, split = ""))
    if (!all(types %in% letters[1:4])) {
      stop("Type of DIF to be tested not recognized. Only parameters 'a', 'b', 'c', or 'd' can be tested
           or 'type' must be one of predefined options: either 'udif', 'nudif', 'both', or 'all'.", call. = FALSE)
    }
    if (any(!is.na(constraints))) {
      if (length(intersect(types, constr)) > 0) {
        stop("The difference in constrained parameters cannot be tested.", call. = FALSE)
      }
    }

    mod0 <- apply(sapply(types, function(x) {
      if (x %in% c("c", "d")) {
        mod0[paste0(x, "F")] <- FALSE
      } else {
        mod0[paste0(x, "Dif")] <- FALSE
      }
      mod0
    }), 1, all)
  }

  # reparametrizations
  if (parameterization == "logistic") {
    mod0 <- c(
      "(Intercept)" = as.logical(mod0[["b"]]) | as.logical(mod0[["a"]]), # note, intercept always estimated, even if discrimination is not
      "x" = as.logical(mod0[["a"]]),
      "g" = as.logical(mod0[["bDif"]]), # | as.logical(mod0[["aDif"]]),
      "x:g" = as.logical(mod0[["aDif"]])
    )
    mod1 <- c(
      "(Intercept)" = as.logical(mod1[["b"]]) | as.logical(mod1[["a"]]), # note, intercept always estimated, even if discrimination is not
      "x" = as.logical(mod1[["a"]]),
      "g" = as.logical(mod1[["bDif"]]), # | as.logical(mod1[["aDif"]]),
      "x:g" = as.logical(mod1[["aDif"]])
    )
    part_expit0 <- names(mod0[-1])[mod0[-1]]
    part_expit0 <- .MYpaste(part_expit0, collapse = " + ")
    part_asym0 <- ""

    part_expit1 <- names(mod1[-1])[mod1[-1]]
    part_expit1 <- .MYpaste(part_expit1, collapse = " + ")
    part_asym1 <- ""
  } else if (parameterization == "is") {
    mod0 <- c(
      "b0" = as.logical(mod0[["b"]]) | as.logical(mod0[["a"]]), # note, intercept always estimated, even if discrimination is not
      "b1" = as.logical(mod0[["a"]]),
      "b2" = as.logical(mod0[["bDif"]]), # | as.logical(mod0[["aDif"]]),
      "b3" = as.logical(mod0[["aDif"]]),
      "c" = as.logical(mod0[["c"]]),
      "cF" = as.logical(mod0[["cF"]]),
      "d" = as.logical(mod0[["d"]]),
      "dF" = as.logical(mod0[["dF"]])
    )
    mod1 <- c(
      "b0" = as.logical(mod1[["b"]]) | as.logical(mod1[["a"]]), # note, intercept always estimated, even if discrimination is not
      "b1" = as.logical(mod1[["a"]]),
      "b2" = as.logical(mod1[["bDif"]]), # | as.logical(mod1[["aDif"]]),
      "b3" = as.logical(mod1[["aDif"]]),
      "c" = as.logical(mod1[["c"]]),
      "cF" = as.logical(mod1[["cF"]]),
      "d" = as.logical(mod1[["d"]]),
      "dF" = as.logical(mod1[["dF"]])
    )
    # expit part
    mod0_expit <- mod0[c("b0", "b1", "b2", "b3")]
    part_expit0 <- paste0(paste0(names(mod0_expit), c("", " * x", " * g", " * x * g"))[mod0_expit], collapse = " + ")
    part_expit0 <- ifelse(model == "Rasch", paste(part_expit0, "+ x"), part_expit0)
    part_expit0 <- paste0(" / (1 + exp(-(", part_expit0, ")))")

    # expit part
    mod1_expit <- mod1[c("b0", "b1", "b2", "b3")]
    part_expit1 <- paste0(paste0(names(mod1_expit), c("", " * x", " * g", " * x * g"))[mod1_expit], collapse = " + ")
    part_expit1 <- ifelse(model == "Rasch", paste(part_expit1, "+ x"), part_expit1)
    part_expit1 <- paste0(" / (1 + exp(-(", part_expit1, ")))")
  } else {
    # expit part - a
    mod0_expit_a0 <- mod0[c("a", "aDif")]
    part_a0 <- paste0(paste0(names(mod0_expit_a0), c("", " * g"))[mod0_expit_a0], collapse = " + ")
    part_a0 <- ifelse(nchar(part_a0) <= 1, part_a0, paste0("(", part_a0, ")"))
    # expit part - b
    mod0_expit_b0 <- mod0[c("b", "bDif")]
    part_b0 <- paste0(paste0(names(mod0_expit_b0), c("", " * g"))[mod0_expit_b0], collapse = " + ")
    part_b0 <- ifelse(nchar(part_b0) <= 1, part_b0, paste0("(", part_b0, ")"))
    # expit part
    part_expit0 <- paste0("/ (1 + exp(-", .MYpaste(c(part_a0, paste0("(x - ", part_b0, ")")), collapse = " * "), "))")

    # expit part - a
    mod1_expit_a1 <- mod1[c("a", "aDif")]
    part_a1 <- paste0(paste0(names(mod1_expit_a1), c("", " * g"))[mod1_expit_a1], collapse = " + ")
    part_a1 <- ifelse(nchar(part_a1) <= 1, part_a1, paste0("(", part_a1, ")"))
    # expit part - b
    mod1_expit_b1 <- mod1[c("b", "bDif")]
    part_b1 <- paste0(paste0(names(mod1_expit_b1), c("", " * g"))[mod1_expit_b1], collapse = " + ")
    part_b1 <- ifelse(nchar(part_b1) <= 1, part_b1, paste0("(", part_b1, ")"))
    # expit part
    part_expit1 <- paste0("/ (1 + exp(-", .MYpaste(c(part_a1, paste0("(x - ", part_b1, ")")), collapse = " * "), "))")
  }

  if (parameterization != "logistic") {
    # c asymptote
    if (mod0["cF"]) {
      names(mod0)[names(mod0) %in% c("c", "cF")] <- c("cR", "cF")
      mod0_c0 <- mod0[c("cR", "cF")]
      part_c0 <- paste0(paste(names(mod0_c0), c("* (1 - g)", "* g"))[mod0_c0], collapse = " + ")
    } else {
      mod0_c0 <- mod0[c("c", "cF")]
      part_c0 <- paste0(paste0(names(mod0_c0), c("", " * g"))[mod0_c0], collapse = " + ")
    }
    part_c0 <- ifelse(nchar(part_c0) > 1, paste0("(", part_c0, ")"), part_c0)

    # d asymptote
    if (mod0["dF"]) {
      names(mod0)[names(mod0) %in% c("d", "dF")] <- c("dR", "dF")
      mod0_d0 <- mod0[c("dR", "dF")]
      part_d0 <- paste0(paste(names(mod0_d0), c("* (1 - g)", "* g"))[mod0_d0], collapse = " + ")
    } else {
      mod0_d0 <- mod0[c("d", "dF")]
      part_d0 <- paste0(paste0(names(mod0_d0), c("", " * g"))[mod0_d0], collapse = " + ")
    }
    part_d0 <- ifelse(part_d0 == "" & (grepl(3, model) | grepl(4, model)), 1, part_d0)

    # d - c
    part_dc0 <- .MYpaste(c(part_d0, part_c0), collapse = " - ")
    part_dc0 <- ifelse(part_dc0 == "" & (grepl(3, model) | grepl(4, model)), 1, part_dc0)
    part_dc0 <- ifelse(nchar(part_dc0) > 1, paste0("(", part_dc0, ")"), part_dc0)
    # c + (d - c) asymptotes
    part_asym0 <- ifelse(grepl(3, model) | grepl(4, model), .MYpaste(c(part_c0, part_dc0), collapse = " + "), 1)

    # c asymptote
    if (mod1["cF"]) {
      names(mod1)[names(mod1) %in% c("c", "cF")] <- c("cR", "cF")
      mod1_c1 <- mod1[c("cR", "cF")]
      part_c1 <- paste0(paste(names(mod1_c1), c("* (1 - g)", "* g"))[mod1_c1], collapse = " + ")
    } else {
      mod1_c1 <- mod1[c("c", "cF")]
      part_c1 <- paste0(paste0(names(mod1_c1), c("", " * g"))[mod1_c1], collapse = " + ")
    }
    part_c1 <- ifelse(nchar(part_c1) > 1, paste0("(", part_c1, ")"), part_c1)

    # d asymptote
    if (mod1["dF"]) {
      names(mod1)[names(mod1) %in% c("d", "dF")] <- c("dR", "dF")
      mod1_d1 <- mod1[c("dR", "dF")]
      part_d1 <- paste0(paste(names(mod1_d1), c("* (1 - g)", "* g"))[mod1_d1], collapse = " + ")
    } else {
      mod1_d1 <- mod1[c("d", "dF")]
      part_d1 <- paste0(paste0(names(mod1_d1), c("", " * g"))[mod1_d1], collapse = " + ")
    }
    part_d1 <- ifelse(part_d1 == "" & (grepl(3, model) | grepl(4, model)), 1, part_d1)

    # d - c
    part_dc1 <- .MYpaste(c(part_d1, part_c1), collapse = " - ")
    part_dc1 <- ifelse(part_dc1 == "" & (grepl(3, model) | grepl(4, model)), 1, part_dc1)
    part_dc1 <- ifelse(nchar(part_dc1) > 1, paste0("(", part_dc1, ")"), part_dc1)
    # c + (d - c) asymptotes
    part_asym1 <- ifelse(grepl(3, model) | grepl(4, model), .MYpaste(c(part_c1, part_dc1), collapse = " + "), 1)
  } else {
    part_asym0 <- part_asym1 <- ""
  }

  # formula
  form0 <- paste0(part_asym0, part_expit0)
  form1 <- paste0(part_asym1, part_expit1)

  # formula - adding outcome
  if (missing(outcome)) {
    outcome <- "y"
  }
  form0 <- paste(outcome, "~", form0)
  form1 <- paste(outcome, "~", form1)

  lower <- switch(parameterization,
    "irt" = c(-Inf, -Inf, 0, 0, -Inf, -Inf, 0, 0),
    "is" = c(-Inf, -Inf, -Inf, -Inf, 0, 0, 0, 0),
    "logistic" = NULL
  )
  upper <- switch(parameterization,
    "irt" = c(Inf, Inf, 1, 1, Inf, Inf, 1, 1),
    "is" = c(Inf, Inf, Inf, Inf, 1, 1, 1, 1),
    "logistic" = NULL
  )

  if (parameterization != "logistic") {
    lower0 <- setNames(lower[mod0], names(mod0[mod0]))
    upper0 <- setNames(upper[mod0], names(mod0[mod0]))
    lower1 <- setNames(lower[mod1], names(mod1[mod1]))
    upper1 <- setNames(upper[mod1], names(mod1[mod1]))
  } else {
    lower0 <- upper0 <- lower1 <- upper1 <- NULL
  }

  return(list(
    M0 = list(
      formula = as.formula(form0),
      parameters = names(mod0[mod0]),
      lower = lower0,
      upper = upper0
    ),
    M1 = list(
      formula = as.formula(form1),
      parameters = names(mod1[mod1]),
      lower = lower1,
      upper = upper1
    )
  ))
}
