#' Formula for Non-Linear Regression Model
#'
#' @aliases formulaNLR
#'
#' @description Based on model specification and DIF type to be tester function returns the formula of the non-linear
#' models.
#'
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param type character: type of DIF to be tested. Possible values are "both" (default), "udif", "nudif", "all",
#' or combination of parameters 'a', 'b', 'c' and 'd'. See \strong{Details}.
#' @param constraints character: which parameters should be the same for both groups. Default value is \code{NULL}. See \strong{Details}.
#' @param parameterization character: which parameterization should be used. Possible values are "classic" (default)
#' and "alternative". See \strong{Details}.
#' @param outcome character: name of outcome to be printed in formula. If not specified 'y' is used.
#'
#' @usage formulaNLR(model, constraints = NULL, type = "both", parameterization = "classic",
#' outcome)
#'
#' @details
#' The unconstrained form of 4PL generalized logistic regression model for probability of correct answer (i.e., y = 1) is
#' P(y = 1) = (c + cDif*g) + (d + dDif*g - c - cDif*g)/(1 + exp(-(a + aDif*g)*(x - b - bDif*g))),
#' where x is standardized total score (also called Z-score) and g is group membership. Parameters a, b, c and d
#' are discrimination, difficulty, guessing and inattention. Parameters aDif, bDif, cDif and dDif
#' then represetn differences between two groups in discrimination, difficulty, guessing and inattention.
#'
#' This 4PL model can be further constrained by \code{model} and \code{constraints} arguments.
#' The arguments \code{model} and \code{constraints} can be also combined.
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
#' The \code{model} can be specified in more detail with \code{constraints} argument which specifies what
#' arguments should be fixed for both groups. For example, choice 'ad' means that discrimination (a) and
#' inattention (d) are fixed for both groups and other parameters (b and c) are not.
#'
#' The \code{type} corresponds to type of DIF to be tested. Possible values are
#' \code{"both"} to detect any DIF caused by difference in difficulty or discrimination (i.e., uniform and/or non-uniform),
#' \code{"udif"} to detect only uniform DIF (i.e., difference in difficulty b),
#' \code{"nudif"} to detect only non-uniform DIF (i.e., difference in discrimination a), or
#' \code{"all"} to detect DIF caused by difference caused by any parameter that can differed between groups. The \code{type}
#' of DIF can be also specified in more detail by using combination of parameters a, b, c and d. For example, with an option
#' 'c' for 4PL model only the difference in parameter c is tested.
#'
#' For an option "alternative" in \code{parameterization} argument, all models with the different guessing or/and inattention
#' parameters are reparameterized as follows:
#' P(y = 1) = (cR*(1-g) + cF*g) + (dR*(1-g) + dF*g - cR*(1-g) - cF*g)/(1 + exp(-(a + aDif*g)*(x - b - bDif*g))).
#'
#'
#' @return A list of two models. Both includes formula, parameters to be estimated and their lower and upper constraints.
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
#' @examples
#' \dontrun{
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
#' formulaNLR(model = "4PL", type = "all")
#'
#' # 4PL model with fixed a and c parameter
#' # to test difference in b
#' formulaNLR(model = "4PL", constraints = "ac", type = "b")
#'
#' # 4PL model with fixed a and c parameter
#' # to test difference in b with alternative parameterization
#' formulaNLR(model = "4PL", constraints = "ac", type = "b", parameterization = "alternative")
#' }
#'
#' @export
#' @importFrom stats as.formula

formulaNLR <- function(model, constraints = NULL, type = "both", parameterization = "classic", outcome){
  if (missing(model)) {
    stop("Argument 'model' is missing")
  } else {
    if (!(model %in% c("Rasch", "1PL", "2PL",
                       "3PLcg", "3PLdg", "3PLc", "3PL", "3PLd",
                       "4PLcgdg", "4PLcgd", "4PLd", "4PLcdg", "4PLc", "4PL"))){
      stop("Invalid value for 'model'")
    }
  }

  # constraints for model
  cons <- rep(T, 8); names(cons) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")

  if (!(is.null(constraints))){
    constr <- unlist(strsplit(constraints, split = ""))
    if (!all(constr %in% letters[1:4])){
      warning("Constraints can be only 'a', 'b', 'c' or 'd'!")
    }
    cons[paste(constr, "Dif", sep = "")] <- F
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
                "4PL"     = c(T, T, T, T, T, T, T, T))

  names(mod) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
  mod <- apply(cbind(mod, cons), 1, all)

  # type of DIF to be tested
  # typ <- logical(8); names(typ) <- c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")
  typ <- typcons <- mod

  if (!(type %in% c("udif", "nudif", "both", "all"))){
    types <- unlist(strsplit(type, split = ""))
    if (!all(types %in% letters[1:4])){
      stop("Type of DIF to be tested not recognized. Only parameters 'a', 'b', 'c' or 'd' can be tested
           or 'type' must be one of predefined options: either 'udif', 'nudif', 'both', or 'all'")
    }
    typcons[paste(types, "Dif", sep = "")] <- F
    type <- "other"
  }

  if (type == "other"){
    if (!is.null(constraints)){
      if (length(intersect(types, constr)) > 0){
        stop("The difference in constrained parameters cannot be tested!")
      }
    }
  }

  switch(type,
         "udif"  = {typ["bDif"] <- F; typ["aDif"] <- F; mod["aDif"] <- F},
         "nudif" = {typ["aDif"] <- F},
         "both"  = {typ["bDif"] <- F; typ["aDif"] <- F},
         "all"   = {typ[c("aDif", "bDif", "cDif", "dDif")] <- F},
         "other" = {typ <- typcons})

  if (model %in% c("Rasch", "1PL")){
    typ["aDif"] <- F
    if (type == "both"){
      warning("Only uniform DIF can be tested with specified model!")
    }
    if (type == "nudif"){
      stop("It is not possible to test non-uniform DIF in specified model!")
    }
  }

  # model 0 and model 1
  mod0 <- mod
  mod1 <- typ

  # parameters in model 0 and model 1
  param0 <- param1 <- list()


  for (i in 1:4){
    param0[[i]] <- paste(names(which(mod0[c(i, i + 4)])), collapse = " + g * ")
    param1[[i]] <- paste(names(which(mod1[c(i, i + 4)])), collapse = " + g * ")
  }

  if (parameterization == "alternative"){
    names(mod0)[3:4] <- names(mod1)[3:4] <- c("cR", "dR")
    names(mod0)[7:8] <- names(mod1)[7:8] <- c("cF", "dF")
    for (i in 3:4){
      param0[[i]] <- paste(names(which(mod0[c(i, i + 4)])), c("* (1 - g)", "* g")[mod0[c(i, i + 4)]], collapse = " + ")
      param1[[i]] <- paste(names(which(mod1[c(i, i + 4)])), c("* (1 - g)", "* g")[mod1[c(i, i + 4)]], collapse = " + ")
    }
  }

  if (param0[[3]] == "") param0[[3]] <- "0"
  if (param0[[4]] == "") param0[[4]] <- "1"

  if (param1[[3]] == "") param1[[3]] <- "0"
  if (param1[[4]] == "") param1[[4]] <- "1"

  if (param0[[1]] == "") param0[[1]] <- "1"
  if (param1[[1]] == "") param1[[1]] <- "1"

  # formulas for model 0 and model 1

  param0 <- paste("(", param0, ")", sep = "")
  param1 <- paste("(", param1, ")", sep = "")


  ### (d - c)
  part_dc0  <- paste(c(param0[4], param0[3]), collapse = " - ")
  part_dc0  <- paste("(", part_dc0, ")", sep = "")
  part_dc1  <- paste(c(param1[4], param1[3]), collapse = " - ")
  part_dc1  <- paste("(", part_dc1, ")", sep = "")

  ### c + (d - c)
  part_cdc0 <- paste(c(param0[3], part_dc0), collapse = " + ")
  part_cdc1 <- paste(c(param1[3], part_dc1), collapse = " + ")

  ### x - b
  part_xb0   <- paste("(x - ", param0[2], ")", sep = "")
  part_xb1   <- paste("(x - ", param1[2], ")", sep = "")

  ### a*(x - b)
  part_axb0  <- paste(param0[1], "*", part_xb0)
  part_axb1  <- paste(param1[1], "*", part_xb1)

  ### 1 + exp(-a*(x - b))
  part_exp0 <- paste("1 + exp(-", part_axb0, ")", sep = "")
  part_exp0 <- paste("(", part_exp0, ")", sep = "")
  part_exp1 <- paste("1 + exp(-", part_axb1, ")", sep = "")
  part_exp1 <- paste("(", part_exp1, ")", sep = "")

  # model
  part_abcd0 <- paste(part_cdc0, "/", part_exp0)
  part_abcd1 <- paste(part_cdc1, "/", part_exp1)

  ### formula
  if (missing(outcome)){
    outcome <- "y"
  }
  form0 <- paste(outcome, "~", part_abcd0)
  form1 <- paste(outcome, "~", part_abcd1)

  if (parameterization == "classic"){
    lower <- c(-Inf, -Inf, 0, 0, -Inf, -Inf, -1, -1)
    upper <- c(Inf, Inf, 1, 1, Inf, Inf, 1, 1)
  } else {
    lower <- c(-Inf, -Inf, 0, 0, -Inf, -Inf, 0, 0)
    upper <- c(Inf, Inf, 1, 1, Inf, Inf, 1, 1)
  }

  return(list(M0 = list(formula = as.formula(form0),
                        parameters = names(mod0[mod0]),
                        lower = lower[mod0],
                        upper = upper[mod0]),
              M1 = list(formula = as.formula(form1),
                        parameters = names(mod1[mod1]),
                        lower = lower[mod1],
                        upper = upper[mod1])))
}
