#' Constraints for Non-Linear Regression Model
#'
#' @aliases constrNLR
#'
#' @description Specifies the constraints for Non-Linear Regression model.
#'
#' @param model character: generalized logistic regression model to be fitted. See \strong{Details}.
#' @param type character: type of DIF to be tested (either "both" (default), "udif", or "nudif"). See \strong{Details}.
#'
#' @usage constrNLR(model, type = "both")
#'
#' @details
#' Function specifies constraints for 4PL generalized logistic regression model with
#' respect to \code{model} choice and \code{type} of DIF to be tested.
#'
#' The options of \code{model} are as follows: \code{Rasch} for one-parameter logistic model with
#' discrimination parameter fixed on value 1 for both groups, \code{1PL} for one-parameter logistic
#' model with discrimination parameter fixed for both groups, \code{2PL} for logistic regression model,
#' \code{3PLcg} for three-parameter logistic regression model with fixed guessing for both groups,
#' \code{3PLdg} for three-parameter logistic regression model with fixed inattention for both groups, or
#' \code{4PLcgdg} for four-parameter logistic regression model with fixed guessing and inattention
#' parameter for both groups.
#'
#' The \code{type} corresponds to type of DIF to be tested. Possible values are \code{"both"} to
#' detect any DIF (uniform and/or non-uniform), \code{"udif"} to detect only uniform DIF or
#' \code{"nudif"} to detect only non-uniform DIF.
#' return(rbind(lowerM0, upperM0, lowerM1, upperM1))
#'
#' @return A matrix with 4 rows and 8 columns. First two rows represent lower and
#' upper constraints for null model. Last two rows represent lower and upper constraints
#' for alternative model. First four columns represent parameters of 4PL generalized
#' logistic regression model (a, b, c, d) for reference group. Last four columns represent
#' differences in parameters between reference and focal group (aDif, bDif, cDif, dDif).
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
#' @examples
#' \dontrun{
#' # 3PL model with the same guessing for both groups
#' # to test both types of DIF
#' constrNLR(model = "3PLcg", type = "both")
#'
#' # 3PL model to test uniform DIF
#' constrNLR(model = "3PL", type = "udif")
#'
#' # 2PL model to test non-uniform DIF
#' constrNLR(model = "2PL", type = "nudif")
#' }
#' @export

constrNLR <- function(model, type = "both"){
  if (missing(model)) {
    stop("'model' is missing",
         call. = FALSE)
  } else {
    if (!(model %in% c("Rasch", "1PL", "2PL", "3PLcg", "3PLdg", "4PLcgdg"))){
      stop("Invalid value for 'model'",
           call. = FALSE)
    }
  }
  if (!type %in% c("udif", "nudif", "both") | !is.character(type))
    stop("'type' must be either 'udif', 'nudif' or 'both'",
         call. = FALSE)

  lower <- c(a = -Inf, b = -Inf, c = 0, d = 0, aDif = -Inf, bDif = -Inf, cDif = 0, dDif = 0)
  upper <- c(a = Inf, b = Inf, c = 1, d = 1, aDif = Inf, bDif = Inf, cDif = 0, dDif = 0)

  if (grepl("4", model)){
    if (!(grepl("cgdg", model))){
      upper["cDif"] <- upper["dDif"] <- 1
      lower["cDif"] <- lower["dDif"] <- -1
      if (grepl("cg", model)){
        lower["cDif"] <- upper["cDif"] <- 0 # TBS
      } else {
        if (grepl("dg", model))
          lower["dDif"] <- upper["dDif"] <- 0 # TBS
      }
    }
  } else {
    if (grepl("3", model)){
      if (!(grepl("d", model))){
        lower["d"] <- 1
        if (!(grepl("g", model))){
          upper["cDif"] <- 1 # TBS
          lower["cDif"] <- -1 # TBS
        }
      } else {
        upper["c"] <- 0
        if (!(grepl("g", model))){
          upper["dDif"] <- 1 # TBS
          lower["dDif"] <- -1 # TBS
        }
      }
    } else {
      upper["c"] <- 0
      lower["d"] <- 1
      if (!(grepl("2", model))){
        lower["aDif"] <- upper["aDif"] <- 0
        if (!(grepl("1", model)))
          lower["a"] <- upper["a"] <- 1
      }
    }
  }

  lowerM0 <- lowerM1 <- lower
  upperM0 <- upperM1 <- upper

  lowerM1["aDif"] <- upperM1["aDif"] <- 0

  if (type == "udif"){
    lowerM0["aDif"] <- upperM0["aDif"] <- 0
    lowerM1["bDif"] <- upperM1["bDif"] <- 0
  }
  if (type == "both")
    lowerM1["bDif"] <- upperM1["bDif"] <- 0

  return(rbind(lowerM0, upperM0, lowerM1, upperM1))
}
