#' Generates data set based on Non-Linear Regression DIF a DDF models.
#'
#' @param N numeric: number of rows representing respondents.
#' @param ratio numeric: ratio of respondents number in reference and focal group.
#' @param itemtype character: type of items to be generated. Options are \code{"dich"} for
#' dichotomous item (default), \code{"nominal"} for nominal items, and \code{"ordinal"} for
#' ordinal data. See \strong{Details}.
#' @param a numeric: matrix representing discriminations with m rows
#' (where m is number of items). Need to be provided. See \strong{Details}.
#' @param b numeric: numeric: matrix representing difficulties with m rows
#' (where m is number of items). Need to be provided. See \strong{Details}.
#' @param c numeric: matrix representing guessings (lower asymptotes) with m rows
#' (where m is number of items). Default is \code{NULL}. See \strong{Details}.
#' @param d numeric: matrix representing inattentions (upper asymptotes) with m rows
#' (where m is number of items). Default is \code{NULL}. See \strong{Details}.
#' @param mu numeric: a mean vector of the underlying distribution. The first value corresponds to
#' reference group, the second to focal group. Default is 0 value for both groups. See \strong{Details}.
#' @param sigma numeric: a standard deviation vector of the underlying distribution. The first value corresponds to
#' reference group, the second to focal group. Default is 1 value for both groups. See \strong{Details}.
#'
#' @description Generates dichotomous and nominal data set based on non-linear regression
#' models for DIF and DDF detection.
#'
#' @details
#'
#' The \code{itemtype} argument specify what type of item should be generated. In case
#' \code{itemtype = "dich"}, dichotomous items are generated with non-linear regression model (generalized
#' logistic regression model) for DIF detection specified in \code{\link[difNLR]{difNLR}}.
#' In case \code{itemtype = "nominal"}, nominal items are generated with multinomial model specified in
#' \code{\link[difNLR]{ddfMLR}}. For option \code{itemtype = "ordinal"}, ordinal items are generated with
#' adjacent logit model specified in \code{\link[difNLR]{ddfORD}} with argument \code{model = "adjacent"}.
#'
#' The \code{a}, \code{b}, \code{c} and \code{d} are numeric matrices with m rows (where m is number of items)
#' representing parameters of regression models for DIF and DDF detection.
#'
#' For option \code{itemtype = "dich"}, matrices should have two columns. The first column represents
#' parameters of the reference group and the second of the focal group. In case that only one column is
#' provided, parameters are set to be the same for both groups.
#'
#' For options \code{itemtype = "nominal"} and \code{itemtype = "ordinal"}, matrices \code{c} and \code{d}
#' are ignored. Matrices \code{a} and \code{b} contain parameters for distractors. For example, when
#' item with 4 different choices is supposed to be generated, user provide matrices with 6 columns.
#' First 3 columns correspond to distractors parameters for reference group and last three columns
#' for focal group. The number of choices can differ for items. Matrices \code{a} and \code{b}
#' need to consist of as many columns as is the maximum number of distractors. Items with less
#' choices can containt NAs.
#'
#' Single value for \code{mu} means that reference and focal group have underlying distribution with the same mean.
#' Single value for \code{sigma} means that reference and focal group have underlying distribution with the same
#' standard deviation. In case that \code{mu} or \code{sigma} are vectors of length grater than two, only
#' first two values are taken.
#'
#' @usage genNLR(N = 1000, ratio = 1, itemtype = "dich", a, b, c, d, mu = 0, sigma = 1)
#'
#' @return
#' A \code{data.frame} containing \code{N} rows representing respondents and \code{m + 1} columns representing
#' \code{m} items. Last column is group membership variable with coding 0 for reference group and 1 for focal
#' group.
#'
#' @author
#' Adela Hladka (nee Drabinova) \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' hladka@cs.cas.cz \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @references
#' Drabinova, A. & Martinkova P. (2017). Detection of Differential Item Functioning with NonLinear Regression:
#' Non-IRT Approach Accounting for Guessing. Journal of Educational Measurement, 54(4), 498-517,
#' \url{https://doi.org/10.1111/jedm.12158}.
#'
#' @seealso \code{\link[difNLR]{difNLR}}, \code{\link[difNLR]{ddfMLR}}, \code{\link[difNLR]{ddfORD}}
#'
#' @examples
#' \dontrun{
#' # seed
#' set.seed(123)
#' # generating parameters for dichotomous data with DIF, 5 items
#' a <- matrix(runif(10, 0.8, 2), ncol = 2)
#' b <- matrix(runif(10, -2, 2), ncol = 2)
#' c <- matrix(runif(10, 0, 0.25), ncol = 2)
#' d <- matrix(runif(10, 0.8, 1), ncol = 2)
#' # generating dichotomous data set with 300 observations (150 each group)
#' genNLR(N = 300, a = a, b = b, c = c, d = d)
#' # generating dichotomous data set with 300 observations (150 each group)
#' # and different mean and standard deviation for underlying distribution
#' genNLR(N = 300, a = a, b = b, c = c, d = d, mu = c(1, 0), sigma = c(1, 2))
#' # generating dichotomous data set with 300 observations (250 reference group, 50 focal)
#' genNLR(N = 300, ratio = 5, a = a, b = b, c = c, d = d)
#'
#' # generating parameters for nominal data with DDF, 5 items,
#' # each item 3 choices
#' a <- matrix(runif(20, 0.8, 2), ncol = 4)
#' b <- matrix(runif(20, -2, 2), ncol = 4)
#' # generating nominal data set with 300 observations (150 each group)
#' genNLR(N = 300, itemtype = "nominal", a = a, b = b)
#' # generating nominal data set with 300 observations (250 reference group, 50 focal)
#' genNLR(N = 300, itemtype = "nominal", ratio = 5, a = a, b = b)
#'
#' # generating parameters for nominal data with DDF, 5 items,
#' # items 1 and 2 have 2 choices, items 3, 4 and 5 have 3 choices
#' a <- matrix(runif(20, 0.8, 2), ncol = 4)
#' a[1:2, c(2, 4)] <- NA
#' b <- matrix(runif(20, -2, 2), ncol = 4)
#' b[1:2, c(2, 4)] <- NA
#' # generating nominal data set with 300 observations (150 each group)
#' genNLR(N = 300, itemtype = "nominal", a = a, b = b)
#' # generating nominal data set with 300 observations (250 reference group, 50 focal)
#' genNLR(N = 300, itemtype = "nominal", ratio = 5, a = a, b = b)
#' }
#' @export


genNLR <- function(N = 1000, ratio = 1, itemtype = "dich", a, b, c = NULL, d = NULL,
                   mu = 0, sigma = 1) {
  N_R <- round(N / (ratio + 1) * ratio)
  N_F <- round(N / (ratio + 1))
  group <- c(rep(0, N_R), rep(1, N_F))

  if (!(is.numeric(mu))) {
    stop("Invalid value for 'mu'. 'mu' needs to be numeric.")
  }
  if (!(is.numeric(sigma))) {
    stop("Invalid value for 'sigma'. 'sigma' needs to be numeric. ")
  }
  if (length(mu) == 1) {
    mu <- c(mu, mu)
  } else {
    if (length(mu) > 2) {
      warning("Length of 'mu' is greater than 2. Only first two values are used.")
    }
    mu <- mu[1:2]
  }
  if (length(sigma) == 1) {
    sigma <- c(sigma, sigma)
  } else {
    if (length(sigma) > 2) {
      warning("Length of 'sigma' is greater than 2. Only first two values are used.")
    }
    sigma <- sigma[1:2]
  }

  theta <- c(
    rnorm(N_R, mean = mu[1], sd = sigma[1]),
    rnorm(N_F, mean = mu[2], sd = sigma[2])
  )

  if (!(itemtype %in% c("dich", "nominal", "ordinal"))) {
    stop("Invalid value for 'itemtype'. Type of item can be either 'dich', 'nominal', or 'ordinal'.")
  }

  if (itemtype == "dich") {
    if (missing(a)) {
      stop("Missing 'a'. Discrimination parameter need to be provided.")
    }
    if (is.null(dim(a))) {
      a <- cbind(a, a)
    }
    m <- nrow(a)
    if (missing(b)) {
      stop("Missing 'b'. Discrimination parameter need to be provided.")
    }
    if (is.null(dim(b))) {
      b <- cbind(b, b)
    }
    if (nrow(a) != nrow(b)) {
      stop("Invalid dimensions for 'a' and 'b'.")
    }
    if (is.null(c)) {
      c <- matrix(0, ncol = 2, nrow = m)
    }
    if (is.null(d)) {
      d <- matrix(1, ncol = 2, nrow = m)
    }
    if (any(is.na(a)) | any(is.na(b)) | any(is.na(c)) | any(is.na(d))) {
      stop("Missing values are not allowed in parameters for dichotomous items.")
    }

    gNLR <- function(x, g, a, b, c, d) {
      return(c + (d - c) / (1 + exp(-a * (x - b))))
    }

    pR <- sapply(1:m, function(i) {
      do.call(gNLR, list(
        x = theta[1:N_R],
        a = a[i, 1], b = b[i, 1], c = c[i, 1], d = d[i, 1]
      ))
    })
    pF <- sapply(1:m, function(i) {
      do.call(gNLR, list(
        x = theta[(N_R + 1):N],
        a = a[i, 2], b = b[i, 2], c = c[i, 2], d = d[i, 2]
      ))
    })

    p <- rbind(pR, pF)

    answer <- matrix(NA, nrow = N, ncol = m)
    for (j in 1:m) {
      for (i in 1:N) {
        answer[i, j] <- rbinom(1, 1, p[i, j])
      }
    }
  } else {
    if (missing(a)) {
      stop("Missing 'a'. Discrimination parameter need to be provided.")
    }
    if (!((ncol(a) %% 2) == 0)) {
      stop("Invalid dimension for 'a'.")
    }

    if (missing(b)) {
      stop("Missing 'b'. Discrimination parameter need to be provided.")
    }
    if (!((ncol(b) %% 2) == 0)) {
      stop("Invalid dimension for 'b'.")
    }

    if (nrow(a) != nrow(b)) {
      stop("Invalid dimensions for 'a' and 'b'.")
    }

    if (any(is.na(a) != is.na(b))) {
      stop("Missing values need to be the same in 'a' and 'b'.")
    }

    gMLR <- function(x, a, b) {
      return(exp(a * (x - b)))
    }
    m <- nrow(a)
    answer <- matrix(NA, nrow = N, ncol = m)

    if (itemtype == "ordinal") {
      L <- apply(a, 1, function(x) sum(!is.na(x)) / 2)
      atmp <- t(sapply(1:nrow(a), function(i) c(cumsum(a[i, 1:L[i]]), cumsum(a[i, (L[i] + 1):ncol(a)]))))
      ab <- a * b
      btmp <- t(sapply(1:nrow(a), function(i) c(cumsum(ab[i, 1:L[i]]), cumsum(ab[i, (L[i] + 1):ncol(a)]))))

      a <- atmp
      b <- btmp / atmp
    }

    for (i in 1:m) {
      L <- sum(!is.na(a[i, ])) / 2
      atmp <- a[i, ]
      atmp <- atmp[!is.na(atmp)]
      btmp <- b[i, ]
      btmp <- btmp[!is.na(btmp)]
      pR <- sapply(1:L, function(l) {
        do.call(gMLR, list(
          x = theta[1:N_R],
          a = atmp[l], b = btmp[l]
        ))
      })
      pF <- sapply(1:L, function(l) {
        do.call(gMLR, list(
          x = theta[(N_R + 1):N],
          a = atmp[l + L], b = btmp[l + L]
        ))
      })

      pR <- cbind(1, pR)
      pF <- cbind(1, pF)
      sumR <- apply(pR, 1, sum)
      sumF <- apply(pF, 1, sum)
      pR <- pR / sumR
      pF <- pF / sumF
      p <- rbind(pR, pF)

      for (j in 1:N) {
        answer[j, i] <- rbinom(1, size = L, prob = p[j, ])
      }
    }

    if (itemtype == "ordinal") {
      uval <- lapply(1:m, function(i) sort(unique(answer[, i])))
      answer <- as.data.frame(sapply(1:m, function(i) factor(answer[, i], levels = uval[[i]], ordered = T)))
    }
  }

  data <- data.frame(answer, group)
  colnames(data) <- c(paste0("Item", 1:m), "group")
  return(data)
}
