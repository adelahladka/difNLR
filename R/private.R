# calculates total test score from unscored data and key of correct answers
.score <- function(Data, key) {
  if (!is.matrix(key)) {
    key <- matrix(key)
  }
  colname <- colnames(Data)
  X <- matrix(0L, nrow(Data), ncol(Data))
  colnames(X) <- colname
  for (i in 1L:ncol(X)) {
    if (all(is.na(key[i, ]))) {
      next
    }
    X[, i] <- Data[, i] %in% key[i, ] + 0L
  }
  rowSums(X)
}

# generalized logistic regression function
.gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif) {
  return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
}
.gNLR.is <- function(x, g, b0, b1, b2, b3, c, d, cDif, dDif) {
  return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(b0 + b1 * x + b2 * g + b3 * x * g))))
}

# generalized logistic regression function without group variable
.gNLR_group <- function(x, a, b, c, d) {
  return(c + (d - c) / (1 + exp(-(a * (x - b)))))
}

# delta method for generalized logistic regression function
.delta.gNLR.irt <- deriv(y ~ (c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))),
  namevec = c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif"),
  function(x, g, a, b, c, d, aDif, bDif, cDif, dDif) {}
)
.delta.gNLR.is <- deriv(y ~ (c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(b0 + b1 * x + b2 * g + b3 * x * g))),
  namevec = c("b0", "b1", "b2", "b3", "c", "d", "cDif", "dDif"),
  function(x, g, b0, b1, b2, b3, c, d, cDif, dDif) {}
)

# ggplot2 theme for plot
.plot.theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}

# ggplot2 theme for plot with legend
.plot.theme.legend <- function() {
  ggplot2::theme(
    legend.box.just = "top",
    legend.justification = c("left", "top"),
    legend.position = c(0.02, 0.98),
    legend.box = "horizontal",
    legend.box.margin = ggplot2::margin(3, 3, 3, 3),
    legend.key = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key.width = ggplot2::unit(0.9, "cm"),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  )
}

# check whether value is in specified vector
.checkInterval <- function(x, vec) {
  ifelse(x >= vec[1] & x <= vec[2], x,
    ifelse(x > vec[2], vec[2], vec[1])
  )
}

# paste function which removes empty strings
.MYpaste <- function(..., sep = "", collapse = NULL, recycle0 = FALSE) {
  x <- lapply(Filter(\(x)any(nzchar(x)), list(...)), \(x)x[nzchar(x)])
  do.call(paste, c(x, sep = sep, collapse = collapse, recycle0 = recycle0))
}
