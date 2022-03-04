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

.gNLR <- function(x, g, a, b, c, d, aDif, bDif, cDif, dDif) {
  return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))))
}

.gNLR_group <- function(x, a, b, c, d) {
  return(c + (d - c) / (1 + exp(-(a * (x - b)))))
}

.delta.gNLR <- deriv(y ~ (c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))),
  namevec = c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif"),
  function(x, g, a, b, c, d, aDif, bDif, cDif, dDif) {}
)

.plot.theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}

.plot.theme.legend <- function() {
  ggplot2::theme(
    legend.box.just = "top",
    legend.justification = c("left", "top"),
    legend.position = c(0.02, 0.98),
    legend.box = "horizontal",
    legend.box.margin = ggplot2::margin(3, 3, 3, 3),
    legend.key = ggplot2::element_rect(fill = "white", colour = NA)
  )
}

.checkInterval <- function(x, vec) {
  ifelse(x >= vec[1] & x <= vec[2], x,
    ifelse(x > vec[2], vec[2], vec[1])
  )
}
