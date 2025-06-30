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
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.box = "horizontal",
    legend.box.margin = ggplot2::margin(3, 3, 3, 3),
    legend.key = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key.width = ggplot2::unit(0.8, "cm"),
    legend.key.spacing = ggplot2::unit(0.1, "cm"),
    legend.spacing.x = ggplot2::unit(0.1, "cm"),
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
  x <- lapply(Filter(function(x) {
    any(nzchar(x))
  }, list(...)), function(x) {
    x[nzchar(x)]
  })
  do.call(paste, c(x, sep = sep, collapse = collapse, recycle0 = recycle0))
}

# checking inputs - anchor
.resolve_anchor <- function(anchor, DATA) {
  m <- ncol(DATA)

  if (is.null(anchor)) {
    ANCHOR <- seq_len(m)
  } else if (is.numeric(anchor)) {
    if (any(anchor < 1 | anchor > m)) {
      stop("Numeric 'anchor' values must be valid column indices within 'Data'.", call. = FALSE)
    }
    ANCHOR <- anchor
  } else if (is.character(anchor)) {
    ANCHOR <- match(anchor, colnames(DATA))
    if (any(is.na(ANCHOR))) {
      stop("Some anchor item names not found in 'Data'.", call. = FALSE)
    }
  } else {
    stop("'anchor' must be either NULL, numeric (column indices), or character (column names).", call. = FALSE)
  }

  return(sort(unique(ANCHOR)))
}

# checking input - group
.resolve_group <- function(Data, group) {
  if (length(group) == 1) {
    # group is a column index or name
    if (is.numeric(group)) {
      if (group < 1 || group > ncol(Data)) {
        stop("'group' index is out of bounds.", call. = FALSE)
      }
      GROUP <- Data[[group]]
      DATA <- Data[, -group, drop = FALSE]
    } else if (is.character(group)) {
      col_idx <- match(group, colnames(Data))
      if (is.na(col_idx)) {
        stop(sprintf("Column '%s' not found in 'Data'.", group), call. = FALSE)
      }
      GROUP <- Data[[col_idx]]
      DATA <- Data[, -col_idx, drop = FALSE]
    } else {
      stop("'group' must be a column name or index, or a vector of group values.", call. = FALSE)
    }
  } else {
    # group is a standalone vector
    GROUP <- group
    DATA <- as.data.frame(Data)
  }

  # Validate that group is binary
  group_levels <- na.omit(unique(GROUP))
  if (length(group_levels) != 2) {
    stop("'group' must contain exactly two unique non-NA values.", call. = FALSE)
  }

  # Check that group length matches Data rows
  if (length(GROUP) != nrow(DATA)) {
    stop("'group' must be of length equal to the number of rows in 'Data'.", call. = FALSE)
  }

  return(list(GROUP = GROUP, DATA = DATA))
}

# checking input - items
.resolve_items <- function(item, colnames_data) {
  m <- length(colnames_data)

  if (is.character(item)) {
    if (any(item == "all")) {
      return(seq_len(m))
    } else if (!all(item %in% colnames_data)) {
      stop("'item' must be either 'all' or valid column names from 'Data'.", call. = FALSE)
    } else {
      return(which(colnames_data %in% item))
    }
  }

  if (is.numeric(item)) {
    if (!all(item %in% seq_len(m))) {
      stop("'item' index is out of bounds.", call. = FALSE)
    }
    return(item)
  }

  stop("'item' must be either 'all', a character vector of column names, or numeric indices.", call. = FALSE)
}

# checking input for plots - check for convergence issues and select only converged items
.resolve_non_converged_items_plot <- function(item, conv_fail_items) {
  non_converged <- intersect(conv_fail_items, item)

  if (length(non_converged) > 0) {
    remaining <- setdiff(item, conv_fail_items)

    if (length(remaining) == 0) {
      stop("None of the selected items converged. Characteristic curves cannot be plotted.", call. = FALSE)
    } else {
      warning(
        paste0(
          "The following items did not converge and will be excluded from plotting: ",
          paste(non_converged, collapse = ", "), "."
        ),
        call. = FALSE
      )
      return(remaining)
    }
  }

  return(item)
}

# checking input - check for convergence issues and select only converged items
.resolve_non_converged_items <- function(item, conv_fail_items) {
  non_converged <- intersect(conv_fail_items, item)

  if (length(non_converged) > 0) {
    remaining <- setdiff(item, conv_fail_items)

    if (length(remaining) == 0) {
      stop("None of the selected items converged. ", call. = FALSE)
    } else {
      warning(
        paste0("Item ", paste(non_converged, collapse = ", "), " does not converge. NA produced."),
        call. = FALSE
      )
      return(remaining)
    }
  } else {
    return(item)
  }
}

# checking input - logical arguments
.check_logical <- function(arg, name) {
  if (!is.logical(arg) || length(arg) != 1) {
    stop(sprintf("'%s' must be a single logical value (TRUE or FALSE).", name), call. = FALSE)
  }
}
# checking input - numeric arguments
.check_numeric <- function(arg, name, low, upp) {
  if (!is.numeric(arg) || length(arg) != 1 || arg < low || arg > upp) {
    bounds <- if (upp == Inf) {
      sprintf("greater than %s", low)
    } else {
      sprintf("between %s and %s.", low, upp)
    }
    stop(paste0(sprintf("'%s' must be a single numeric value ", name), bounds), call. = FALSE)
  }
}
