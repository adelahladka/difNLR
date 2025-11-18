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
.gNLR.is <- function(x, g, b0, b1, b2, b3, c, d, cDif, dDif) {
  return((c + cDif * g) + ((d + dDif * g) - (c + cDif * g)) / (1 + exp(-(b0 + b1 * x + b2 * g + b3 * x * g))))
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
.resolve_anchor <- function(anchor, Data) {
  m <- ncol(Data)

  if (is.null(anchor)) {
    ANCHOR <- seq_len(m)
  } else if (is.numeric(anchor)) {
    if (any(anchor < 1 | anchor > m)) {
      stop("Numeric 'anchor' values must be valid column indices within 'Data'.", call. = FALSE)
    }
    ANCHOR <- anchor
  } else if (is.character(anchor)) {
    ANCHOR <- match(anchor, colnames(Data))
    if (any(is.na(ANCHOR))) {
      stop("Some anchor item names not found in 'Data'.", call. = FALSE)
    }
  } else {
    stop("'anchor' must be either NULL, numeric (column indices), or character (column names).", call. = FALSE)
  }

  return(sort(unique(ANCHOR)))
}

# checking input - group
.resolve_group <- function(Data, group, focal.name) {
  # 0. standardize Data into a data.frame
  if (is.vector(Data)) {
    Data <- data.frame(Item1 = Data)
  } else if (is.matrix(Data)) {
    Data <- as.data.frame(Data)
  } else if (!is.data.frame(Data)) {
    stop("'Data' must be a vector, matrix, or data.frame.", call. = FALSE)
  }

  # 1. group is a column index or name
  if (length(group) == 1) {
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
    # 2. group is a vector
  } else {
    if (length(group) != nrow(Data)) {
      stop("'group' must be of length equal to the number of rows in 'Data'.", call. = FALSE)
    }
    # group is a standalone vector
    GROUP <- group
    DATA <- as.data.frame(Data)
  }

  # validate that group is binary
  group_levels <- na.omit(unique(GROUP))
  if (length(group_levels) != 2) {
    stop("'group' must contain exactly two unique non-NA values.", call. = FALSE)
  }
  if (!focal.name %in% group_levels) {
    stop("'focal.name' must be a valid value from 'group'.", call. = FALSE)
  }
  if (group_levels[1] == focal.name) {
    group_levels <- rev(group_levels)
  }
  GROUP <- as.numeric(as.factor(GROUP) == focal.name)
  DATA <- as.data.frame(DATA)

  return(list(GROUP = GROUP, DATA = DATA, group_levels = group_levels))
}

# checking input - match
.resolve_match <- function(match, Data, anchor, key = NULL) {
  m <- ncol(Data)
  # 1. match is predefined character option
  if (is.character(match) && match[1] %in% c("score", "zscore", "restscore", "zrestscore")) {
    match.name <- switch(match[1],
                         "zscore" = "Standardized total score",
                         "score" = "Total score",
                         "restscore" = "Total score without tested item",
                         "zrestscore" = "Standardized total score without tested item")
    if (match[1] %in% c("score", "zscore")) {
      # anchor items with item currently tested
      MATCH <- sapply(1:m, function(item) {
        # item_anchor <- union(anchor, item)
        # DATA <- Data[, item_anchor, drop = FALSE]
        DATA <- Data[, anchor, drop = FALSE]
        if (is.null(key)) {
          rowSums(DATA, na.rm = TRUE)
        } else {
          .score(DATA, key[anchor])
        }

      })
      if (match[1] == "zscore") {
        MATCH[] <- sapply(1:m, function(item) scale(MATCH[, item]))
      }
    } else if (match[1] %in% c("restscore", "zrestscore")) {
      # anchor items without item currently tested
      MATCH <- sapply(1:m, function(item) {
        rest_anchor <- setdiff(anchor, item)
        if (length(rest_anchor) == 0) {
          stop("No items left to compute matching criterion. Try to re-specify anchor or match arguments. ", call. = FALSE)
        } else {
          DATA <- Data[, rest_anchor, drop = FALSE]
          if (is.null(key)) {
            rowSums(DATA, na.rm = TRUE)
          } else {
            .score(DATA, key[anchor])
          }
        }
      })
      if (match[1] == "zrestscore") {
        MATCH[] <- sapply(1:m, function(item) scale(MATCH[, item]))
      }
    }
    # 2. match is user-specified
  } else {
    match.name <- "Matching criterion"
    if (is.numeric(match) && is.null(dim(match))) {
      # 2a. numeric vector
      if (length(match) != nrow(Data)) {
        stop("'match' vector must have the same length as number of rows in 'Data'.", call. = FALSE)
      }
      MATCH <- as.data.frame(replicate(m, match))
    } else if (is.numeric(match) && !is.null(dim(match))) {
      # 2b. numeric matrix
      # when match is a numeric matrix (each column is a matching for one item)
      if (any(dim(match) != dim(Data))) {
        if (nrow(match) == nrow(Data) && ncol(match) == 1) {
          MATCH <- as.data.frame(replicate(m, match))
        } else {
          stop("'match' matrix must have the same dimensions as 'Data'.", call. = FALSE)
        }
      } else {
        MATCH <- match
      }
    } else if (is.data.frame(match)) {
      # 2c. numeric data.frame
      if (!all(sapply(match, is.numeric))) {
        stop("'match' data.frame must contain only numeric columns.", call. = FALSE)
      }
      if (!all(dim(match) == dim(Data))) {
        stop("'match' data.frame must have the same dimensions as 'Data'.", call. = FALSE)
      }
      MATCH <- match
    } else {
      # 3. anything else (invalid)
      stop("'match' must be either 'score', 'zscore', 'restscore', 'zrestscore',
      a numeric vector of length equal to the number of rows in 'Data', or
           a numeric matrix of the same dimension as 'Data'.", call. = FALSE)
    }
  }
  MATCH <- as.data.frame(MATCH)
  colnames(MATCH) <- paste0("MATCH", 1:m)

  return(list(MATCH = MATCH, match.name = match.name))
}

# remove missing values
.resolve_missing <- function(Data, group, match) {
  n <- nrow(Data)

  df <- data.frame(Data, group, match, check.rows = FALSE)
  df <- df[complete.cases(df), ]
  n_df <- nrow(df)

  if (nrow(df) == 0) {
    stop("'Data' contanins no complete cases after removing missing values.", call. = FALSE)
  }
  if (n_df < n) {
    warning(
      sprintf(
        "%d observation%s removed due to missing values.",
        n - n_df, ifelse(n - n_df > 1, "s were", " was")
      ),
      call. = FALSE
    )
  }

  DATA <- df[, !grepl("MATCH", colnames(df)) & !grepl("group", colnames(df)), drop = FALSE]
  GROUP <- df[["group"]]
  MATCH <- df[, grepl("MATCH", colnames(df)), drop = FALSE]

  return(list(DATA = DATA, GROUP = GROUP, MATCH = MATCH))
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
# checking input - character arguments
.check_character <- function(arg, name, values) {
  if (!is.character(arg) | !(arg %in% values)) {
    n_vals <- length(values)
    values <- paste0("'", values, "'")
    vals <- paste0(paste0(values[seq(n_vals - 1)], collapse = ", "), ifelse(n_vals == 2, " or ", ", or "), values[n_vals], ".")
    stop(paste0(sprintf("'%s' must be either ", name), vals), call. = FALSE)
  }
}
# checking input - character arguments, item specific
.resolve_character_is <- function(arg, name, values, m) {
  n_arg <- length(arg)
  if (n_arg == 1) {
    if (!is.character(arg) | !(arg %in% values)) {
      n_vals <- length(values)
      values <- paste0("'", values, "'")
      vals <- paste0(paste0(values[seq(n_vals - 1)], collapse = ", "), ifelse(n_vals == 2, " or ", ", or "), values[n_vals], ".")
      stop(paste0(sprintf("'%s' must be either ", name), vals), call. = FALSE)
    }
    ARG <- rep(arg, m)
  } else {
    if (!is.character(arg) | !(all(arg %in% values))) {
      n_vals <- length(values)
      values <- paste0("'", values, "'")
      vals <- paste0(paste0(values[seq(n_vals - 1)], collapse = ", "), ifelse(n_vals == 2, " or ", ", or "), values[n_vals], ".")
      stop(paste0(sprintf("'%s' must be either ", name), vals), call. = FALSE)
    }
    if (n_arg != m) {
      stop(sprintf("'%s' must be either must be either a single value or have one value per each item.", name), call. = FALSE)
    }
    ARG <- arg
  }
  return(ARG)
}
