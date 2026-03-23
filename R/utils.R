library(data.table)

extract_luna_table <- function(result, table_name, cols = NULL) {
  if (length(result) == 0 || is.null(result[[1]]) || is.null(result[[1]][[table_name]])) {
    dt <- data.table()
  } else {
    dt <- as.data.table(result[[1]][[table_name]])
  }

  if (!is.null(cols)) {
    missing <- setdiff(cols, names(dt))
    if (length(missing)) {
      dt[, (missing) := NA]
    }
    dt <- dt[, cols, with = FALSE]
  }

  dt
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

extract_first_available_table <- function(result, table_names, cols = NULL) {
  result_names <- names(result[[1]] %||% list())
  for (table_name in table_names) {
    dt <- extract_luna_table(result, table_name, cols = cols)
    if (nrow(dt) > 0 || table_name %in% result_names) {
      return(dt)
    }
  }

  data.table()
}

merge_dt_list <- function(dt_list, by) {
  non_empty <- Filter(function(x) nrow(x) > 0, dt_list)
  if (!length(non_empty)) {
    return(data.table())
  }

  Reduce(function(x, y) merge(x, y, by = by, all = TRUE), non_empty)
}

ensure_dt_cols <- function(x, cols) {
  dt <- as.data.table(x)
  missing <- setdiff(cols, names(dt))
  if (length(missing)) {
    dt[, (missing) := NA]
  }
  dt[, cols, with = FALSE]
}

coalesce_zero <- function(x) {
  y <- copy(x)
  y[is.na(y)] <- 0
  y
}

collect_tables <- function(x) {
  if (is.null(x)) {
    return(list())
  }

  if (is.data.table(x) || is.data.frame(x)) {
    return(list(as.data.table(x)))
  }

  if (is.list(x)) {
    return(unlist(lapply(x, collect_tables), recursive = FALSE))
  }

  list()
}

safe_mean_value <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_median_value <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  median(x, na.rm = TRUE)
}

safe_max_value <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}
