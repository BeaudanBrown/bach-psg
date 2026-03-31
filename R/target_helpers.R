collect_data_tables <- function(x) {
  bind_rows <- collect_tables(x)
  if (!length(bind_rows)) {
    return(data.table())
  }

  rbindlist(bind_rows, fill = TRUE)
}

extract_psd_b_ch <- function(psd_result) {
  if (is.null(psd_result$psd_b_ch[[1]])) {
    return(NULL)
  }
  out <- copy(as.data.table(psd_result$psd_b_ch[[1]]))
  out$stage <- if (is.null(psd_result$sleep_stage)) {
    NA_character_
  } else {
    psd_result$sleep_stage[[1]]
  }
  out$filter_profile <- psd_result$filter_profile[[1]]
  out$bach_id <- psd_result$bach_id[[1]]
  out
}

collect_psd_b_ch <- function(psd_results) {
  if (is.data.table(psd_results) || is.data.frame(psd_results)) {
    psd_tables <- lapply(seq_len(nrow(psd_results)), function(i) {
      extract_psd_b_ch(psd_results[i])
    })
  } else {
    psd_tables <- lapply(
      collect_tables(psd_results),
      extract_psd_b_ch
    )
  }
  psd_tables <- Filter(Negate(is.null), psd_tables)
  if (!length(psd_tables)) {
    return(data.table())
  }

  rbindlist(psd_tables, fill = TRUE)
}

build_qc_csv_rows <- function(qc_results) {
  if (!nrow(qc_results)) {
    return(data.table())
  }

  qc_rows <- lapply(seq_len(nrow(qc_results)), function(i) {
    if (is.null(qc_results$qc[[i]])) {
      return(NULL)
    }
    dt <- as.data.table(qc_results$qc[[i]])
    dt[, bach_id := qc_results$bach_id[i]]
    dt[, filter_profile := qc_results$filter_profile[i]]
    dt
  })
  qc_rows <- Filter(Negate(is.null), qc_rows)
  if (!length(qc_rows)) {
    return(data.table())
  }

  rbindlist(qc_rows, fill = TRUE)
}

build_output_dir <- function(data_dir, filter_profile_name) {
  path <- file.path(data_dir, filter_profile_name)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  path
}
