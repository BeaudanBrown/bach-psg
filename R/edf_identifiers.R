infer_filter_profile <- function(edf_path) {
  edf_dir <- dirname(edf_path)
  if (grepl("/filtered/", edf_dir, perl = TRUE)) {
    profile <- basename(edf_dir)
    return(ifelse(profile == "filtered", "base", profile))
  }
  "base"
}

infer_bach_id <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  sub("_filtered$", "", base_name)
}

get_filtered_edf_path <- function(edf_path, filter_profile_name = "base") {
  filtered_dir <- file.path(dirname(edf_path), "filtered", filter_profile_name)
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  file.path(filtered_dir, paste0(base_name, "_filtered", ".edf"))
}
