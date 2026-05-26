normalise_recorder_label <- function(label_text, fallback = "unknown") {
  candidate <- trimws(as.character(label_text[[1]]))

  if (!nzchar(candidate) || is.na(candidate)) {
    return(fallback)
  }

  candidate <- toupper(candidate)
  candidate <- gsub("[-[:space:]]+", "_", candidate)
  candidate <- gsub("[^A-Z0-9_]", "", candidate)

  if (!nzchar(candidate)) {
    fallback
  } else {
    candidate
  }
}

extract_recorder_label_from_path <- function(path_text, fallback = "unknown") {
  path_text <- normalizePath(as.character(path_text), winslash = "/", mustWork = FALSE)
  recorder_candidates <- regmatches(
    path_text,
    gregexpr("GEL[-_ ][A-Z]+", path_text, perl = TRUE)
  )[[1]]
  recorder_candidates <- recorder_candidates[!is.na(recorder_candidates) & nzchar(recorder_candidates)]

  if (length(recorder_candidates) > 0) {
    return(normalise_recorder_label(recorder_candidates[[1]], fallback = fallback))
  }

  fallback
}

canonical_recording_key <- function(path_text) {
  path_text <- normalizePath(as.character(path_text), winslash = "/", mustWork = FALSE)
  candidate <- basename(as.character(path_text))
  candidate <- sub("_birdnet_species_summary\\.csv$", "", candidate)
  candidate <- sub("_birdnet_predictions\\.csv$", "", candidate)
  candidate <- sub("\\.(wav|flac|mp3|aif|aiff|ogg|m4a|mp4)$", "", candidate, ignore.case = TRUE)
  candidate <- sub("^recording_[0-9]+_", "", candidate)

  recorder_label <- extract_recorder_label_from_path(path_text, fallback = "")

  timestamp_text <- regmatches(candidate, regexpr("[0-9]{8}T[0-9]{6}[+-][0-9]{4}", candidate))
  timestamp_text <- if (length(timestamp_text) == 1 && !is.na(timestamp_text) && nzchar(timestamp_text)) timestamp_text else ""

  coordinate_parts <- regmatches(
    candidate,
    regexec("(-?[0-9]{1,2}\\.[0-9]+)([+-][0-9]{1,3}\\.[0-9]+)", candidate, perl = TRUE)
  )[[1]]
  coordinate_key <- if (length(coordinate_parts) == 3) {
    paste0(coordinate_parts[[2]], coordinate_parts[[3]])
  } else {
    ""
  }

  if (nzchar(timestamp_text) && nzchar(recorder_label)) {
    return(sprintf("%s/%s", recorder_label, timestamp_text))
  }

  if (nzchar(timestamp_text) && nzchar(coordinate_key)) {
    return(sprintf("%s/%s", timestamp_text, coordinate_key))
  }

  if (nzchar(timestamp_text)) {
    return(timestamp_text)
  }

  candidate
}

candidate_recording_keys <- function(path_text) {
  path_text <- normalizePath(as.character(path_text), winslash = "/", mustWork = FALSE)
  candidate <- basename(as.character(path_text))
  candidate <- sub("_birdnet_species_summary\\.csv$", "", candidate)
  candidate <- sub("_birdnet_predictions\\.csv$", "", candidate)
  candidate <- sub("\\.(wav|flac|mp3|aif|aiff|ogg|m4a|mp4)$", "", candidate, ignore.case = TRUE)
  candidate <- sub("^recording_[0-9]+_", "", candidate)

  recorder_label <- extract_recorder_label_from_path(path_text, fallback = "")

  timestamp_text <- regmatches(candidate, regexpr("[0-9]{8}T[0-9]{6}[+-][0-9]{4}", candidate))
  timestamp_text <- if (length(timestamp_text) == 1 && !is.na(timestamp_text) && nzchar(timestamp_text)) timestamp_text else ""

  coordinate_parts <- regmatches(
    candidate,
    regexec("(-?[0-9]{1,2}\\.[0-9]+)([+-][0-9]{1,3}\\.[0-9]+)", candidate, perl = TRUE)
  )[[1]]
  coordinate_key <- if (length(coordinate_parts) == 3) {
    paste0(coordinate_parts[[2]], coordinate_parts[[3]])
  } else {
    ""
  }

  key_candidates <- c(
    if (nzchar(timestamp_text) && nzchar(recorder_label)) sprintf("%s/%s", recorder_label, timestamp_text) else "",
    if (nzchar(candidate)) candidate else ""
  )
  if (!nzchar(recorder_label)) {
    key_candidates <- c(
      key_candidates,
      if (nzchar(timestamp_text) && nzchar(coordinate_key)) sprintf("%s/%s", timestamp_text, coordinate_key) else "",
      if (nzchar(timestamp_text)) timestamp_text else ""
    )
  }
  unique(key_candidates[nzchar(key_candidates)])
}
