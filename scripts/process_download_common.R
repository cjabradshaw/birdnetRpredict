get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_args <- grep("^--file=", command_args, value = TRUE)

  if (length(file_args) > 0) {
    candidate_path <- sub("^--file=", "", file_args[1])

    if (nzchar(candidate_path) && candidate_path != "-" && file.exists(candidate_path)) {
      return(dirname(normalizePath(candidate_path)))
    }
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile) && nzchar(frame$ofile) && file.exists(frame$ofile)) {
      return(dirname(normalizePath(frame$ofile)))
    }
  }

  normalizePath(".")
}

if (!exists("script_dir", inherits = FALSE)) {
  script_dir <- get_script_dir()
}

source(file.path(script_dir, "birdnet_helpers.R"))

required_option_names <- c(
  "source_mode",
  "archive_file",
  "ecosounds_workbench_url",
  "ecosounds_project_id",
  "ecosounds_recorder_id",
  "ecosounds_recorder_name",
  "ecosounds_download_method",
  "ecosounds_powershell_script",
  "ecosounds_refresh_powershell_script",
  "ecosounds_listing_page_size",
  "ecosounds_auth_token",
  "ecosounds_user_name",
  "ecosounds_password",
  "species_csv",
  "pipeline_timezone",
  "fallback_latitude",
  "fallback_longitude",
  "prediction_min_confidence",
  "summary_confidence_threshold",
  "use_arrow",
  "stage_heartbeat_seconds",
  "stage_timeout_seconds"
)
missing_option_names <- required_option_names[!vapply(required_option_names, exists, logical(1), inherits = TRUE)]

if (length(missing_option_names) > 0) {
  stop(
    sprintf(
      "Missing download options: %s. Source downloading_user_options.R before process_download_common.R.",
      paste(missing_option_names, collapse = ", ")
    )
  )
}

if (!requireNamespace("processx", quietly = TRUE)) {
  stop("The processx package is required for live progress monitoring. Install it with install.packages('processx').")
}

if (!requireNamespace("callr", quietly = TRUE)) {
  stop("The callr package is required for monitored BirdNET execution. Install it with install.packages('callr').")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required for EcoSounds downloads. Install it with install.packages('jsonlite').")
}


species_csv <- normalizePath(path.expand(species_csv), mustWork = TRUE)

if (!source_mode %in% c("archive", "ecosounds")) {
  stop("source_mode must be either 'archive' or 'ecosounds'.")
}

archive_file <- if (identical(source_mode, "archive")) {
  normalizePath(archive_file, mustWork = TRUE)
} else {
  archive_file
}

source_name <- if (identical(source_mode, "archive")) {
  sub("\\.tar\\.zst$", "", basename(archive_file), ignore.case = TRUE)
} else {
  sprintf("ecosounds_project_%s", as.integer(ecosounds_project_id))
}
source_description <- if (identical(source_mode, "archive")) {
  archive_file
} else {
  sprintf("EcoSounds project %s via %s", as.integer(ecosounds_project_id), ecosounds_workbench_url)
}
output_root <- file.path(
  script_dir,
  "..",
  "out",
  paste0(source_name, "_birdnet_output")
)
extract_root <- file.path(tempdir(), source_name, "audio_work")
manifest_csv <- file.path(output_root, paste0(source_name, "_processing_manifest.csv"))
file_results_txt <- file.path(output_root, paste0(source_name, "_file_results.txt"))
summary_of_summaries_txt <- file.path(output_root, paste0(source_name, "_summary_of_summaries.txt"))
stage_heartbeat_seconds <- as.numeric(stage_heartbeat_seconds)
stage_timeout_seconds <- as.numeric(stage_timeout_seconds)

pipeline_settings <- list(
  species_csv = species_csv,
  timezone = pipeline_timezone,
  fallback_latitude = as.numeric(fallback_latitude),
  fallback_longitude = as.numeric(fallback_longitude),
  prediction_min_confidence = as.numeric(prediction_min_confidence),
  summary_confidence_threshold = as.numeric(summary_confidence_threshold),
  use_arrow = isTRUE(use_arrow)
)

timestamp_text <- function(x = Sys.time()) {
  format(x, "%Y-%m-%d %H:%M:%S")
}

format_duration <- function(seconds) {
  if (is.null(seconds) || is.na(seconds) || !is.finite(seconds)) {
    return("n/a")
  }

  total_seconds <- max(0L, as.integer(round(seconds)))
  hours <- total_seconds %/% 3600L
  minutes <- (total_seconds %% 3600L) %/% 60L
  secs <- total_seconds %% 60L

  sprintf("%02d:%02d:%02d", hours, minutes, secs)
}

emit_console <- function(message) {
  cat(sprintf("[%s] %s\n", timestamp_text(), message))
  flush.console()
}

safe_file_component <- function(text_value) {
  text_value <- gsub("[^-_A-Za-z0-9]", "", text_value)

  if (!nzchar(text_value)) {
    return("unknown")
  }

  text_value
}

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

canonical_recording_key <- function(path_text) {
  path_text <- normalizePath(as.character(path_text), winslash = "/", mustWork = FALSE)
  candidate <- basename(as.character(path_text))
  candidate <- sub("_birdnet_species_summary\\.csv$", "", candidate)
  candidate <- sub("_birdnet_predictions\\.csv$", "", candidate)
  candidate <- sub("\\.(wav|flac|mp3|aif|aiff|ogg|m4a|mp4)$", "", candidate, ignore.case = TRUE)
  candidate <- sub("^recording_[0-9]+_", "", candidate)

  recorder_candidates <- regmatches(
    path_text,
    gregexpr("GEL[-_ ][A-Z]+", path_text, perl = TRUE)
  )[[1]]
  recorder_candidates <- recorder_candidates[!is.na(recorder_candidates) & nzchar(recorder_candidates)]
  recorder_label <- if (length(recorder_candidates) > 0) {
    normalise_recorder_label(recorder_candidates[[1]])
  } else {
    ""
  }

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

  recorder_candidates <- regmatches(
    path_text,
    gregexpr("GEL[-_ ][A-Z]+", path_text, perl = TRUE)
  )[[1]]
  recorder_candidates <- recorder_candidates[!is.na(recorder_candidates) & nzchar(recorder_candidates)]
  recorder_label <- if (length(recorder_candidates) > 0) {
    normalise_recorder_label(recorder_candidates[[1]])
  } else {
    ""
  }

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
    if (nzchar(timestamp_text) && nzchar(coordinate_key)) sprintf("%s/%s", timestamp_text, coordinate_key) else "",
    if (nzchar(timestamp_text)) timestamp_text else "",
    if (nzchar(candidate)) candidate else ""
  )
  unique(key_candidates[nzchar(key_candidates)])
}

ecosounds_archive_member <- function(recording) {
  canonical_name <- basename(as.character(recording[["canonical_file_name"]]))
  recorder_label <- normalise_recorder_label(
    recording[["sites.name"]],
    fallback = sprintf("site_%s", as.integer(recording[["site_id"]]))
  )
  file.path(
    recorder_label,
    canonical_name
  )
}

normalise_optional_integer <- function(value, setting_name) {
  if (length(value) == 0 || is.null(value) || all(is.na(value))) {
    return(NA_integer_)
  }

  candidate <- trimws(as.character(value[[1]]))

  if (!nzchar(candidate)) {
    return(NA_integer_)
  }

  integer_value <- suppressWarnings(as.integer(candidate))

  if (is.na(integer_value)) {
    stop(sprintf("%s must be left empty/NA or set to a whole-number EcoSounds recorder ID.", setting_name))
  }

  integer_value
}

normalise_optional_text <- function(value) {
  if (length(value) == 0 || is.null(value) || all(is.na(value))) {
    return("")
  }

  trimws(as.character(value[[1]]))
}

normalise_optional_path <- function(value) {
  path_text <- normalise_optional_text(value)

  if (!nzchar(path_text)) {
    return("")
  }

  path.expand(path_text)
}

normalise_positive_integer <- function(value, setting_name) {
  integer_value <- suppressWarnings(as.integer(value[[1]]))

  if (length(integer_value) == 0 || is.na(integer_value) || integer_value < 1L) {
    stop(sprintf("%s must be set to a positive whole number.", setting_name))
  }

  integer_value
}

ecosounds_filter_object <- function(project_id,
                                    recorder_id = NA_integer_,
                                    recorder_name = "",
                                    page_size = 500L,
                                    recording_id = NA_integer_) {
  recorder_id <- normalise_optional_integer(recorder_id, "ecosounds_recorder_id")
  recorder_name <- normalise_optional_text(recorder_name)
  page_size <- normalise_positive_integer(page_size, "ecosounds_listing_page_size")
  recording_id <- normalise_optional_integer(recording_id, "recording_id")

  if (!is.na(recorder_id) && nzchar(recorder_name)) {
    stop("Set only one of ecosounds_recorder_id or ecosounds_recorder_name.")
  }

  and_filters <- list(
    "projects.id" = list(eq = as.integer(project_id)),
    "status" = list(eq = "ready")
  )

  if (!is.na(recorder_id)) {
    and_filters[["sites.id"]] <- list(eq = recorder_id)
  }

  if (nzchar(recorder_name)) {
    and_filters[["sites.name"]] <- list(eq = recorder_name)
  }

  if (!is.na(recording_id)) {
    and_filters[["id"]] <- list(eq = recording_id)
  }

  list(
    filter = list(and = and_filters),
    projection = list(
      only = c("id", "recorded_date", "sites.name", "site_id", "canonical_file_name")
    ),
    sorting = list(order_by = "recorded_date", direction = "desc"),
    paging = list(items = page_size)
  )
}

filter_ecosounds_recordings <- function(recordings,
                                        recorder_id = NA_integer_,
                                        recorder_name = "") {
  recorder_id <- normalise_optional_integer(recorder_id, "ecosounds_recorder_id")
  recorder_name <- normalise_optional_text(recorder_name)

  if (!is.na(recorder_id) && nzchar(recorder_name)) {
    stop("Set only one of ecosounds_recorder_id or ecosounds_recorder_name.")
  }

  if (nrow(recordings) == 0) {
    return(recordings)
  }

  if (!is.na(recorder_id)) {
    if (!"site_id" %in% names(recordings)) {
      stop("EcoSounds listing did not include site_id, so ecosounds_recorder_id cannot be applied.")
    }

    filtered <- recordings[recordings$site_id == recorder_id, , drop = FALSE]

    if (nrow(filtered) == 0) {
      stop(sprintf("No EcoSounds recordings matched ecosounds_recorder_id = %s in project %s.", recorder_id, as.integer(ecosounds_project_id)))
    }

    return(filtered)
  }

  if (nzchar(recorder_name)) {
    name_column <- "sites.name"

    if (!name_column %in% names(recordings)) {
      stop("EcoSounds listing did not include sites.name, so ecosounds_recorder_name cannot be applied.")
    }

    filtered <- recordings[trimws(as.character(recordings[[name_column]])) == recorder_name, , drop = FALSE]

    if (nrow(filtered) == 0) {
      stop(sprintf("No EcoSounds recordings matched ecosounds_recorder_name = '%s' in project %s.", recorder_name, as.integer(ecosounds_project_id)))
    }

    return(filtered)
  }

  recordings
}

run_curl_request <- function(url,
                             method = "GET",
                             headers = character(),
                             body_json = NULL,
                             output_file = NULL) {
  response_file <- if (is.null(output_file)) tempfile(pattern = "curl-response-", fileext = ".tmp") else output_file
  args <- c("-sS", "-L", "-X", method)

  for (header in headers) {
    args <- c(args, "-H", header)
  }

  if (!is.null(body_json)) {
    args <- c(args, "--data-binary", body_json)
  }

  args <- c(args, "-o", response_file, "-w", "%{http_code}", url)
  response <- processx::run("curl", args = args, error_on_status = FALSE)

  if (!identical(response$status, 0L)) {
    stop(
      paste(
        c(
          sprintf("curl failed for %s", url),
          trimws(response$stderr)
        ),
        collapse = "\n"
      )
    )
  }

  list(
    status_code = suppressWarnings(as.integer(trimws(response$stdout))),
    response_file = response_file
  )
}

ecosounds_get_auth_token <- function(workbench_url, auth_token = "", user_name = "", password = "") {
  if (nzchar(auth_token)) {
    return(auth_token)
  }

  if (!nzchar(user_name) || !nzchar(password)) {
    stop(
      paste(
        "EcoSounds access requires credentials in the user-defined settings or environment variables.",
        "Set ecosounds_auth_token (or ECOSOUNDS_AUTH_TOKEN),",
        "or set both ecosounds_user_name + ecosounds_password",
        "(or ECOSOUNDS_USERNAME + ECOSOUNDS_PASSWORD)."
      )
    )
  }

  login_payload <- if (grepl("@", user_name, fixed = TRUE)) {
    list(email = user_name, password = password)
  } else {
    list(login = user_name, password = password)
  }

  login_response <- run_curl_request(
    url = sprintf("%s/security", sub("/$", "", workbench_url)),
    method = "POST",
    headers = c("Content-Type: application/json", "Accept: application/json"),
    body_json = jsonlite::toJSON(login_payload, auto_unbox = TRUE)
  )

  if (!identical(login_response$status_code, 200L)) {
    stop(sprintf("EcoSounds login failed with HTTP status %s.", login_response$status_code))
  }

  login_content <- jsonlite::fromJSON(login_response$response_file)
  token <- login_content$data$auth_token

  if (!nzchar(token)) {
    stop("EcoSounds login succeeded but no auth_token was returned.")
  }

  token
}

list_ecosounds_recordings <- function(workbench_url,
                                      auth_token,
                                      project_id,
                                      recorder_id,
                                      recorder_name,
                                      page_size,
                                      manifest,
                                      start_time) {
  page <- 1L
  max_page <- Inf
  records <- list()
  base_url <- sub("/$", "", workbench_url)
  json_headers <- c(
    sprintf("Authorization: Token token=\"%s\"", auth_token),
    "Content-Type: application/json",
    "Accept: application/json"
  )
  recorder_id <- normalise_optional_integer(recorder_id, "ecosounds_recorder_id")
  recorder_name <- normalise_optional_text(recorder_name)
  page_size <- normalise_positive_integer(page_size, "ecosounds_listing_page_size")
  list_endpoint <- if (!is.na(recorder_id)) {
    sprintf(
      "%s/projects/%s/sites/%s/audio_recordings/filter",
      base_url,
      as.integer(project_id),
      recorder_id
    )
  } else {
    sprintf("%s/audio_recordings/filter", base_url)
  }
  filter_json <- jsonlite::toJSON(
    ecosounds_filter_object(
      project_id = project_id,
      recorder_id = recorder_id,
      recorder_name = recorder_name,
      page_size = page_size
    ),
    auto_unbox = TRUE
  )

  while (page <= max_page) {
    page_response <- run_curl_request(
      url = sprintf("%s?page=%d", list_endpoint, page),
      method = "POST",
      headers = json_headers,
      body_json = filter_json
    )

    if (!identical(page_response$status_code, 200L)) {
      stop(sprintf("EcoSounds recording listing failed on page %d with HTTP status %s.", page, page_response$status_code))
    }

    page_content <- jsonlite::fromJSON(page_response$response_file)
    max_page <- page_content$meta$paging$max_page
    page_records <- page_content$data
    records[[length(records) + 1L]] <- if (is.null(page_records)) {
      data.frame()
    } else {
      as.data.frame(page_records, stringsAsFactors = FALSE)
    }

    recordings_found <- sum(vapply(records, nrow, integer(1)))
    detail_text <- sprintf(
      "listing EcoSounds project %s%s%s | page=%d/%s | recordings_found_so_far=%d",
      as.integer(project_id),
      if (!is.na(recorder_id)) sprintf(" | recorder_id=%s", recorder_id) else "",
      if (nzchar(recorder_name)) sprintf(" | recorder_name=%s", recorder_name) else "",
      page,
      max_page,
      recordings_found
    )
    emit_console(sprintf("[ecosounds list] %s", detail_text))
    update_live_progress(
      manifest = manifest,
      current_member = "",
      current_phase = "listing_archive",
      current_detail = detail_text,
      start_time = start_time,
      total_files = recordings_found
    )

    page <- page + 1L
  }

  non_empty_records <- Filter(function(x) !is.null(x) && nrow(x) > 0, records)
  if (length(non_empty_records) == 0) {
    return(data.frame(
      id = integer(),
      recorded_date = character(),
      site_id = integer(),
      canonical_file_name = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, non_empty_records)
}

download_ecosounds_downloader_script <- function(workbench_url,
                                                 auth_token,
                                                 filter_json,
                                                 output_path = "") {
  script_path <- if (nzchar(normalise_optional_path(output_path))) {
    normalise_optional_path(output_path)
  } else {
    tempfile(pattern = "ecosounds-downloader-", fileext = ".ps1")
  }

  dir.create(dirname(script_path), recursive = TRUE, showWarnings = FALSE)
  script_response <- run_curl_request(
    url = sprintf("%s/audio_recordings/downloader", sub("/$", "", workbench_url)),
    method = "POST",
    headers = c(
      sprintf("Authorization: Token token=\"%s\"", auth_token),
      "Content-Type: application/json",
      "Accept: text/plain"
    ),
    body_json = filter_json,
    output_file = script_path
  )

  if (!identical(script_response$status_code, 200L)) {
    if (file.exists(script_path)) {
      unlink(script_path, force = TRUE)
    }
    stop(sprintf("Failed to generate EcoSounds downloader script (HTTP %s).", script_response$status_code))
  }

  Sys.chmod(script_path, mode = "700")
  script_path
}

fetch_ecosounds_recording_details <- function(workbench_url, auth_token, recording_id) {
  details_response <- run_curl_request(
    url = sprintf("%s/audio_recordings/%s", sub("/$", "", workbench_url), as.integer(recording_id)),
    method = "GET",
    headers = c(
      sprintf("Authorization: Token token=\"%s\"", auth_token),
      "Accept: application/json"
    )
  )

  if (!identical(details_response$status_code, 200L)) {
    stop(sprintf("Failed to fetch EcoSounds recording metadata for %s (HTTP %s).", recording_id, details_response$status_code))
  }

  jsonlite::fromJSON(details_response$response_file)
}

format_ecosounds_offset <- function(value) {
  formatted <- formatC(as.numeric(value), format = "f", digits = 4)
  formatted <- sub("0+$", "", formatted)
  sub("\\.$", "", formatted)
}

download_ecosounds_recording_via_media_segments <- function(recording,
                                                            workbench_url,
                                                            auth_token,
                                                            download_root,
                                                            recording_details,
                                                            segment_seconds = 300) {
  duration_seconds <- suppressWarnings(as.numeric(recording_details$data$duration_seconds[[1]]))

  if (is.na(duration_seconds) || duration_seconds <= 0) {
    stop(sprintf("EcoSounds recording %s does not have a valid duration for segmented media download.", recording[["id"]]))
  }

  relative_item <- ecosounds_archive_member(recording)
  local_path <- file.path(download_root, relative_item)
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  segment_dir <- file.path(download_root, "media_segments")
  dir.create(segment_dir, recursive = TRUE, showWarnings = FALSE)

  segment_starts <- seq(0, duration_seconds, by = segment_seconds)
  if (tail(segment_starts, 1) >= duration_seconds) {
    segment_starts <- head(segment_starts, -1)
  }
  if (length(segment_starts) == 0) {
    segment_starts <- 0
  }

  is_valid_wav_file <- function(path_text) {
    if (!file.exists(path_text) || file.info(path_text)$size <= 1024) {
      return(FALSE)
    }

    connection <- file(path_text, open = "rb")
    on.exit(close(connection), add = TRUE)
    header_bytes <- readBin(connection, what = "raw", n = 12)
    length(header_bytes) >= 12 &&
      identical(rawToChar(header_bytes[1:4]), "RIFF") &&
      identical(rawToChar(header_bytes[9:12]), "WAVE")
  }

  segment_paths <- vapply(seq_along(segment_starts), function(index) {
    start_offset <- segment_starts[[index]]
    end_offset <- min(start_offset + segment_seconds, duration_seconds)
    segment_path <- file.path(segment_dir, sprintf("segment_%04d.wav", index))
    request_variants <- list(
      c(start_offset, end_offset, 0),
      c(start_offset, if (end_offset < duration_seconds) max(start_offset + 0.1, end_offset - 0.1) else end_offset, 0),
      c(if (start_offset > 0) start_offset + 0.1 else start_offset, if (end_offset < duration_seconds) max(start_offset + 0.1, end_offset - 0.1) else end_offset, 0),
      c(start_offset, end_offset, 1)
    )
    variant_keys <- vapply(request_variants, function(bounds) sprintf("%.4f|%.4f|%s", bounds[[1]], bounds[[2]], bounds[[3]]), character(1))
    request_variants <- request_variants[!duplicated(variant_keys)]
    segment_ok <- FALSE
    last_status_code <- NA_integer_

    for (bounds in request_variants) {
      for (attempt_index in seq_len(3L)) {
        if (file.exists(segment_path)) {
          unlink(segment_path, force = TRUE)
        }
        segment_url <- sprintf(
          "%s/audio_recordings/%s/media.wav?start_offset=%s&end_offset=%s&channel=%s",
          sub("/$", "", workbench_url),
          as.integer(recording[["id"]]),
          format_ecosounds_offset(bounds[[1]]),
          format_ecosounds_offset(bounds[[2]]),
          as.integer(bounds[[3]])
        )
        segment_response <- run_curl_request(
          url = segment_url,
          method = "GET",
          headers = sprintf("Authorization: Token token=\"%s\"", auth_token),
          output_file = segment_path
        )
        last_status_code <- segment_response$status_code

        if (identical(last_status_code, 200L) && is_valid_wav_file(segment_path)) {
          segment_ok <- TRUE
          break
        }

        if (file.exists(segment_path)) {
          unlink(segment_path, force = TRUE)
        }

        if (attempt_index < 3L) {
          Sys.sleep(1)
        }
      }

      if (segment_ok) {
        break
      }
    }

    if (!segment_ok) {
      if (file.exists(segment_path)) {
        unlink(segment_path, force = TRUE)
      }
      stop(
        sprintf(
          "Segmented EcoSounds media download failed for recording %s at %.3f-%.3f seconds (HTTP %s).",
          recording[["id"]],
          start_offset,
          end_offset,
          last_status_code
        )
      )
    }

    segment_path
  }, character(1))

  concat_manifest <- file.path(segment_dir, "segments.txt")
  writeLines(
    sprintf("file '%s'", gsub("'", "'\\''", normalizePath(segment_paths, winslash = "/", mustWork = TRUE), fixed = TRUE)),
    concat_manifest
  )

  ffmpeg_status <- system2(
    "ffmpeg",
    args = c("-hide_banner", "-loglevel", "error", "-y", "-f", "concat", "-safe", "0", "-i", concat_manifest, "-c", "copy", local_path)
  )

  if (!identical(ffmpeg_status, 0L) || !file.exists(local_path) || file.info(local_path)$size <= 0) {
    stop(sprintf("Failed to concatenate segmented EcoSounds media download for recording %s.", recording[["id"]]))
  }

  local_path
}

download_ecosounds_recording <- function(recording,
                                         workbench_url,
                                         auth_token,
                                         download_root,
                                         download_method = "api_then_powershell",
                                         powershell_script = "",
                                         project_id = NA_integer_,
                                         refresh_powershell_script = TRUE) {
  relative_item <- ecosounds_archive_member(recording)
  local_path <- file.path(download_root, relative_item)
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  auth_header <- sprintf("Authorization: Token token=\"%s\"", auth_token)
  base_url <- sub("/$", "", workbench_url)
  requested_original_url <- sprintf("%s/audio_recordings/%s/original", base_url, recording[["id"]])
  original_status_code <- NA_integer_
  fallback_status_code <- NA_integer_
  recording_details <- fetch_ecosounds_recording_details(workbench_url, auth_token, recording[["id"]])
  can_download_original <- isTRUE(recording_details$meta$capabilities$original_download$can[[1]])
  powershell_script <- normalise_optional_path(powershell_script)
  project_id <- normalise_optional_integer(project_id, "ecosounds_project_id")
  refresh_powershell_script <- isTRUE(refresh_powershell_script)

  run_api_download <- function() {
    if (!isTRUE(can_download_original)) {
      local_path <<- download_ecosounds_recording_via_media_segments(
        recording = recording,
        workbench_url = workbench_url,
        auth_token = auth_token,
        download_root = download_root,
        recording_details = recording_details
      )
      return(TRUE)
    }

    download_response <- run_curl_request(
      url = requested_original_url,
      method = "GET",
      headers = auth_header,
      output_file = local_path
    )
    original_status_code <<- download_response$status_code

    if (identical(original_status_code, 403L)) {
      local_path <<- download_ecosounds_recording_via_media_segments(
        recording = recording,
        workbench_url = workbench_url,
        auth_token = auth_token,
        download_root = download_root,
        recording_details = recording_details
      )
      fallback_status_code <<- 200L
      return(TRUE)
    }

    if (!identical(download_response$status_code, 200L) && file.exists(local_path)) {
      unlink(local_path, force = TRUE)
    }

    identical(download_response$status_code, 200L)
  }

  run_powershell_download <- function() {
    powershell_binary <- Sys.which("pwsh")

    if (!nzchar(powershell_binary)) {
      stop("PowerShell download method requested, but pwsh is not available on PATH.")
    }

    powershell_target <- file.path(download_root, "powershell_download")
    dir.create(powershell_target, recursive = TRUE, showWarnings = FALSE)
    filter_json <- jsonlite::toJSON(
      ecosounds_filter_object(
        project_id = project_id,
        recorder_id = normalise_optional_integer(recording[["site_id"]], "recording_site_id"),
        recorder_name = "",
        page_size = 1L,
        recording_id = normalise_optional_integer(recording[["id"]], "recording_id")
      ),
      auto_unbox = TRUE
    )
    script_path <- if (refresh_powershell_script || !nzchar(powershell_script) || !file.exists(powershell_script)) {
      download_ecosounds_downloader_script(
        workbench_url = workbench_url,
        auth_token = auth_token,
        filter_json = filter_json,
        output_path = if (refresh_powershell_script) "" else powershell_script
      )
    } else {
      Sys.chmod(powershell_script, mode = "700")
      powershell_script
    }
    powershell_args <- c(
      "-NoProfile",
      "-File",
      script_path,
      "-target",
      powershell_target,
      "-auth_token",
      auth_token,
      "-filter",
      filter_json,
      "-workbench_url",
      workbench_url,
      "-clobber"
    )
    powershell_result <- processx::run(
      command = powershell_binary,
      args = powershell_args,
      error_on_status = FALSE
    )

    if (!identical(powershell_result$status, 0L)) {
      stop(
        paste(
          c(
            sprintf("PowerShell EcoSounds download failed for recording %s.", recording[["id"]]),
            trimws(powershell_result$stdout),
            trimws(powershell_result$stderr)
          ),
          collapse = "\n"
        )
      )
    }

    target_name <- basename(as.character(recording[["canonical_file_name"]]))
    matching_downloads <- list.files(
      powershell_target,
      recursive = TRUE,
      full.names = TRUE
    )
    matching_downloads <- matching_downloads[basename(matching_downloads) == target_name]

    if (length(matching_downloads) == 0) {
      stop(sprintf("PowerShell EcoSounds download reported success but did not produce %s.", target_name))
    }

    local_path <<- matching_downloads[[1]]
    TRUE
  }

  download_success <- switch(
    download_method,
    api = run_api_download(),
    powershell = run_powershell_download(),
    api_then_powershell = {
      api_success <- run_api_download()
      if (isTRUE(api_success)) TRUE else run_powershell_download()
    },
    stop("ecosounds_download_method must be one of 'api', 'powershell', or 'api_then_powershell'.")
  )

  if (!isTRUE(download_success) || !file.exists(local_path)) {
    if (file.exists(local_path)) {
      unlink(local_path, force = TRUE)
    }
    stop(
      sprintf(
        paste(
          "EcoSounds download failed for recording %s.",
          "Original-file request returned HTTP %s%s."
        ),
        recording[["id"]],
        if (is.na(original_status_code)) "n/a" else original_status_code,
        if (!is.na(fallback_status_code)) sprintf(" and fallback media.wav request returned HTTP %s", fallback_status_code) else ""
      )
    )
  }

  list(
    archive_member = relative_item,
    local_audio_path = local_path,
    local_workspace = download_root
  )
}

cleanup_local_workspace <- function(workspace_path) {
  if (!is.null(workspace_path) && nzchar(workspace_path) && dir.exists(workspace_path)) {
    unlink(workspace_path, recursive = TRUE, force = TRUE)
  }
}

progress_label <- function(index, total_files) {
  if (total_files <= 0) {
    return(sprintf("%d/%d (0.0%%)", index, total_files))
  }

  sprintf("%d/%d (%.1f%%)", index, total_files, 100 * index / total_files)
}

estimate_remaining_seconds <- function(start_time, completed_files, total_files) {
  if (completed_files <= 0) {
    return(NA_real_)
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  average_seconds_per_file <- elapsed / completed_files
  remaining_files <- max(0L, total_files - completed_files)

  average_seconds_per_file * remaining_files
}

safe_read_summary_csv <- function(summary_csv) {
  if (!file.exists(summary_csv) || file.info(summary_csv)$size == 0) {
    return(empty_summary_table())
  }

  summary_df <- tryCatch(
    read.csv(summary_csv, stringsAsFactors = FALSE),
    error = function(error) empty_summary_table()
  )

  required_columns <- c(
    "date_time",
    "scientific_name",
    "common_name",
    "confidence",
    "cumulative_number_of_new_species_detected",
    "total_number_of_species_identified"
  )

  missing_columns <- setdiff(required_columns, names(summary_df))
  for (column_name in missing_columns) {
    if (nrow(summary_df) == 0) {
      summary_df[[column_name]] <- empty_summary_table()[[column_name]]
    } else if (column_name %in% c("date_time", "scientific_name", "common_name")) {
      summary_df[[column_name]] <- rep("", nrow(summary_df))
    } else {
      summary_df[[column_name]] <- rep(NA_real_, nrow(summary_df))
    }
  }

  summary_df[, required_columns, drop = FALSE]
}

collapse_species <- function(summary_table, limit = 12L) {
  if (nrow(summary_table) == 0) {
    return("none")
  }

  species_labels <- unique(
    paste(summary_table$common_name, sprintf("(%s)", summary_table$scientific_name))
  )
  species_labels <- species_labels[seq_len(min(length(species_labels), limit))]
  collapse_text <- paste(species_labels, collapse = "; ")

  if (length(unique(paste(summary_table$common_name, summary_table$scientific_name))) > limit) {
    paste0(collapse_text, "; ...")
  } else {
    collapse_text
  }
}

stage_progress <- function(stage) {
  switch(
    stage,
    listing_archive = 0,
    starting_file = 0,
    skipped_existing = 100,
    downloading_audio = 10,
    downloaded_audio = 20,
    extracting_flac = 10,
    extracted_flac = 20,
    converting_wav = 30,
    converted_wav = 40,
    building_range_filter = 55,
    running_birdnet = 80,
    writing_empty_summary = 95,
    writing_summary = 95,
    cleaning_temp = 98,
    completed_file = 100,
    error = 100,
    complete = 100,
    0
  )
}

sanitize_stream_lines <- function(lines) {
  if (length(lines) == 0) {
    return(character())
  }

  cleaned <- gsub("\r", "", lines, fixed = TRUE)
  cleaned <- gsub("[[:cntrl:]]", "", cleaned)
  cleaned[nzchar(cleaned)]
}

split_stream_text <- function(text) {
  if (length(text) == 0 || is.null(text) || !nzchar(text)) {
    return(character())
  }

  pieces <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  sanitize_stream_lines(pieces)
}

filter_birdnet_progress_lines <- function(lines) {
  if (length(lines) == 0) {
    return(lines)
  }

  excluded_pattern <- paste0(
    "^WARNING: Attempting to use a delegate that only supports static-sized tensors ",
    "with a graph that has dynamic-sized tensors \\(tensor#187 is a dynamic-sized tensor\\)\\.?$"
  )

  lines[!grepl(excluded_pattern, trimws(lines))]
}

consume_stream_text <- function(buffer, text) {
  if (length(text) == 0 || is.null(text) || !nzchar(text)) {
    return(list(lines = character(), buffer = buffer))
  }

  combined <- paste0(buffer, text)
  pieces <- unlist(strsplit(combined, "\n", fixed = TRUE), use.names = FALSE)

  if (endsWith(combined, "\n")) {
    complete <- pieces
    buffer <- ""
  } else {
    if (length(pieces) == 0) {
      complete <- character()
      buffer <- combined
    } else {
      complete <- pieces[-length(pieces)]
      buffer <- pieces[length(pieces)]
    }
  }

  list(
    lines = sanitize_stream_lines(complete),
    buffer = buffer
  )
}

make_ffmpeg_progress_parser <- function(input_name, output_name) {
  latest_out_time <- NULL
  latest_progress <- NULL

  function(lines) {
    if (length(lines) > 0) {
      for (line in lines) {
        if (grepl("^out_time=", line)) {
          latest_out_time <<- sub("^out_time=", "", line)
        } else if (grepl("^progress=", line)) {
          latest_progress <<- sub("^progress=", "", line)
        }
      }
    }

    detail <- sprintf("converting %s -> %s", input_name, output_name)

    if (!is.null(latest_out_time) && nzchar(latest_out_time)) {
      detail <- paste0(detail, " | encoded=", latest_out_time)
    }

    if (!is.null(latest_progress) && nzchar(latest_progress)) {
      detail <- paste0(detail, " | ffmpeg=", latest_progress)
    }

    detail
  }
}

run_monitored_command <- function(command,
                                  args,
                                  phase_prefix,
                                  archive_member,
                                  stage,
                                  start_time,
                                  total_files,
                                  manifest,
                                  detail_text,
                                  heartbeat_seconds = 5,
                                  timeout_seconds = Inf,
                                  detail_parser = NULL) {
  process <- processx::process$new(
    command = command,
    args = args,
    stdout = "|",
    stderr = "|",
    cleanup_tree = TRUE
  )

  start_stage_at <- Sys.time()
  last_report_at <- as.POSIXct(NA)
  collected_output <- character()
  current_detail <- detail_text

  on.exit({
    if (process$is_alive()) {
      process$kill_tree()
    }
  }, add = TRUE)

  repeat {
    process$poll_io(1000)

    stdout_lines <- process$read_output_lines()
    stderr_lines <- process$read_error_lines()

    if (length(stdout_lines) > 0 || length(stderr_lines) > 0) {
      collected_output <- c(collected_output, stdout_lines, stderr_lines)
      if (!is.null(detail_parser)) {
        current_detail <- detail_parser(c(stdout_lines, stderr_lines))
      }
    }

    now <- Sys.time()
    stage_elapsed_seconds <- as.numeric(difftime(now, start_stage_at, units = "secs"))

    if (is.na(last_report_at) ||
        as.numeric(difftime(now, last_report_at, units = "secs")) >= heartbeat_seconds) {
      emit_console(
        sprintf(
          "%s [file %.1f%%] %s | stage_elapsed=%s",
          phase_prefix,
          stage_progress(stage),
          current_detail,
          format_duration(stage_elapsed_seconds)
        )
      )
      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = stage,
        current_detail = paste0(current_detail, " | stage_elapsed=", format_duration(stage_elapsed_seconds)),
        start_time = start_time,
        total_files = total_files
      )
      last_report_at <- now
    }

    if (!process$is_alive()) {
      break
    }

    if (is.finite(timeout_seconds) && stage_elapsed_seconds > timeout_seconds) {
      process$kill_tree()
      stop(
        sprintf(
          "%s timed out after %s for %s",
          stage,
          format_duration(stage_elapsed_seconds),
          archive_member
        )
      )
    }
  }

  collected_output <- c(collected_output, process$read_output_lines(), process$read_error_lines())

  if (!identical(process$get_exit_status(), 0L)) {
    stop(
      paste(
        c(
          sprintf("%s failed for %s", command, archive_member),
          utils::tail(collected_output, 20)
        ),
        collapse = "\n"
      )
    )
  }

  invisible(collected_output)
}

run_monitored_birdnet_analysis <- function(script_dir,
                                           pipeline_settings,
                                           wav_path,
                                           output_dir,
                                           output_stem,
                                           phase_prefix,
                                           archive_member,
                                           start_time,
                                           total_files,
                                           manifest,
                                           heartbeat_seconds = 5,
                                           timeout_seconds = Inf) {
  result_rds <- tempfile(pattern = "birdnet-result-", fileext = ".rds")

  bg <- callr::r_bg(
    func = function(script_dir,
                    pipeline_settings,
                    wav_path,
                    output_dir,
                    output_stem,
                    result_rds) {
      suppressPackageStartupMessages(source(file.path(script_dir, "birdnet_helpers.R")))

      pipeline <- do.call(initialize_birdnet_pipeline, pipeline_settings)
      result <- process_audio_file(
        pipeline = pipeline,
        audio_file = wav_path,
        output_dir = output_dir,
        output_stem = output_stem,
        allow_empty_summary = TRUE
      )

      saveRDS(result, result_rds)
      invisible(NULL)
    },
    args = list(
      script_dir = script_dir,
      pipeline_settings = pipeline_settings,
      wav_path = wav_path,
      output_dir = output_dir,
      output_stem = output_stem,
      result_rds = result_rds
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )

  start_stage_at <- Sys.time()
  last_report_at <- as.POSIXct(NA)
  output_lines <- character()

  on.exit({
    if (bg$is_alive()) {
      bg$kill()
    }
    if (file.exists(result_rds)) {
      unlink(result_rds)
    }
  }, add = TRUE)

  repeat {
    bg$poll_io(1000)

    stdout_lines <- bg$read_output_lines()
    stderr_lines <- bg$read_error_lines()
    if (length(stdout_lines) > 0 || length(stderr_lines) > 0) {
      output_lines <- c(output_lines, filter_birdnet_progress_lines(c(stdout_lines, stderr_lines)))
    }

    now <- Sys.time()
    stage_elapsed_seconds <- as.numeric(difftime(now, start_stage_at, units = "secs"))
    recent_output <- trimws(paste(utils::tail(output_lines[nzchar(output_lines)], 2), collapse = " | "))
    current_detail <- sprintf("analysing %s", basename(wav_path))
    if (nzchar(recent_output)) {
      current_detail <- paste0(current_detail, " | last_output=", recent_output)
    }

    if (is.na(last_report_at) ||
        as.numeric(difftime(now, last_report_at, units = "secs")) >= heartbeat_seconds) {
      emit_console(
        sprintf(
          "%s [file %.1f%%] %s | stage_elapsed=%s",
          phase_prefix,
          stage_progress("running_birdnet"),
          current_detail,
          format_duration(stage_elapsed_seconds)
        )
      )
      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = "running_birdnet",
        current_detail = paste0(current_detail, " | stage_elapsed=", format_duration(stage_elapsed_seconds)),
        start_time = start_time,
        total_files = total_files
      )
      last_report_at <- now
    }

    if (!bg$is_alive()) {
      break
    }

    if (is.finite(timeout_seconds) && stage_elapsed_seconds > timeout_seconds) {
      bg$kill()
      stop(
        sprintf(
          "running_birdnet timed out after %s for %s",
          format_duration(stage_elapsed_seconds),
          archive_member
        )
      )
    }
  }

  stdout_lines <- bg$read_output_lines()
  stderr_lines <- bg$read_error_lines()
  output_lines <- c(output_lines, filter_birdnet_progress_lines(c(stdout_lines, stderr_lines)))

  status <- bg$get_exit_status()
  if (!identical(status, 0L)) {
    stop(
      paste(
        c(
          sprintf("BirdNET analysis failed for %s", archive_member),
          utils::tail(output_lines, 20)
        ),
        collapse = "\n"
      )
    )
  }

  if (!file.exists(result_rds)) {
    stop("BirdNET analysis completed without producing a result file for ", archive_member)
  }

  readRDS(result_rds)
}

list_archive_flac_members <- function(archive_path,
                                      manifest,
                                      start_time,
                                      heartbeat_seconds = 5,
                                      timeout_seconds = Inf) {
  process <- processx::process$new(
    command = "tar",
    args = c("--zstd", "-tf", archive_path),
    stdout = NULL,
    stderr = NULL,
    cleanup_tree = TRUE,
    pty = TRUE
  )

  start_stage_at <- Sys.time()
  last_report_at <- as.POSIXct(NA)
  listing_lines <- character()
  listing_buffer <- ""

  on.exit({
    if (process$is_alive()) {
      process$kill_tree()
    }
  }, add = TRUE)

  repeat {
    process$poll_io(1000)

    output_text <- process$read_output()
    parsed_output <- consume_stream_text(listing_buffer, output_text)
    output_lines <- parsed_output$lines
    listing_buffer <- parsed_output$buffer
    if (length(output_lines) > 0) {
      listing_lines <- c(listing_lines, output_lines)
    }

    now <- Sys.time()
    stage_elapsed_seconds <- as.numeric(difftime(now, start_stage_at, units = "secs"))
    member_count <- length(listing_lines)
    flac_count <- sum(grepl("\\.flac$", listing_lines, ignore.case = TRUE))
    latest_member <- if (member_count > 0) tail(listing_lines, 1) else "none"
    detail_text <- sprintf(
      "scanning archive members | members_seen=%d | flac_found_so_far=%d | latest_member=%s",
      member_count,
      flac_count,
      latest_member
    )

    if (is.na(last_report_at) ||
        as.numeric(difftime(now, last_report_at, units = "secs")) >= heartbeat_seconds) {
      emit_console(
        sprintf(
          "[archive scan] %s | stage_elapsed=%s",
          detail_text,
          format_duration(stage_elapsed_seconds)
        )
      )
      update_live_progress(
        manifest = manifest,
        current_member = "",
        current_phase = "listing_archive",
        current_detail = paste0(detail_text, " | stage_elapsed=", format_duration(stage_elapsed_seconds)),
        start_time = start_time,
        total_files = 0
      )
      last_report_at <- now
    }

    if (!process$is_alive()) {
      break
    }

    if (is.finite(timeout_seconds) && stage_elapsed_seconds > timeout_seconds) {
      process$kill_tree()
      stop(
        sprintf(
          "listing_archive timed out after %s",
          format_duration(stage_elapsed_seconds)
        )
      )
    }
  }

  parsed_output <- consume_stream_text(listing_buffer, process$read_output())
  listing_lines <- c(listing_lines, parsed_output$lines)
  if (nzchar(parsed_output$buffer)) {
    listing_lines <- c(listing_lines, sanitize_stream_lines(parsed_output$buffer))
  }

  if (!identical(process$get_exit_status(), 0L)) {
    stop(
      paste(
        c(
          "tar archive listing failed",
          utils::tail(listing_lines, 20)
        ),
        collapse = "\n"
      )
    )
  }

  listing_lines[grepl("\\.flac$", listing_lines, ignore.case = TRUE)]
}

extract_archive_member <- function(archive_path, archive_member, target_dir) {
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  tar_status <- system2(
    "tar",
    args = c("--zstd", "-xf", archive_path, "-C", target_dir, archive_member)
  )

  if (!identical(tar_status, 0L)) {
    stop("tar extraction failed for ", archive_member)
  }

  file.path(target_dir, archive_member)
}

convert_flac_to_wav <- function(flac_path, wav_path) {
  dir.create(dirname(wav_path), recursive = TRUE, showWarnings = FALSE)
  ffmpeg_status <- system2(
    "ffmpeg",
    args = c("-hide_banner", "-loglevel", "error", "-y", "-i", flac_path, wav_path)
  )

  if (!identical(ffmpeg_status, 0L)) {
    stop("ffmpeg conversion failed for ", flac_path)
  }
}

output_paths_for_member <- function(output_dir, archive_member) {
  relative_stem <- tools::file_path_sans_ext(archive_member)

  list(
    predictions_csv = file.path(output_dir, paste0(relative_stem, "_birdnet_predictions.csv")),
    summary_csv = file.path(output_dir, paste0(relative_stem, "_birdnet_species_summary.csv"))
  )
}

build_existing_summary_index <- function(out_root) {
  summary_csv_files <- list.files(
    out_root,
    pattern = "_birdnet_species_summary\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  summary_csv_files <- summary_csv_files[!grepl("/analysis/", summary_csv_files)]
  summary_csv_files <- summary_csv_files[file.exists(summary_csv_files)]
  predictions_csv_files <- sub(
    "_birdnet_species_summary\\.csv$",
    "_birdnet_predictions.csv",
    summary_csv_files
  )
  paired_rows <- file.exists(predictions_csv_files)
  summary_csv_files <- summary_csv_files[paired_rows]
  predictions_csv_files <- predictions_csv_files[paired_rows]

  if (length(summary_csv_files) == 0) {
    return(data.frame(
      recording_key = character(),
      summary_csv = character(),
      predictions_csv = character(),
      stringsAsFactors = FALSE
    ))
  }

  summary_index <- do.call(
    rbind,
    lapply(seq_along(summary_csv_files), function(index) {
      summary_csv <- summary_csv_files[[index]]
      predictions_csv <- predictions_csv_files[[index]]
      recording_keys <- unique(c(
        candidate_recording_keys(summary_csv),
        candidate_recording_keys(predictions_csv)
      ))
      data.frame(
        recording_key = recording_keys,
        summary_csv = summary_csv,
        predictions_csv = predictions_csv,
        stringsAsFactors = FALSE
      )
    })
  )
  summary_index <- summary_index[!duplicated(summary_index$recording_key), , drop = FALSE]
  summary_index
}

find_existing_output_paths <- function(archive_member, output_paths, summary_index) {
  if (file.exists(output_paths$summary_csv) && file.exists(output_paths$predictions_csv)) {
    return(output_paths)
  }

  candidate_keys <- unique(c(
    candidate_recording_keys(archive_member),
    candidate_recording_keys(output_paths$summary_csv),
    candidate_recording_keys(output_paths$predictions_csv)
  ))
  matching_rows <- summary_index[summary_index$recording_key %in% candidate_keys, , drop = FALSE]

  if (nrow(matching_rows) == 0) {
    return(NULL)
  }

  list(
    predictions_csv = matching_rows$predictions_csv[[1]],
    summary_csv = matching_rows$summary_csv[[1]]
  )
}

add_summary_to_index <- function(summary_index, archive_member, output_paths) {
  if (is.null(output_paths$summary_csv) || !file.exists(output_paths$summary_csv) ||
      is.null(output_paths$predictions_csv) || !file.exists(output_paths$predictions_csv)) {
    return(summary_index)
  }

  recording_keys <- unique(c(
    candidate_recording_keys(archive_member),
    candidate_recording_keys(output_paths$summary_csv),
    candidate_recording_keys(output_paths$predictions_csv)
  ))
  new_keys <- setdiff(recording_keys, summary_index$recording_key)

  if (length(new_keys) == 0) {
    return(summary_index)
  }

  rbind(
    summary_index,
    data.frame(
      recording_key = new_keys,
      summary_csv = rep(output_paths$summary_csv, length(new_keys)),
      predictions_csv = rep(output_paths$predictions_csv, length(new_keys)),
      stringsAsFactors = FALSE
    )
  )
}

write_file_results_txt <- function(manifest, output_path) {
  lines <- c(
    "BirdNET processing results by file",
    paste("Updated:", timestamp_text()),
    ""
  )

  if (nrow(manifest) == 0) {
    lines <- c(lines, "No files processed yet.")
    writeLines(lines, output_path)
    return(invisible(NULL))
  }

  for (row_index in seq_len(nrow(manifest))) {
    row <- manifest[row_index, , drop = FALSE]

    lines <- c(
      lines,
      sprintf("[%d] %s", row_index, row$archive_member),
      sprintf("  status: %s", row$status),
      sprintf("  started_at: %s", row$started_at),
      sprintf("  finished_at: %s", row$finished_at),
      sprintf("  processing_time: %s", row$processing_time_hms),
      sprintf("  recording_date: %s", row$recording_date),
      sprintf("  latitude: %s", row$recording_latitude),
      sprintf("  longitude: %s", row$recording_longitude),
      sprintf("  summary_rows: %s", row$summary_rows),
      sprintf("  total_species_identified: %s", row$total_species_identified),
      sprintf("  species_detected: %s", row$species_detected),
      sprintf("  predictions_csv: %s", row$predictions_csv),
      sprintf("  summary_csv: %s", row$summary_csv),
      sprintf("  error_message: %s", row$error_message),
      ""
    )
  }

  writeLines(lines, output_path)
}

write_summary_of_summaries_txt <- function(manifest,
                                           output_path,
                                           source_description,
                                           current_member,
                                           current_phase,
                                           current_detail,
                                           current_file_progress,
                                           start_time,
                                           total_files) {
  completed_files <- nrow(manifest)
  ok_files <- sum(manifest$status == "ok")
  skipped_files <- sum(manifest$status == "skipped_existing")
  empty_files <- sum(manifest$status %in% c("no_summary_detections", "no_usable_detections"))
  error_files <- sum(manifest$status == "error")
  total_summary_rows <- sum(manifest$summary_rows, na.rm = TRUE)
  total_species_hits <- sum(manifest$total_species_identified, na.rm = TRUE)

  cumulative_species <- character()
  if (nrow(manifest) > 0) {
    for (summary_csv in manifest$summary_csv[file.exists(manifest$summary_csv)]) {
      summary_df <- safe_read_summary_csv(summary_csv)
      cumulative_species <- unique(c(cumulative_species, summary_df$scientific_name))
    }
  }
  cumulative_species <- stats::na.omit(cumulative_species)
  cumulative_species <- cumulative_species[nzchar(cumulative_species)]

  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  remaining_seconds <- estimate_remaining_seconds(start_time, completed_files, total_files)
  eta_text <- if (is.na(remaining_seconds)) {
    "n/a"
  } else {
    timestamp_text(Sys.time() + remaining_seconds)
  }

  lines <- c(
    "BirdNET processing summary of summaries",
    paste("Source:", source_description),
    paste("Updated:", timestamp_text()),
    paste("Current file:", if (nzchar(current_member)) current_member else "none"),
    paste("Current phase:", if (nzchar(current_phase)) current_phase else "idle"),
    paste("Current detail:", if (nzchar(current_detail)) current_detail else "none"),
    sprintf("Current file stage progress: %.1f%%", current_file_progress),
    "",
    sprintf(
      "Progress: %d/%d files complete (%.1f%%)",
      completed_files,
      total_files,
      if (total_files > 0) 100 * completed_files / total_files else 0
    ),
    sprintf("Elapsed time: %s", format_duration(elapsed_seconds)),
    sprintf("Estimated time remaining: %s", format_duration(remaining_seconds)),
    sprintf("Estimated completion time: %s", eta_text),
    "",
    sprintf("Successful files: %d", ok_files),
    sprintf("Skipped existing files: %d", skipped_files),
    sprintf("Files with no summary detections: %d", empty_files),
    sprintf("Errored files: %d", error_files),
    sprintf("Total summary rows across files: %d", total_summary_rows),
    sprintf("Sum of per-file species counts: %d", total_species_hits),
    sprintf("Cumulative unique species across all summaries: %d", length(cumulative_species)),
    ""
  )

  if (length(cumulative_species) > 0) {
    lines <- c(
      lines,
      "Cumulative species list:",
      paste(sort(cumulative_species), collapse = ", "),
      ""
    )
  }

  writeLines(lines, output_path)
}

update_live_progress <- function(manifest,
                                 current_member,
                                 current_phase,
                                 current_detail,
                                 start_time,
                                 total_files) {
  write_summary_of_summaries_txt(
    manifest = manifest,
    output_path = summary_of_summaries_txt,
    source_description = source_description,
    current_member = current_member,
    current_phase = current_phase,
    current_detail = current_detail,
    current_file_progress = stage_progress(current_phase),
    start_time = start_time,
    total_files = total_files
  )
}

parse_archive_stream_event <- function(line) {
  fields <- strsplit(line, "\t", fixed = TRUE)[[1]]
  event <- fields[1]

  if (identical(event, "SCAN") && length(fields) >= 4) {
    return(list(
      event = event,
      members_seen = as.integer(fields[2]),
      flac_found_so_far = as.integer(fields[3]),
      latest_member = fields[4]
    ))
  }

  if (identical(event, "FILE") && length(fields) >= 5) {
    return(list(
      event = event,
      members_seen = as.integer(fields[2]),
      flac_found_so_far = as.integer(fields[3]),
      archive_member = fields[4],
      flac_path = fields[5]
    ))
  }

  if (identical(event, "COMPLETE") && length(fields) >= 3) {
    return(list(
      event = event,
      members_seen = as.integer(fields[2]),
      flac_found_so_far = as.integer(fields[3])
    ))
  }

  if (identical(event, "ERROR")) {
    return(list(
      event = event,
      error_message = paste(fields[-1], collapse = "\t")
    ))
  }

  list(event = "RAW", raw = line)
}

append_skipped_existing_manifest <- function(manifest,
                                             archive_member,
                                             output_paths,
                                             file_started_at,
                                             total_files,
                                             start_time) {
  summary_df <- safe_read_summary_csv(output_paths$summary_csv)
  file_finished_at <- Sys.time()
  processing_seconds <- as.numeric(difftime(file_finished_at, file_started_at, units = "secs"))

  manifest <- rbind(
    manifest,
    data.frame(
      archive_member = archive_member,
      status = "skipped_existing",
      started_at = timestamp_text(file_started_at),
      finished_at = timestamp_text(file_finished_at),
      processing_seconds = processing_seconds,
      processing_time_hms = format_duration(processing_seconds),
      recording_date = if (nrow(summary_df) > 0) substr(summary_df$date_time[1], 1, 10) else "",
      recording_latitude = NA_real_,
      recording_longitude = NA_real_,
      predictions_csv = output_paths$predictions_csv,
      summary_csv = output_paths$summary_csv,
      summary_rows = nrow(summary_df),
      total_species_identified = length(unique(summary_df$scientific_name)),
      species_detected = collapse_species(summary_df),
      error_message = "",
      stringsAsFactors = FALSE
    )
  )

  write.csv(manifest, manifest_csv, row.names = FALSE)
  write_file_results_txt(manifest, file_results_txt)
  existing_summary_index <<- add_summary_to_index(existing_summary_index, archive_member, output_paths)
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = "skipped_existing",
    current_detail = sprintf("using existing summary for %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )

  manifest
}

process_local_audio_item <- function(archive_member,
                                     input_audio_path,
                                     file_index,
                                     total_files,
                                     manifest,
                                     start_time,
                                     ready_phase,
                                     ready_detail,
                                     cleanup_input = TRUE) {
  output_paths <- output_paths_for_member(output_root, archive_member)
  file_started_at <- Sys.time()
  phase_prefix <- sprintf("[%s]", progress_label(file_index, total_files))

  emit_console(sprintf("%s [file %.1f%%] starting %s", phase_prefix, stage_progress("starting_file"), archive_member))
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = "starting_file",
    current_detail = sprintf("ready for processing: %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )

  existing_output_paths <- find_existing_output_paths(
    archive_member = archive_member,
    output_paths = output_paths,
    summary_index = existing_summary_index
  )

  if (!is.null(existing_output_paths)) {
    manifest <- append_skipped_existing_manifest(
      manifest = manifest,
      archive_member = archive_member,
      output_paths = existing_output_paths,
      file_started_at = file_started_at,
      total_files = total_files,
      start_time = start_time
    )
    emit_console(sprintf("%s Skipping existing results for %s using %s", phase_prefix, archive_member, existing_output_paths$summary_csv))
    if (cleanup_input && file.exists(input_audio_path)) {
      unlink(input_audio_path, force = TRUE)
    }
    return(manifest)
  }

  wav_path <- input_audio_path
  result <- tryCatch(
    {
      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = ready_phase,
        current_detail = ready_detail,
        start_time = start_time,
        total_files = total_files
      )

      input_extension <- tolower(tools::file_ext(input_audio_path))

      if (!identical(input_extension, "wav")) {
        wav_path <- file.path(
          dirname(input_audio_path),
          paste0(tools::file_path_sans_ext(basename(input_audio_path)), ".wav")
        )

        run_monitored_command(
          command = "ffmpeg",
          args = c(
            "-hide_banner",
            "-nostats",
            "-progress",
            "pipe:1",
            "-y",
            "-i",
            input_audio_path,
            wav_path
          ),
          phase_prefix = phase_prefix,
          archive_member = archive_member,
          stage = "converting_wav",
          start_time = start_time,
          total_files = total_files,
          manifest = manifest,
          detail_text = sprintf("converting %s -> %s", basename(input_audio_path), basename(wav_path)),
          heartbeat_seconds = stage_heartbeat_seconds,
          timeout_seconds = stage_timeout_seconds,
          detail_parser = make_ffmpeg_progress_parser(basename(input_audio_path), basename(wav_path))
        )

        if (!file.exists(wav_path)) {
          stop("ffmpeg completed without producing expected file: ", wav_path)
        }
      }

      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = "converted_wav",
        current_detail = sprintf("converted .wav ready for analysis: %s", basename(wav_path)),
        start_time = start_time,
        total_files = total_files
      )

      processed <- run_monitored_birdnet_analysis(
        script_dir = script_dir,
        pipeline_settings = pipeline_settings,
        wav_path = wav_path,
        output_dir = dirname(output_paths$summary_csv),
        output_stem = tools::file_path_sans_ext(basename(archive_member)),
        phase_prefix = phase_prefix,
        archive_member = archive_member,
        start_time = start_time,
        total_files = total_files,
        manifest = manifest,
        heartbeat_seconds = stage_heartbeat_seconds,
        timeout_seconds = stage_timeout_seconds
      )

      processed$error_message <- ""
      processed
    },
    error = function(error) {
      list(
        status = "error",
        predictions_csv = output_paths$predictions_csv,
        summary_csv = output_paths$summary_csv,
        summary_rows = NA_integer_,
        total_species_identified = NA_integer_,
        recording_date = "",
        recording_latitude = NA_real_,
        recording_longitude = NA_real_,
        summary_table = empty_summary_table(),
        error_message = conditionMessage(error)
      )
    }
  )

  emit_console(sprintf("%s [file %.1f%%] cleaning temporary files for %s", phase_prefix, stage_progress("cleaning_temp"), archive_member))
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = "cleaning_temp",
    current_detail = sprintf("cleaning temporary audio files for %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )
  if (cleanup_input && file.exists(input_audio_path)) {
    unlink(input_audio_path, force = TRUE)
  }
  if (!identical(normalizePath(wav_path, winslash = "/", mustWork = FALSE), normalizePath(input_audio_path, winslash = "/", mustWork = FALSE)) &&
      file.exists(wav_path)) {
    unlink(wav_path, force = TRUE)
  }

  file_finished_at <- Sys.time()
  processing_seconds <- as.numeric(difftime(file_finished_at, file_started_at, units = "secs"))
  species_detected <- collapse_species(result$summary_table)

  manifest <- rbind(
    manifest,
    data.frame(
      archive_member = archive_member,
      status = result$status,
      started_at = timestamp_text(file_started_at),
      finished_at = timestamp_text(file_finished_at),
      processing_seconds = processing_seconds,
      processing_time_hms = format_duration(processing_seconds),
      recording_date = result$recording_date,
      recording_latitude = result$recording_latitude,
      recording_longitude = result$recording_longitude,
      predictions_csv = result$predictions_csv,
      summary_csv = result$summary_csv,
      summary_rows = result$summary_rows,
      total_species_identified = result$total_species_identified,
      species_detected = species_detected,
      error_message = if (!is.null(result$error_message)) result$error_message else "",
      stringsAsFactors = FALSE
    )
  )

  write.csv(manifest, manifest_csv, row.names = FALSE)
  write_file_results_txt(manifest, file_results_txt)
  existing_summary_index <<- add_summary_to_index(existing_summary_index, archive_member, result)
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = if (identical(result$status, "error")) "error" else "completed_file",
    current_detail = if (identical(result$status, "error")) {
      sprintf("error while processing %s: %s", archive_member, result$error_message)
    } else {
      sprintf("completed %s", archive_member)
    },
    start_time = start_time,
    total_files = total_files
  )

  eta_seconds <- estimate_remaining_seconds(start_time, nrow(manifest), total_files)
  emit_console(
    sprintf(
      "%s Finished %s | status=%s | rows=%s | species=%s | file_time=%s | eta=%s",
      phase_prefix,
      archive_member,
      result$status,
      if (is.na(result$summary_rows)) "n/a" else result$summary_rows,
      if (is.na(result$total_species_identified)) "n/a" else result$total_species_identified,
      format_duration(processing_seconds),
      format_duration(eta_seconds)
    )
  )

  manifest
}

process_streamed_flac <- function(archive_member,
                                  flac_path,
                                  file_index,
                                  total_files,
                                  manifest,
                                  start_time) {
  process_local_audio_item(
    archive_member = archive_member,
    input_audio_path = flac_path,
    file_index = file_index,
    total_files = total_files,
    manifest = manifest,
    start_time = start_time,
    ready_phase = "extracted_flac",
    ready_detail = sprintf("stream-extracted audio ready: %s", basename(flac_path)),
    cleanup_input = TRUE
  )
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
existing_summary_index <- build_existing_summary_index(file.path(script_dir, "..", "out"))
start_time <- Sys.time()
total_files <- 0L
members_seen <- 0L
scan_complete <- FALSE

manifest <- data.frame(
  archive_member = character(),
  status = character(),
  started_at = character(),
  finished_at = character(),
  processing_seconds = numeric(),
  processing_time_hms = character(),
  recording_date = character(),
  recording_latitude = numeric(),
  recording_longitude = numeric(),
  predictions_csv = character(),
  summary_csv = character(),
  summary_rows = integer(),
  total_species_identified = integer(),
  species_detected = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)

write_file_results_txt(manifest, file_results_txt)
write_summary_of_summaries_txt(
  manifest = manifest,
  output_path = summary_of_summaries_txt,
  source_description = source_description,
  current_member = "",
  current_phase = "starting",
  current_detail = if (identical(source_mode, "archive")) "initializing archive run" else "initializing EcoSounds run",
  current_file_progress = 0,
  start_time = start_time,
  total_files = total_files
)

emit_console("Starting BirdNET source processing run")
emit_console(sprintf("Source: %s", source_description))
emit_console(sprintf("Progress summary text: %s", summary_of_summaries_txt))
emit_console(sprintf("Per-file results text: %s", file_results_txt))

unlink(extract_root, recursive = TRUE, force = TRUE)
dir.create(extract_root, recursive = TRUE, showWarnings = FALSE)

if (identical(source_mode, "archive")) {
  emit_console("Streaming archive members; processing starts as soon as a .flac is found")

  stream_helper <- processx::process$new(
    command = "python",
    args = c(
      file.path(script_dir, "stream_archive_flacs.py"),
      archive_file,
      extract_root,
      as.character(stage_heartbeat_seconds)
    ),
    stdin = "|",
    stdout = "|",
    stderr = "|",
    cleanup_tree = TRUE
  )

  on.exit({
    if (exists("stream_helper") && stream_helper$is_alive()) {
      try(stream_helper$write_input("STOP\n"), silent = TRUE)
      try(stream_helper$kill_tree(), silent = TRUE)
    }
  }, add = TRUE)

  repeat {
    stream_helper$poll_io(1000)

    helper_stdout <- stream_helper$read_output_lines()
    helper_stderr <- stream_helper$read_error_lines()

    if (length(helper_stderr) > 0) {
      emit_console(sprintf("[archive stream] helper stderr: %s", paste(helper_stderr, collapse = " | ")))
    }

    if (length(helper_stdout) > 0) {
      for (line in helper_stdout) {
        event <- parse_archive_stream_event(line)

        if (identical(event$event, "SCAN")) {
          members_seen <- event$members_seen
          total_files <- max(total_files, event$flac_found_so_far)
          detail_text <- sprintf(
            "streaming archive | members_seen=%d | flac_found_so_far=%d | latest_member=%s",
            event$members_seen,
            event$flac_found_so_far,
            event$latest_member
          )
          emit_console(sprintf("[archive stream] %s", detail_text))
          update_live_progress(
            manifest = manifest,
            current_member = "",
            current_phase = "listing_archive",
            current_detail = detail_text,
            start_time = start_time,
            total_files = total_files
          )
        } else if (identical(event$event, "FILE")) {
          members_seen <- event$members_seen
          total_files <- max(total_files, event$flac_found_so_far)
          manifest <- process_streamed_flac(
            archive_member = event$archive_member,
            flac_path = event$flac_path,
            file_index = event$flac_found_so_far,
            total_files = total_files,
            manifest = manifest,
            start_time = start_time
          )
          stream_helper$write_input("NEXT\n")
        } else if (identical(event$event, "COMPLETE")) {
          members_seen <- event$members_seen
          total_files <- event$flac_found_so_far
          scan_complete <- TRUE
          emit_console(sprintf("Archive stream complete. Members seen: %d, flac files found: %d", members_seen, total_files))
          update_live_progress(
            manifest = manifest,
            current_member = "",
            current_phase = "starting",
            current_detail = sprintf("archive stream complete; discovered %d .flac files", total_files),
            start_time = start_time,
            total_files = total_files
          )
        } else if (identical(event$event, "ERROR")) {
          stop(event$error_message)
        }
      }
    }

    if (!stream_helper$is_alive()) {
      break
    }
  }

  if (!scan_complete) {
    final_status <- stream_helper$get_exit_status()
    if (!identical(final_status, 0L)) {
      helper_tail <- c(stream_helper$read_output_lines(), stream_helper$read_error_lines())
      stop(
        paste(
          c("archive stream helper failed", utils::tail(helper_tail, 20)),
          collapse = "\n"
        )
      )
    }
  }
} else {
  emit_console(sprintf("Listing recordings from EcoSounds project %s", as.integer(ecosounds_project_id)))
  ecosounds_token <- ecosounds_get_auth_token(
    workbench_url = ecosounds_workbench_url,
    auth_token = ecosounds_auth_token,
    user_name = ecosounds_user_name,
    password = ecosounds_password
  )
  recordings <- list_ecosounds_recordings(
    workbench_url = ecosounds_workbench_url,
    auth_token = ecosounds_token,
    project_id = ecosounds_project_id,
    recorder_id = ecosounds_recorder_id,
    recorder_name = ecosounds_recorder_name,
    page_size = ecosounds_listing_page_size,
    manifest = manifest,
    start_time = start_time
  )
  recordings <- filter_ecosounds_recordings(
    recordings = recordings,
    recorder_id = ecosounds_recorder_id,
    recorder_name = ecosounds_recorder_name
  )

  if (nrow(recordings) == 0) {
    stop(sprintf("No accessible recordings were returned for EcoSounds project %s.", as.integer(ecosounds_project_id)))
  }

  recordings <- recordings[order(recordings$recorded_date, recordings$site_id, recordings$canonical_file_name), , drop = FALSE]
  total_files <- nrow(recordings)
  recorder_filter_text <- if (!is.na(normalise_optional_integer(ecosounds_recorder_id, "ecosounds_recorder_id"))) {
    sprintf(" | recorder_id=%s", normalise_optional_integer(ecosounds_recorder_id, "ecosounds_recorder_id"))
  } else if (nzchar(normalise_optional_text(ecosounds_recorder_name))) {
    sprintf(" | recorder_name=%s", normalise_optional_text(ecosounds_recorder_name))
  } else {
    ""
  }
  emit_console(sprintf("EcoSounds listing complete. Recordings found: %d%s", total_files, recorder_filter_text))

  for (recording_index in seq_len(total_files)) {
    recording <- recordings[recording_index, , drop = FALSE]
    archive_member <- ecosounds_archive_member(recording)
    output_paths <- output_paths_for_member(output_root, archive_member)
    existing_output_paths <- find_existing_output_paths(
      archive_member = archive_member,
      output_paths = output_paths,
      summary_index = existing_summary_index
    )
    phase_prefix <- sprintf("[%s]", progress_label(recording_index, total_files))

    if (!is.null(existing_output_paths)) {
      file_started_at <- Sys.time()
      manifest <- append_skipped_existing_manifest(
        manifest = manifest,
        archive_member = archive_member,
        output_paths = existing_output_paths,
        file_started_at = file_started_at,
        total_files = total_files,
        start_time = start_time
      )
      emit_console(sprintf("%s Skipping existing results for %s using %s", phase_prefix, archive_member, existing_output_paths$summary_csv))
      next
    }

    emit_console(sprintf("%s [file %.1f%%] downloading %s", phase_prefix, stage_progress("downloading_audio"), archive_member))
    update_live_progress(
      manifest = manifest,
      current_member = archive_member,
      current_phase = "downloading_audio",
      current_detail = sprintf("downloading audio from EcoSounds: %s", archive_member),
      start_time = start_time,
      total_files = total_files
    )

    recording_workspace <- file.path(extract_root, sprintf("recording_%s", recording[["id"]]))
    cleanup_local_workspace(recording_workspace)
    dir.create(recording_workspace, recursive = TRUE, showWarnings = FALSE)

    downloaded_recording <- NULL
    manifest <- tryCatch(
      {
        downloaded_recording <- download_ecosounds_recording(
          recording = recording,
          workbench_url = ecosounds_workbench_url,
          auth_token = ecosounds_token,
          download_root = recording_workspace,
          download_method = ecosounds_download_method,
          powershell_script = ecosounds_powershell_script,
          project_id = ecosounds_project_id,
          refresh_powershell_script = ecosounds_refresh_powershell_script
        )

        updated_manifest <- process_local_audio_item(
          archive_member = downloaded_recording$archive_member,
          input_audio_path = downloaded_recording$local_audio_path,
          file_index = recording_index,
          total_files = total_files,
          manifest = manifest,
          start_time = start_time,
          ready_phase = "downloaded_audio",
          ready_detail = sprintf("downloaded audio ready: %s", basename(downloaded_recording$local_audio_path)),
          cleanup_input = TRUE
        )

        if (file.exists(downloaded_recording$local_audio_path)) {
          stop(sprintf("Downloaded local audio was not removed after processing: %s", downloaded_recording$local_audio_path))
        }

        updated_manifest
      },
      finally = {
        cleanup_local_workspace(recording_workspace)
      }
    )
  }
}

write_summary_of_summaries_txt(
  manifest = manifest,
  output_path = summary_of_summaries_txt,
  source_description = source_description,
  current_member = "",
  current_phase = "complete",
  current_detail = "source processing complete",
  current_file_progress = 100,
  start_time = start_time,
  total_files = total_files
)

emit_console(sprintf("Source processing complete. Manifest: %s", manifest_csv))
manifest
