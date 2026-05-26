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

find_repo_root <- function(start_path) {
  current_path <- normalizePath(start_path, mustWork = TRUE)

  if (file.info(current_path)$isdir) {
    current_dir <- current_path
  } else {
    current_dir <- dirname(current_path)
  }

  repeat {
    has_scripts_dir <- dir.exists(file.path(current_dir, "scripts"))
    has_readme <- file.exists(file.path(current_dir, "README.md"))

    if (has_scripts_dir && has_readme) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("could not locate the repository root from the current script location")
    }

    current_dir <- parent_dir
  }
}

timestamp_text <- function(x = Sys.time()) {
  format(x, "%Y-%m-%d %H:%M:%S")
}

emit_console <- function(message) {
  cat(sprintf("[%s] %s\n", timestamp_text(), message))
  flush.console()
}

build_existing_amalgamated_manifest <- function(amalgamated_root) {
  manifest <- data.frame(
    recorder_id = character(),
    recording_key = character(),
    source_summary_csv = character(),
    source_predictions_csv = character(),
    amalgamated_summary_csv = character(),
    amalgamated_predictions_csv = character(),
    copy_verification_status = character(),
    copy_verification_message = character(),
    source_deletion_status = character(),
    source_deletion_message = character(),
    stringsAsFactors = FALSE
  )

  if (!dir.exists(amalgamated_root)) {
    return(manifest)
  }

  summary_csv_files <- list.files(
    amalgamated_root,
    pattern = "_birdnet_species_summary\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
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
    return(manifest)
  }

  data.frame(
    recorder_id = vapply(summary_csv_files, extract_recorder_label_from_path, character(1), fallback = "UNKNOWN"),
    recording_key = vapply(summary_csv_files, canonical_recording_key, character(1)),
    source_summary_csv = rep("", length(summary_csv_files)),
    source_predictions_csv = rep("", length(summary_csv_files)),
    amalgamated_summary_csv = summary_csv_files,
    amalgamated_predictions_csv = predictions_csv_files,
    copy_verification_status = rep("", length(summary_csv_files)),
    copy_verification_message = rep("", length(summary_csv_files)),
    source_deletion_status = rep("", length(summary_csv_files)),
    source_deletion_message = rep("", length(summary_csv_files)),
    stringsAsFactors = FALSE
  )
}

verify_single_copy <- function(source_path, destination_path) {
  if (!file.exists(source_path)) {
    return(list(ok = FALSE, message = sprintf("source file missing: %s", source_path)))
  }
  if (!file.exists(destination_path)) {
    return(list(ok = FALSE, message = sprintf("destination file missing: %s", destination_path)))
  }

  source_info <- file.info(source_path)
  destination_info <- file.info(destination_path)
  if (is.na(source_info$size) || is.na(destination_info$size)) {
    return(list(ok = FALSE, message = "could not determine file size for verification"))
  }
  if (!identical(as.numeric(source_info$size), as.numeric(destination_info$size))) {
    return(list(
      ok = FALSE,
      message = sprintf(
        "file size mismatch: %s bytes at source versus %s bytes at destination",
        source_info$size,
        destination_info$size
      )
    ))
  }

  source_md5 <- unname(tools::md5sum(source_path)[[1]])
  destination_md5 <- unname(tools::md5sum(destination_path)[[1]])
  if (is.na(source_md5) || is.na(destination_md5) || !identical(source_md5, destination_md5)) {
    return(list(ok = FALSE, message = "md5 checksum mismatch between source and destination"))
  }

  list(ok = TRUE, message = "verified")
}

verify_amalgamated_copies <- function(manifest) {
  if (nrow(manifest) == 0) {
    return(manifest)
  }

  manifest$copy_verification_status <- "verified"
  manifest$copy_verification_message <- ""

  for (index in seq_len(nrow(manifest))) {
    summary_check <- verify_single_copy(
      manifest$source_summary_csv[[index]],
      manifest$amalgamated_summary_csv[[index]]
    )
    predictions_check <- verify_single_copy(
      manifest$source_predictions_csv[[index]],
      manifest$amalgamated_predictions_csv[[index]]
    )

    if (!isTRUE(summary_check$ok) || !isTRUE(predictions_check$ok)) {
      manifest$copy_verification_status[[index]] <- "failed"
      manifest$copy_verification_message[[index]] <- paste(
        c(
          if (!isTRUE(summary_check$ok)) paste("summary:", summary_check$message) else NULL,
          if (!isTRUE(predictions_check$ok)) paste("predictions:", predictions_check$message) else NULL
        ),
        collapse = " | "
      )
    }
  }

  manifest
}

delete_original_source_files <- function(manifest) {
  if (nrow(manifest) == 0) {
    return(manifest)
  }

  manifest$source_deletion_status <- "deleted"
  manifest$source_deletion_message <- ""

  for (index in seq_len(nrow(manifest))) {
    if (!identical(manifest$copy_verification_status[[index]], "verified")) {
      manifest$source_deletion_status[[index]] <- "not_deleted"
      manifest$source_deletion_message[[index]] <- "source files retained because copy verification did not succeed"
      next
    }

    source_paths <- c(manifest$source_summary_csv[[index]], manifest$source_predictions_csv[[index]])
    existing_paths <- source_paths[file.exists(source_paths)]
    unlink(existing_paths, force = TRUE)
    remaining_paths <- source_paths[file.exists(source_paths)]

    if (length(remaining_paths) > 0) {
      manifest$source_deletion_status[[index]] <- "delete_failed"
      manifest$source_deletion_message[[index]] <- paste(
        "could not delete:",
        paste(remaining_paths, collapse = ", ")
      )
    }
  }

  manifest
}

merge_manifest_metadata <- function(existing_manifest, current_manifest) {
  if (nrow(existing_manifest) == 0) {
    return(current_manifest)
  }
  if (nrow(current_manifest) == 0) {
    return(existing_manifest)
  }

  current_match <- match(existing_manifest$amalgamated_summary_csv, current_manifest$amalgamated_summary_csv)
  columns_to_update <- setdiff(names(current_manifest), c("recorder_id", "recording_key", "amalgamated_summary_csv", "amalgamated_predictions_csv"))
  for (column_name in columns_to_update) {
    matched_rows <- !is.na(current_match)
    existing_manifest[[column_name]][matched_rows] <- current_manifest[[column_name]][current_match[matched_rows]]
  }

  existing_manifest
}

safe_output_stem_component <- function(text_value) {
  text_value <- gsub("[^A-Za-z0-9_-]", "_", as.character(text_value))
  text_value <- gsub("_+", "_", text_value)
  text_value <- gsub("^_|_$", "", text_value)

  if (!nzchar(text_value)) {
    return("recording")
  }

  text_value
}

build_amalgamation_manifest <- function(source_pairs) {
  manifest <- data.frame(
    recorder_id = character(),
    recording_key = character(),
    source_summary_csv = character(),
    source_predictions_csv = character(),
    amalgamated_summary_csv = character(),
    amalgamated_predictions_csv = character(),
    copy_verification_status = character(),
    copy_verification_message = character(),
    source_deletion_status = character(),
    source_deletion_message = character(),
    stringsAsFactors = FALSE
  )

  if (nrow(source_pairs) == 0) {
    return(manifest)
  }

  amalgamated_root <- unique(source_pairs$amalgamated_root)
  if (length(amalgamated_root) != 1) {
    stop("source_pairs must contain exactly one amalgamated_root value")
  }

  used_stems <- new.env(parent = emptyenv())

  for (index in seq_len(nrow(source_pairs))) {
    row <- source_pairs[index, , drop = FALSE]
    recorder_dir <- file.path(amalgamated_root, row$recorder_id[[1]])
    dir.create(recorder_dir, recursive = TRUE, showWarnings = FALSE)

    source_stem <- sub("_birdnet_species_summary\\.csv$", "", basename(row$summary_csv[[1]]))
    stem_key <- sprintf("%s::%s", row$recorder_id[[1]], source_stem)
    dest_stem <- source_stem

    if (exists(stem_key, envir = used_stems, inherits = FALSE)) {
      previous_key <- get(stem_key, envir = used_stems, inherits = FALSE)
      if (!identical(previous_key, row$recording_key[[1]])) {
        dest_stem <- paste0(
          source_stem,
          "__",
          safe_output_stem_component(gsub("/", "_", row$recording_key[[1]], fixed = TRUE))
        )
      }
    }

    unique_stem <- dest_stem
    suffix <- 2L
    repeat {
      summary_dest <- file.path(recorder_dir, paste0(unique_stem, "_birdnet_species_summary.csv"))
      predictions_dest <- file.path(recorder_dir, paste0(unique_stem, "_birdnet_predictions.csv"))
      if (!file.exists(summary_dest) && !file.exists(predictions_dest)) {
        break
      }
      unique_stem <- sprintf("%s__%d", dest_stem, suffix)
      suffix <- suffix + 1L
    }

    file.copy(row$summary_csv[[1]], summary_dest, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    file.copy(row$predictions_csv[[1]], predictions_dest, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    assign(sprintf("%s::%s", row$recorder_id[[1]], unique_stem), row$recording_key[[1]], envir = used_stems)
    assign(stem_key, row$recording_key[[1]], envir = used_stems)

    manifest <- rbind(
      manifest,
      data.frame(
        recorder_id = row$recorder_id[[1]],
        recording_key = row$recording_key[[1]],
        source_summary_csv = row$summary_csv[[1]],
        source_predictions_csv = row$predictions_csv[[1]],
        amalgamated_summary_csv = summary_dest,
        amalgamated_predictions_csv = predictions_dest,
        copy_verification_status = "not_run",
        copy_verification_message = "",
        source_deletion_status = "not_run",
        source_deletion_message = "",
        stringsAsFactors = FALSE
      )
    )
  }

  manifest
}

script_dir <- get_script_dir()
repo_root <- find_repo_root(script_dir)
source(file.path(repo_root, "scripts", "cleanup_user_options.R"), local = environment())
source(file.path(repo_root, "scripts", "recording_key_helpers.R"), local = environment())

out_root <- file.path(repo_root, "out")
amalgamated_root <- file.path(out_root, "amalgamated_birdnet_output")
cleanup_amalgamated_recorder_name <- if (exists("cleanup_amalgamated_recorder_name", inherits = FALSE)) {
  trimws(as.character(cleanup_amalgamated_recorder_name[[1]]))
} else {
  ""
}
cleanup_verify_copied_files <- if (exists("cleanup_verify_copied_files", inherits = FALSE)) {
  cleanup_verify_copied_files[[1]]
} else {
  TRUE
}
cleanup_delete_original_source_files <- if (exists("cleanup_delete_original_source_files", inherits = FALSE)) {
  cleanup_delete_original_source_files[[1]]
} else {
  FALSE
}
cleanup_recorder_filter <- if (nzchar(cleanup_amalgamated_recorder_name)) {
  normalise_recorder_label(cleanup_amalgamated_recorder_name)
} else {
  ""
}

if (!is.logical(cleanup_verify_copied_files) || length(cleanup_verify_copied_files) != 1 || is.na(cleanup_verify_copied_files)) {
  stop("cleanup_verify_copied_files must be TRUE or FALSE")
}
if (!is.logical(cleanup_delete_original_source_files) || length(cleanup_delete_original_source_files) != 1 || is.na(cleanup_delete_original_source_files)) {
  stop("cleanup_delete_original_source_files must be TRUE or FALSE")
}
effective_verify_copies <- isTRUE(cleanup_verify_copied_files) || isTRUE(cleanup_delete_original_source_files)

if (!dir.exists(out_root)) {
  stop(sprintf("output directory not found: %s", out_root))
}

summary_csv_files <- list.files(
  out_root,
  pattern = "_birdnet_species_summary\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
summary_csv_files <- summary_csv_files[!grepl("/analysis/", summary_csv_files, fixed = TRUE)]
summary_csv_files <- summary_csv_files[!grepl("/amalgamated_birdnet_output/", summary_csv_files, fixed = TRUE)]
summary_csv_files <- summary_csv_files[file.exists(summary_csv_files)]

predictions_csv_files <- sub(
  "_birdnet_species_summary\\.csv$",
  "_birdnet_predictions.csv",
  summary_csv_files
)
paired_rows <- file.exists(predictions_csv_files)
summary_csv_files <- summary_csv_files[paired_rows]
predictions_csv_files <- predictions_csv_files[paired_rows]

emit_console(sprintf("found %d paired BirdNET output files for amalgamation", length(summary_csv_files)))

if (length(summary_csv_files) == 0) {
  emit_console("no paired BirdNET outputs found outside analysis/amalgamated directories; nothing to do")
  quit(save = "no", status = 0)
}

source_pairs <- data.frame(
  summary_csv = summary_csv_files,
  predictions_csv = predictions_csv_files,
  recording_key = vapply(summary_csv_files, canonical_recording_key, character(1)),
  recorder_id = vapply(summary_csv_files, extract_recorder_label_from_path, character(1), fallback = "UNKNOWN"),
  modified_time = pmax(
    as.numeric(file.info(summary_csv_files)$mtime),
    as.numeric(file.info(predictions_csv_files)$mtime)
  ),
  amalgamated_root = amalgamated_root,
  stringsAsFactors = FALSE
)
source_pairs <- source_pairs[
  order(source_pairs$recording_key, -source_pairs$modified_time, source_pairs$summary_csv),
  ,
  drop = FALSE
]
source_pairs <- source_pairs[!duplicated(source_pairs$recording_key), , drop = FALSE]
if (nzchar(cleanup_recorder_filter)) {
  source_pairs <- source_pairs[source_pairs$recorder_id == cleanup_recorder_filter, , drop = FALSE]
}
source_pairs <- source_pairs[order(source_pairs$recorder_id, source_pairs$recording_key), , drop = FALSE]

if (nrow(source_pairs) == 0) {
  if (nzchar(cleanup_recorder_filter)) {
    emit_console(sprintf("no paired BirdNET outputs matched cleanup_amalgamated_recorder_name = %s; nothing to do", cleanup_recorder_filter))
  } else {
    emit_console("no paired BirdNET outputs found outside analysis/amalgamated directories; nothing to do")
  }
  quit(save = "no", status = 0)
}

if (nzchar(cleanup_recorder_filter)) {
  dir.create(amalgamated_root, recursive = TRUE, showWarnings = FALSE)
  target_recorder_dir <- file.path(amalgamated_root, cleanup_recorder_filter)
  if (dir.exists(target_recorder_dir)) {
    unlink(target_recorder_dir, recursive = TRUE, force = TRUE)
  }
} else {
  if (dir.exists(amalgamated_root)) {
    unlink(amalgamated_root, recursive = TRUE, force = TRUE)
  }
}
dir.create(amalgamated_root, recursive = TRUE, showWarnings = FALSE)

manifest <- build_amalgamation_manifest(source_pairs)
if (isTRUE(effective_verify_copies)) {
  manifest <- verify_amalgamated_copies(manifest)
  verification_failures <- sum(manifest$copy_verification_status != "verified")
  emit_console(sprintf("copy verification complete: %d verified, %d failed", nrow(manifest) - verification_failures, verification_failures))
  if (verification_failures > 0) {
    manifest_csv <- file.path(amalgamated_root, "amalgamation_manifest.csv")
    write.csv(manifest, manifest_csv, row.names = FALSE)
    stop(sprintf("copy verification failed for %d amalgamated file pairs; original source files were not deleted", verification_failures))
  }
}
if (isTRUE(cleanup_delete_original_source_files)) {
  manifest <- delete_original_source_files(manifest)
  emit_console(sprintf(
    "source deletion complete: %d deleted, %d retained, %d failed",
    sum(manifest$source_deletion_status == "deleted"),
    sum(manifest$source_deletion_status == "not_deleted"),
    sum(manifest$source_deletion_status == "delete_failed")
  ))
}
manifest_csv <- file.path(amalgamated_root, "amalgamation_manifest.csv")
final_manifest <- merge_manifest_metadata(
  build_existing_amalgamated_manifest(amalgamated_root),
  manifest
)
write.csv(final_manifest, manifest_csv, row.names = FALSE)

if (nzchar(cleanup_recorder_filter)) {
  emit_console(sprintf("rebuilt amalgamated recorder directory for %s under %s", cleanup_recorder_filter, amalgamated_root))
} else {
  emit_console(sprintf("created amalgamated recorder directories under %s", amalgamated_root))
}
emit_console(sprintf("copied %d unique BirdNET output pairs across %d recorders", nrow(manifest), length(unique(manifest$recorder_id))))
emit_console(sprintf("amalgamation manifest written to %s", manifest_csv))
