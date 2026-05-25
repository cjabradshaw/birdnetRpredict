# user-defined download settings --------------------------------------------
source_mode <- "archive"  # "archive" or "ecosounds"

archive_file <- "/Volumes/brad0317/Documents/acoustic/GEL_D/GEL_D19082025_25102025.tar.zst"
#archive_file <- "/Volumes/CSE-GlobalEcology/bradshaw/acoustic/GEL_E/GEL_E20250924_20251130.tar.zst"

ecosounds_workbench_url <- "https://api.ecosounds.org"
ecosounds_project_id <- 1281L

# EcoSounds site/recorder ID
# GEL_A is project 1281 site 7238;
# GEL_B is site 7239
# GEL_C is site 7240
# GEL_D is site 7241
# GEL_E is site 7242
ecosounds_recorder_id <- 7238L
ecosounds_recorder_name <- ""  # exact EcoSounds site/recorder name; use this instead of ecosounds_recorder_id if preferred
ecosounds_download_method <- "api_then_powershell"  # "api", "powershell", or "api_then_powershell"
ecosounds_powershell_script <- "/Users/brad0317/Downloads/download_audio_files.ps1"
ecosounds_refresh_powershell_script <- TRUE
ecosounds_listing_page_size <- 500L
ecosounds_auth_token <- trimws(Sys.getenv("ECOSOUNDS_AUTH_TOKEN", unset = ""))
ecosounds_user_name <- trimws(Sys.getenv("ECOSOUNDS_USERNAME", unset = ""))
ecosounds_password <- Sys.getenv("ECOSOUNDS_PASSWORD", unset = "")

species_csv <- file.path(
  script_dir,
  "..",
  "data",
  "species_lists",
  "regional",
  "lower_murray",
  "BirdNet_SA_LowerMurray_Tolderol_matches.csv"
)
pipeline_timezone <- "Australia/Adelaide"
fallback_latitude <- -35.52235
fallback_longitude <- 139.10576
prediction_min_confidence <- 0.05
summary_confidence_threshold <- 0.05
use_arrow <- TRUE
stage_heartbeat_seconds <- 5
stage_timeout_seconds <- 3600
# ---------------------------------------------------------------------------
