# Pipeline for analysing audio files for bird identification using <a href="https://birdnet.cornell.edu">BirdNET</a>
<img align="right" src="www/Acrocephalus_australis_-_Bushell's_Lagoon.jpg" title="Australian reed warbler at Bushell's Lagoon; image credit: John Harrison, Wikimedia Commons" alt="Australian reed warbler at Bushell's Lagoon; image credit: John Harrison, Wikimedia Commons" width="220" style="margin-top: 20px">

In partnership with the <a href="http://ngarrindjeri.com.au/">Ngarrindjeri Aboriginal Corporation</a> and the Raukkan Rangers, Flinders University (<a href="http://globalecologyflinders.com/">Global Ecology Laboratory</a>) under the auspices of the Australian Research Council <a href="http://ciehf.au">Centre of Excellence for Indigenous and Environmental Histories and Futures</a> (CIEHF) have set up an initial array of 5 passive acoustic recorders to document the change in bird diversity in recently restored wetlands within the <a href="https://www.environment.sa.gov.au/topics/water-and-river-murray/projects-plans-security-and-legislation/water-projects/coorong/current-projects/on-ground-works/ogw-teringie">Teringie Wetlands</a> complex in South Australia. We are comparing these records to existing wetland complexes and control saltponds devoid of most birdlife (control). These data belong to the Ngarrindjeri Nation.

The audio-file repository is available at <a href="https://api.ecosounds.org/projects/1281">EcoSounds</a> (but not currently open-access).

## Workflow

R-based <a href="https://birdnet.cornell.edu">BirdNET</a> workflow for:

1. processing a single audio file
2. processing a large local `.tar.zst` archive one `.flac` at a time <strong>OR</strong> downloading and processing original recordings directly from an authenticated EcoSounds project
3. converting non-`.wav` source audio to `.wav`
4. filtering BirdNET predictions with a repository-local species list
5. writing per-file prediction summaries and rolling progress reports
6. post-processing existing summary CSVs into plots and aggregate tables

## Repository layout

```text
birdnetRpredict/
├── README.md
├── data/
│   └── species_lists/
│       ├── reference/
│       └── regional/
│           └── lower_murray/
├── scripts/
│   ├── analyse_birdnet_output.R
│   ├── birdnet_helpers.R
│   ├── birdnetID.R
│   ├── cleanup_amalgamated_outputs.R
│   ├── cleanup_user_options.R
│   ├── downloading_user_options.R
│   ├── process_download_common.R
│   ├── process_download_pipeline.R
│   ├── process_ecosounds.R
│   ├── process_tar_archive.R
│   └── recording_key_helpers.R
└── www
```

## What the pipeline does

### Single-file workflow

`scripts/birdnetID.R`:

1. loads the shared helper functions
2. points to a single `.wav` file
3. loads a species CSV from this repository
4. builds BirdNET models
5. extracts date/time and coordinates from the file name where available
6. uses the BirdNET range model to narrow candidate species for that file
7. runs BirdNET on the audio
8. writes:
   - a filtered prediction CSV
   - a cleaned species summary CSV

### Source-processing workflow

The source-processing pipeline is now split across six scripts:

1. `scripts/downloading_user_options.R`
   - holds the user-editable download settings in one place
   - includes `source_mode <- "archive"` or `source_mode <- "ecosounds"`
   - includes the archive path, EcoSounds credentials/settings, and other user-adjustable processing settings

2. `scripts/process_download_pipeline.R`
   - sources `downloading_user_options.R`
   - dispatches automatically to the correct source-specific entrypoint based on `source_mode`

3. `scripts/process_tar_archive.R`
   - archive-only entrypoint
   - opens a source `.tar.zst`
   - streams the archive sequentially instead of doing a full pre-scan
   - starts processing as soon as the next `.flac` is encountered
   - extracts a single `.flac` while preserving the internal archive path
   - converts that `.flac` to `.wav` with `ffmpeg`

4. `scripts/process_ecosounds.R`
   - EcoSounds-only entrypoint
   - authenticates against the EcoSounds / Acoustic Workbench API
   - lists only the recordings accessible in the chosen project and matching the selected recorder/site filter
   - when a site ID is set, uses the site-specific EcoSounds filter endpoint and a larger page size to obtain the file list faster
   - can optionally restrict processing to a single recorder/site in the user-defined settings
   - downloads each original recording file into a temporary local workspace one file at a time
   - if original-file download is not permitted for your account, falls back to chunked `media.wav` downloads and concatenates them locally
   - if EcoSounds denies direct access to the original file (`403`), falls back to the standard `media.wav` route for that recording
   - can also fall back to the EcoSounds-generated PowerShell downloader script for single-recording downloads
   - processes `.wav` recordings directly and converts other source formats to `.wav` when needed
   - deletes the downloaded local audio immediately after that one file is analysed, before downloading the next file

5. `scripts/cleanup_amalgamated_outputs.R`
   - optional post-processing clean-up step to run after archive/EcoSounds processing has finished
   - scans recorder outputs across both archive-derived and EcoSounds-derived directories already present under `out/`
   - honours `cleanup_amalgamated_recorder_name` from `scripts/cleanup_user_options.R`, so you can update just one recorder such as `GEL_A` instead of all recorder amalgamations
   - can optionally verify each copied summary/predictions CSV pair against its original source files before completion
   - can optionally delete the original unamalgamated source CSV pairs after successful copy verification
   - incrementally updates `out/amalgamated_birdnet_output/<RECORDER>/` directories so new deduplicated `*_birdnet_species_summary.csv` + `*_birdnet_predictions.csv` pairs are added without removing valid files already present there
   - collapses byte-identical duplicate source pairs that appear in multiple roots (for example, both archive-derived and EcoSounds-derived output trees), but stops with an explicit error if the same recorder/timestamp key appears more than once with different file contents

6. `scripts/recording_key_helpers.R`
   - shared helper functions for recorder-name normalisation and origin-agnostic recording-key matching used by the downloader, analysis deduplication, and amalgamation clean-up

7. `scripts/cleanup_user_options.R`
   - holds clean-up-only settings separately from the downloading pipeline settings
   - currently includes the optional recorder filter plus copy-verification and source-deletion settings used by `cleanup_amalgamated_outputs.R`

The shared source-processing logic then:

1. runs the same BirdNET summary workflow used by the single-file script
2. writes per-file CSV outputs
3. deletes temporary downloaded/extracted audio files
4. moves to the next source item until the run is complete
5. can optionally be followed by `Rscript scripts/cleanup_amalgamated_outputs.R` to assemble recorder-level amalgamated output directories for later resume checks

Archive mode still avoids unpacking the entire archive at once and avoids waiting for a full member enumeration before processing starts. macOS sidecar entries such as `._*.flac` and `__MACOSX/` metadata are skipped during archive streaming.

### Post-processing analysis workflow

`scripts/analyse_birdnet_output.R`:

1. searches recursively under `out/` for existing `*_birdnet_species_summary.csv` files
2. ignores `out/analysis/` and, when the same recording exists in multiple places, prefers `out/amalgamated_birdnet_output/` over the original recorder/source directories
3. combines the summary CSVs that are already present and readable
4. filters detections by a user-defined minimum confidence threshold
5. bins detections into a user-defined time step (default `60` minutes)
6. writes aggregate CSV tables plus plots for:
    - identifications over time
    - cumulative new species over time
    - identifications per species
    - temporal autocorrelation, partial autocorrelation, spectral periodicity, and Ljung-Box periodicity tests for both detections-per-bin and unique-species-per-bin

This script is intended to work while archive processing is still incomplete. You can rerun it at any time and it will analyse whatever summary CSVs currently exist in `out/`.

## Determining coordinates and date

The pipeline first tries to parse metadata from the audio file name.

For a name like:

```text
20251123T080000+0930_REC_-31.52235+152.10576.flac
```

the scripts derive:

- date/time: `2025-11-23 08:00:00 +0930`
- latitude: `-35.52235`
- longitude: `139.10576`

The archive pipeline extracts `.flac`, converts it to `.wav`, and keeps the same basename, so the parsed coordinates and timestamp continue to apply during BirdNET processing.

If a file name cannot be parsed, the scripts can fall back to user-defined default latitude, longitude, and date values.

## Species lists

Species list files are stored in this repository under:

- `data/species_lists/reference/`
- `data/species_lists/regional/lower_murray/`

The current scripts use:

```text
data/species_lists/regional/lower_murray/BirdNet_SA_LowerMurray_Tolderol_matches.csv
```

That file is combined with the BirdNET location/week range model to reduce false positives before prediction summaries are written.

## Requirements

- R packages: <code>birdnetR</code>, <code>processx</code>, <code>callr</code>, <code>jsonlite</code>
- casks: <code>ffmpeg</code>, <code>tar</code> with <code>--zstd</code> support, <code>zstd</code>

The current environment also expects BirdNET's Python dependencies to be installable through <a href="https://github.com/birdnet-team/birdnetR"><code>birdnetR</code></a>.

## User-defined settings

Main user-editable settings are at the top of the scripts.

### `scripts/birdnetID.R`

Edit:

- `audio_file`
- `species_csv`
- `fallback_latitude`
- `fallback_longitude`
- `prediction_min_confidence`
- `summary_confidence_threshold`

### `scripts/downloading_user_options.R`

Edit:

- `source_mode`
- `archive_file`
- `ecosounds_workbench_url`
- `ecosounds_project_id`
- `ecosounds_recorder_id`
- `ecosounds_recorder_name`
- `ecosounds_download_method`
- `ecosounds_powershell_script`
- `ecosounds_refresh_powershell_script`
- `ecosounds_listing_page_size`
- `ecosounds_auth_token`
- `ecosounds_user_name`
- `ecosounds_password`
- `species_csv`
- `pipeline_timezone`
- `fallback_latitude`
- `fallback_longitude`
- `prediction_min_confidence`
- `summary_confidence_threshold`
- `use_arrow`
- `stage_heartbeat_seconds`
- `stage_timeout_seconds`

These control which source pipeline is used, whether the run processes a local archive or an authenticated EcoSounds project, which single EcoSounds recorder/site is included (if any), which species filter is used, how strict the prediction summaries are, and how often the source-processing pipeline emits heartbeat updates while waiting on extraction/download or BirdNET subprocess stages.

For EcoSounds access, prefer supplying credentials through environment variables rather than storing secrets in the script:

- `ECOSOUNDS_AUTH_TOKEN`
- `ECOSOUNDS_USERNAME`
- `ECOSOUNDS_PASSWORD`

If `ECOSOUNDS_AUTH_TOKEN` is supplied, the script uses it directly. Otherwise it logs in with `ECOSOUNDS_USERNAME` + `ECOSOUNDS_PASSWORD` and then downloads recordings from the selected project.

To process only one EcoSounds recorder at a time, set one of these near the top of `scripts/downloading_user_options.R`:

- `ecosounds_recorder_id <- 7238L` for the `GEL_A` EcoSounds site in project `1281`
- `ecosounds_recorder_name <- "GEL_A"` for an exact recorder/site name match when you know the API site name matches that label

The EcoSounds listing request itself is restricted to that recorder/site before files are queued for download. Leave both empty if you want the whole project. Do not set both at once.

### `scripts/cleanup_user_options.R`

Edit:

- `cleanup_amalgamated_recorder_name`
- `cleanup_verify_copied_files`
- `cleanup_delete_original_source_files`

For `scripts/cleanup_amalgamated_outputs.R`, you can optionally restrict the clean-up pass to one recorder by setting, for example:

- `cleanup_amalgamated_recorder_name <- "GEL_A"` to rebuild only the `GEL_A` amalgamated directory

Leave `cleanup_amalgamated_recorder_name <- ""` to rebuild all recorder amalgamations.

Additional clean-up controls:

- `cleanup_verify_copied_files <- TRUE` to cross-check that each copied amalgamated summary/predictions CSV pair matches its original source files
- `cleanup_delete_original_source_files <- FALSE` to retain the original unamalgamated source CSV pairs by default; set to `TRUE` only if you want those originals deleted after successful copy verification

The default EcoSounds settings now also include:

- `ecosounds_download_method <- "api_then_powershell"` to try the direct API first and then fall back to the EcoSounds-generated PowerShell downloader
- `ecosounds_powershell_script <- "/Users/brad0317/Downloads/download_audio_files.ps1"` as an optional local script path to reuse when refresh is disabled
- `ecosounds_refresh_powershell_script <- TRUE` to regenerate the downloader script from EcoSounds for the current authenticated session before PowerShell fallback downloads
- `ecosounds_listing_page_size <- 500L` to reduce the number of listing pages needed for large sites

### `scripts/analyse_birdnet_output.R`

Edit the user-defined settings directly near the top of the script:

- `summary_root`
- `output_root`
- `analysis_timezone`
- `bin_minutes`
- `diversity_window_days`
- `top_species_time_bin_minutes`
- `rolling_mean_window_days`
- `min_confidence`
- `periodicity_max_lag_bins`
- `show_plots_in_session`
- `ala_sanity_check_enabled`
- `ala_sanity_check_remove_improbable`
- `ala_auth_mode`
- `ala_match_radius_km`
- `ala_min_local_occurrence_records`
- `ala_download_reason_id`
- `ala_user_name`
- `ala_email`
- `ala_password`
- `diel_sanity_check_enabled`
- `diel_sanity_check_remove_improbable`
- `xeno_canto_api_key`
- `diel_min_reference_record_count`
- `diel_xeno_canto_per_page`
- `diel_inaturalist_per_page`

These control which existing summary CSVs are included, where the analysis outputs are written, the temporal bin size used by the plots, the diversity-analysis window length, and the minimum confidence required for a detection to be counted. The <a href="http://www.ala.org.au">Atlas of Living Australia</a> (ALA) settings control an optional pre-plot sanity check that queries the public ALA biocache API for species counts within the configured recorder-region radius, flagging species as potentially suspicious when no nearby records are found or when only a few nearby records are returned. Downstream tables and figures now standardise scientific names to the <a href="https://www.worldbirdnames.org/new/">IOC World Bird List</a> wherever an IOC crosswalk entry is available in the repository, while retaining the current common names already supplied by BirdNET. The ALA count query uses the ALA `species` field so subspecies records are counted under the detected species, matching the species-level totals returned by ALA downloads more closely. When BirdNET names clash with ALA nomenclature, the script now cross-checks against ALA accepted taxonomy and any IOC crosswalk entries already present in the repository, then re-queries using the best resolved official name. If `ala_sanity_check_remove_improbable <- TRUE`, detections for species that fail the configured ALA minimum local-occurrence criterion are removed before the later summaries and plots are built. The online diel settings control an optional pre-summary filter that queries public bird-sound sources directly to identify detections that occur in an unlikely light phase.

For the ALA sanity check, you can either:

- use `ala_auth_mode <- "ala"` if you want to retain your ALA account details in the output metadata fields; the current API-based count query does not require them to run
- use `ala_auth_mode <- "none"` to skip the ALA check entirely

ALA username, email, and password fields are retained in the settings block for compatibility with the earlier workflow, but the current count-based sanity check does not require them. If you want ALA-failing species removed from all downstream tables and plots, set `ala_sanity_check_remove_improbable <- TRUE`.
Set `ala_download_reason_id <- 4L` for the ALA reason `scientific research`. The current public ALA biocache count endpoint used by this script does not require that field for radius counts, but the setting is retained explicitly so the workflow records the intended ALA use case.

For the online diel sanity check:

- set `XENO_CANTO_API_KEY` in your shell or assign `xeno_canto_api_key` near the top of `scripts/analyse_birdnet_output.R` before running the script
- the script queries `xeno-canto` first for each detected species
- if `xeno-canto` returns no matches, the script falls back to `iNaturalist` sound observations

The script classifies returned source records into `daylight`, `twilight`, and `night` using each source record's date, time, and coordinates. `xeno-canto` (see below) does not publish a timezone field, so its source-side light-phase classification uses a longitude-based timezone estimate; `iNaturalist` uses the observation timezone when available. If `diel_sanity_check_remove_improbable <- TRUE`, BirdNET detections in a light phase not supported by the chosen online source are removed before downstream summaries and plots are built. To avoid over-filtering from very sparse public records, a species is only assessed when at least `diel_min_reference_record_count` usable source records are available.

## How to run

### Single file

```bash
Rscript scripts/birdnetID.R
```

### Source-processing pipeline

```bash
Rscript scripts/process_download_pipeline.R
```

With `source_mode <- "ecosounds"`, make sure your EcoSounds credentials are available first, for example:

```bash
export ECOSOUNDS_USERNAME="your_username"
export ECOSOUNDS_PASSWORD="your_password"
Rscript scripts/process_download_pipeline.R
```

Or set `ecosounds_auth_token`, `ecosounds_user_name`, and `ecosounds_password` directly in `scripts/downloading_user_options.R` before running the pipeline.

If you want to force a specific source entrypoint directly, you can run:

```bash
Rscript scripts/process_tar_archive.R
Rscript scripts/process_ecosounds.R
```

Those entrypoints still read `scripts/downloading_user_options.R`, but they now stop if `source_mode` does not match the script you ran.

### Post-processing analysis

Open `scripts/analyse_birdnet_output.R` in RStudio, VS Code, or another R editor, adjust the settings block if needed, then run the script inside R.

The script is intended to be run as a standalone analysis file rather than driven by command-line arguments.

If you want to provide the online-source and ALA credentials explicitly inside **R / RStudio before sourcing the script**, run:

```r
Sys.setenv(
  XENO_CANTO_API_KEY = "your_xeno_canto_api_key",
  ALA_USERNAME = "your_ala_username",
  ALA_EMAIL = "your_ala_email",
  ALA_PASSWORD = "your_ala_password"
)

source("scripts/analyse_birdnet_output.R")
```

If you prefer to keep them as R objects for that session instead of environment variables, open `scripts/analyse_birdnet_output.R` and set the top-of-file values directly before running:

```r
ala_auth_mode <- "ala"
ala_user_name <- "your_ala_username"
ala_email <- "your_ala_email"
ala_password <- "your_ala_password"
xeno_canto_api_key <- "your_xeno_canto_api_key"
```

For an RStudio workflow, the simplest pattern is:

```r
setwd("/path/to/birdnetRpredict")

Sys.setenv(
  XENO_CANTO_API_KEY = "your_xeno_canto_api_key",
  ALA_USERNAME = "your_ala_username",
  ALA_EMAIL = "your_ala_email",
  ALA_PASSWORD = "your_ala_password"
)

source("scripts/analyse_birdnet_output.R")
```

It uses `ggplot2` for all figures.
By default, the figures are shown in the active R graphics session and also saved as `.png` files.

## Outputs

### Per audio file

For each processed recording, the pipeline writes:

- `*_birdnet_predictions.csv`
- `*_birdnet_species_summary.csv`

The summary CSV contains:

- `date_time`
- `scientific_name`
- `common_name`
- `confidence`
- `cumulative_number_of_new_species_detected`
- `total_number_of_species_identified`

### Archive-level outputs

For source-processing runs, output is written under:

```text
out/<source_name>_birdnet_output/
```

The source-processing workflow writes:

- `*_processing_manifest.csv`  
  machine-readable log of file-by-file outcomes

- `*_file_results.txt`  
  continually updated text summary by file, including status, timing, coordinates, outputs, and any errors

- `*_summary_of_summaries.txt`  
  continually updated overall run summary, including progress, current file, current phase, elapsed time, ETA, and cumulative species count

All source-processing outputs are written to the local repository drive, not back to the source archive drive or remote EcoSounds repository.
In EcoSounds mode, the workflow does **not** build up a local cache of all recordings first; it downloads one source audio file, analyses it locally, deletes the temporary local audio, and only then moves to the next recording.

### Analysis outputs

Post-processing outputs are written under:

```text
out/analysis/confidence_<threshold>_bin_<minutes>min/
```

The analysis workflow writes:

- `birdnet_analysis_summary.txt`  
  text summary of the current analysis run, including skipped or incomplete input files

- `birdnet_analysis_input_files.csv`  
  one row per discovered summary CSV, showing whether it was loaded successfully

- `birdnet_analysis_filtered_detections.csv`  
  all retained detections after applying the chosen minimum confidence threshold, now including local light-phase classification where timestamps and coordinates can be recovered

- `birdnet_identifications_by_time_bin.csv`  
  identifications per time bin, plus unique species count per bin

- `birdnet_identifications_by_time_bin_by_recorder.csv`  
  identifications per time bin for each recorder separately

- `birdnet_light_phase_calendar.csv`  
  local civil-dawn, sunrise, sunset, and civil-dusk times by date/location, plus total daylight, twilight, and darkness hours

- `birdnet_light_phase_calendar_long.csv`  
  the same local light-phase calendar in long format, one row per date/location/phase

- `birdnet_light_phase_sampling_effort.csv`  
  total sampled recording hours falling within daylight, twilight, and darkness across all currently analysed recordings

- `birdnet_light_phase_sampling_effort_by_recorder.csv`  
  the same sampled light-phase effort summarized separately for each recorder

- `birdnet_diel_activity_by_species.csv`  
  per-species daylight, twilight, and darkness detections, normalized detections-per-hour, dominant light phase, and normalized night-versus-day rate ratio

- `birdnet_diel_activity_by_species_by_recorder.csv`  
  recorder-specific diel activity summaries by species using the same normalized light-phase metrics

- `birdnet_non_native_species_detected.csv`  
  detected species from the repository lookup that are non-native to Australia, including first/last detection times and which recorders detected them

- `birdnet_non_native_species_detections_through_time.csv`  
  time-binned detections through time for the detected non-native species across all recorders

- `birdnet_non_native_species_detections_through_time_by_recorder.csv`  
  time-binned detections through time for the detected non-native species calculated separately for each recorder

- `birdnet_top_10_species_detections_through_time.csv`  
  detections through time for the 10 most detected species across all currently analysed recorders

- `birdnet_top_10_species_detections_through_time_by_recorder.csv`  
  detections through time for the 10 most detected species within each recorder

- `birdnet_cumulative_new_species_by_time_bin.csv`  
  newly detected species per time bin and cumulative species richness through time

- `birdnet_cumulative_new_species_by_time_bin_by_recorder.csv`  
  cumulative new-species summaries calculated separately for each recorder

- `birdnet_identifications_by_species.csv`  
  species ranked from most frequently identified to least frequently identified

- `birdnet_identifications_by_species_by_recorder.csv`  
  species-frequency summaries calculated separately for each recorder

- `birdnet_identifications_by_species_by_month.csv`  
  species ranked by identification frequency within each month of the year

- `birdnet_identifications_by_species_by_month_by_recorder.csv`  
  month-by-species summaries calculated separately for each recorder

- `birdnet_species_composition_by_diversity_window_by_recorder.csv`  
  recorder-specific species-composition summaries across user-defined diversity windows, retaining the species that cumulatively cover 95% of identifications within each window and pooling the remaining <5% into an `other species (<5%)` category

- `birdnet_online_diel_sanity_check.csv`  
  species-level online diel sanity summary, including the source used (`xeno-canto` or `iNaturalist`), public-source record counts by light phase, check status, and counts of detections flagged or removed as improbable for their light phase

- `birdnet_online_diel_removed_detections.csv`  
  detection-level audit table listing each BirdNET detection removed by the online diel sanity filter, together with the matched source taxon and expected calling phases

- `birdnet_ala_species_sanity_check.csv`  
  species-level Atlas of Living Australia sanity-check summary, using IOC scientific names where a repository IOC crosswalk entry is available, retaining the current common names, and including nearby-record counts plus the resolved ALA query name/field/source used after any taxonomy reconciliation

- `birdnet_ala_removed_detections.csv`  
  detection-level audit table listing each BirdNET detection removed because its species failed the configured ALA minimum local-occurrence criterion

- `birdnet_ala_removed_species.csv`  
  species-level summary table listing the species removed by the ALA sanity filter, together with the number of detections removed for each species and the resolved ALA query details that triggered removal

- `birdnet_ala_occurrence_counts_by_recorder.csv`  
  raw recorder-by-species Atlas of Living Australia occurrence counts returned within the configured radius around each recorder reference location, including the resolved ALA query name/field/source used after any taxonomy reconciliation, plus any query status or error message

- `birdnet_monthly_diversity_metrics.csv`  
  recorder-level diversity metrics calculated from detections-as-abundance across user-defined diversity windows, including Shannon index, Simpson index, and Hill numbers for <em>q</em> = 1 and <em>q</em> = 2

- `birdnet_monthly_diversity_metrics_overall.csv`  
  diversity metrics after combining detections across all currently analysed recorders within each user-defined diversity window

- `birdnet_monthly_diversity_metrics_daily_incidence.csv`  
  recorder-level diversity metrics calculated from daily species incidence within each user-defined diversity window

- `birdnet_monthly_diversity_metrics_daily_incidence_overall.csv`  
  diversity metrics after combining daily species incidence across all currently analysed recorders within each user-defined diversity window

- `birdnet_raw_species_richness_by_diversity_window.csv`  
  raw species richness calculated as the number of unique species detected within each user-defined diversity window

- `birdnet_identification_acf.csv`  
  detection-count autocorrelation values by temporal lag, retained for backward compatibility

- `birdnet_identification_spectrum.csv`  
  detection-count spectral-density summary for inspecting periodicity, retained for backward compatibility

- `birdnet_identification_periodicity_by_recorder.csv`  
  recorder-specific detection-count temporal diagnostics used by the recorder comparison figure

- `birdnet_temporal_diagnostics.csv`  
  combined temporal diagnostics for both detections per bin and unique species identified per bin, including autocorrelation (ACF), partial autocorrelation (PACF), and spectral-density curves

- `birdnet_temporal_periodicity_tests.csv`  
  Ljung-Box test summaries at key lags for both detections per bin and unique species identified per bin

- `birdnet_temporal_spectral_peaks.csv`  
  ranked dominant spectral peaks, including the strongest candidate periods and their relative power

- `birdnet_temporal_diagnostics_by_recorder.csv`  
  recorder-specific ACF, PACF, and spectral-density values for both detections per bin and unique species identified per bin

- `birdnet_temporal_periodicity_tests_by_recorder.csv`  
  recorder-specific Ljung-Box test summaries at key lags

- `birdnet_temporal_spectral_peaks_by_recorder.csv`  
  recorder-specific dominant spectral peaks for easier interpretation of likely recurring periods

- `birdnet_identifications_over_time.png`
- `birdnet_identifications_over_time_linear.png`
- `birdnet_top_10_species_detections_through_time.png`
- `birdnet_diel_activity_by_species.png`
- `birdnet_day_night_calling_bias_by_species.png`
- `birdnet_cumulative_new_species.png`
- `birdnet_identifications_by_species.png`
- `birdnet_identifications_by_species_by_month.png`
- `birdnet_monthly_diversity_metrics.png`
- `birdnet_monthly_diversity_metrics_daily_incidence.png`
- `birdnet_raw_species_richness_by_diversity_window.png`
- `birdnet_identifications_over_time_recorder_comparison.png`
- `birdnet_cumulative_new_species_recorder_comparison.png`
- `birdnet_total_diversity_recorder_comparison.png`
- `birdnet_hill_q2_recorder_comparison.png`
- `birdnet_non_native_species_detections_through_time.png`
- `birdnet_periodicity.png`  
  overall temporal-diagnostics figure combining all recorders currently present in the analysis, with ACF, PACF, and spectral-density panels for detections and unique species

- `birdnet_top_10_species_detections_through_time_by_recorder.png`
- `birdnet_cumulative_new_species_by_recorder.png`

- `recorders/<RECORDER_ID>/...`  
  recorder-specific figures written for each recorder that currently has usable detections (for example `recorders/GEL_A/`). Crowded recorder-level figures such as detections through time, identifications by species, identifications by species by month, species composition by diversity window, diversity metrics, temporal periodicity diagnostics, and non-native-species time-series profiles are now written only here as separate recorder-specific figures rather than root-level multi-panel comparisons.

In the species-frequency plots, the identification axis is shown on a log<sub>10</sub> scale, and common names are displayed in lowercase except where proper nouns remain capitalised. Latin names are italicised in the species-axis labels.
The root-level analysis figures are the combined overall results across all recorders currently present in `out/`. A small set of simplified recorder-comparison plots is also written at the root level for direct cross-recorder comparison, while recorder-specific figures are written into the `recorders/` subdirectory as each recorder becomes available.
The detections-over-time plots now include two versions: a log<sub>10</sub>-scale plot with black bars and a red robust smoother controlled by `rolling_mean_window_days`, fitted only within contiguous data-available segments and excluding zero-detection bins from the trend fit, and a separate linear-scale plot with black bars only and no smoother. The linear-scale versions include low-alpha background bands showing local morning twilight (pink), daylight (yellow), evening twilight (pink), and night (dark blue).
Before downstream summaries and plots are built, detections can optionally be checked against public online bird-sound evidence. The script queries <a href="https://xeno-canto.org/">`xeno-canto`</a> first and falls back to <a href="https://www.inaturalist.org/">`iNaturalist`</a> sound observations only when `xeno-canto` has no matches for that species. If a species is marked as unlikely to call during the detected light phase (`daylight`, `twilight`, or `night`), the detection is written to the online removal-audit CSV and removed from the analysis when `diel_sanity_check_remove_improbable <- TRUE`.
Before the analysis plots are built, detected species can optionally be checked against Atlas of Living Australia occurrence records using the public ALA biocache API. This check searches for records within a user-defined radius (default 200 km) of the recorder reference locations, counts records against the ALA `species` field so subspecies observations remain attached to the parent species, and now resolves BirdNET taxonomy clashes against ALA accepted names plus any available IOC crosswalk/synonym entries already stored in the repository before deciding that a species has zero local records. The CSV and figure outputs use IOC scientific names where those crosswalk entries are available, while keeping the current common names in labels and tables. It flags species with no nearby records or only very small nearby record counts as potentially suspicious, and writes those flags to the ALA sanity-check CSV outputs. When `ala_sanity_check_remove_improbable <- TRUE`, detections for species failing that configured local-occurrence threshold are also written to an ALA removal-audit CSV, summarised in `birdnet_ala_removed_species.csv`, and removed before all later summaries and plots are generated, except when that filter would otherwise remove every remaining detection from the run.
The count-based diversity metrics treat the number of detections per species as the abundance proxy for Shannon, Simpson, and Hill-number calculations, and are produced across user-defined diversity windows set with `diversity_window_days`. A parallel daily-incidence diversity summary is also written, and raw species richness is summarized across those same diversity windows in a separate plot/table.
The top-species time-series plots default to 24-hour bins through `top_species_time_bin_minutes <- 24 × 60`, but that bin size can be changed directly in `scripts/analyse_birdnet_output.R`.
In the recorder-comparison diversity figure, each recorder-by-metric panel now uses its own y-axis range so Shannon, Simpson, and Hill-number panels are scaled to their local maxima.
The temporal periodicity figures now show both detections per time bin and unique species identified per time bin. Autocorrelation function (ACF) and partial autocorrelation function (PACF) panels include approximate type I error bands (<code>± 1.96/√<em>N</em></code>), spectral-density panels mark the strongest candidate periods, and the companion Ljung-Box CSV outputs provide a compact test summary at identified lags for easier interpretation of recurring temporal structure.
The diel activity analysis reconstructs local daylight, civil twilight, and darkness from each recording's timestamp and recovered coordinates, summarizes the sampled hours available in each light phase, and reports normalized detections-per-hour so day-versus-night comparisons are not biased by unequal photoperiod or recording effort.

### Diversity-metric calculations

For each recorder and each calendar month, the analysis script:

1. filters detections to those meeting `min_confidence`
2. groups the remaining detections by recorder and month
3. counts the number of detections for each species within that recorder-month
4. treats those species-level detection counts as the abundance vector
5. converts counts to relative abundance:

<code><em>p<sub>i</sub></em> = <em>n<sub>i</sub></em> / <em>N</em></code>

where:

- <code><em>n<sub>i</sub></em></code> = number of detections for species <code><em>i</em></code>
- <code><em>N</em></code> = total number of detections across all species in that recorder-month
- <code><em>p<sub>i</sub></em></code> = relative abundance of species <code><em>i</em></code>

The diversity metrics are then calculated as follows:

#### Shannon diversity index

<code><em>H</em><sup>'</sup> = -Σ(<em>p<sub>i</sub></em> log<sub><em>e</em></sub> <em>p<sub>i</sub></em>)</code>

#### Simpson diversity index

The script reports the Gini-Simpson form:

<code>1 - Σ (<em>p<sub>i</sub></em><sup>2</sup>)</code>

#### Hill number, <em>q</em> = 1

This is the exponential of Shannon diversity:

<code><sup>1</sup><em>D</em> = <em>e</em><sup>H'</sup></code>

#### Hill number, <em>q</em> = 2

This is the inverse Simpson concentration:


<code><sup>2</sup><em>D</em> = 1 / Σ <em>p<sub>i</sub></em><sup>2</sup>
</code>

Interpretation in this workflow:

- larger Shannon and Hill <em>q</em> = 1 indicate greater effective diversity with sensitivity to both common and less-common species
- larger Simpson and Hill <em>q</em>  = 2 indicate greater diversity with stronger weighting toward the most frequently detected species
- because the pipeline uses detections rather than direct counts of individuals, these are diversity estimates based on the assumption that detection frequency is a reasonable proxy for relative abundance

## Console progress during source-processing runs

When `Rscript scripts/process_download_pipeline.R` is running, the console reports:

- current file index and percent complete
- current per-file stage percent
- current source item being processed
- archive streaming progress or EcoSounds listing progress
- extraction/download step
- `.flac` to `.wav` conversion step
- BirdNET range-filter step
- BirdNET prediction step
- output-writing step
- cleanup step
- per-file elapsed time
- estimated time remaining

Archive streaming, EcoSounds downloads, extraction, and conversion stages are monitored through `processx`, so they emit recurring heartbeat updates instead of staying silent until the subprocess returns.

BirdNET analysis is also run in a monitored child R process through <code>callr</code>, so TensorFlow/TFLite warnings should no longer make the main console progress appear frozen.

## Resume behaviour

The source-processing script is restart-friendly.

If a file already has an existing BirdNET output pair in `out/` (`*_birdnet_species_summary.csv` plus the matching `*_birdnet_predictions.csv`), that file is skipped and logged as:

```text
skipped_existing
```

This allows rerunning the script after interruption without reprocessing every file, regardless of whether the source is a local archive or EcoSounds.
When switching between archive mode and EcoSounds mode, skip detection now scans the actual processed BirdNET CSV outputs already present under `out/` and matches recordings using origin-agnostic filename keys derived from those existing outputs, so recordings already processed from one source are skipped when encountered later from the other source even if the incoming archive/header path differs.
If an older output pair was written with the same recorder and wall-clock timestamp but a different Adelaide UTC offset in the filename stem, the skip logic now falls back to that recorder-specific local clock time before downloading the file again.
If `out/amalgamated_birdnet_output/` has been created with the optional clean-up script, those amalgamated recorder-level CSV pairs are preferred by the skip index when matching future archive or EcoSounds inputs.
In EcoSounds mode, the stable recording-ID path is still used for the local EcoSounds output tree itself, so already processed recordings are also skipped on rerun before any fresh download is attempted.

## Contingencies and failure behaviour

### 1. No coordinates in file name

The script uses fallback latitude/longitude if configured. If neither filename coordinates nor fallback coordinates are available, processing stops for that file.

### 2. No parseable date/time in file name

The script uses the fallback date if supplied. If not, processing stops for that file.

### 3. Empty species filter

If the repository species CSV and BirdNET range-model output do not overlap, processing stops for that file with an explicit error.

### 4. No usable detections

If BirdNET returns no usable detections after cleaning, the script writes empty summary outputs and records the file as:

- `no_usable_detections`

### 5. No detections pass the summary threshold

If BirdNET runs successfully but no predictions meet `summary_confidence_threshold`, the script writes empty summary outputs and records the file as:

- `no_summary_detections`

### 6. Conversion failure

If `ffmpeg` fails to convert a `.flac`, the file is recorded as:

- `error`

and processing continues to the next source item.

### 7. Archive extraction or EcoSounds download failure

If `tar --zstd` fails for a specific member, or an authenticated EcoSounds download fails for a specific recording, that file is recorded as:

- `error`

and processing continues.

### 8. Slow extraction from `.tar.zst` or slow remote downloads

This workflow extracts one archive member at a time in archive mode, and downloads one recording at a time in EcoSounds mode. For compressed `.tar.zst` archives, extraction can still be slow because `tar` may need to scan or decompress a large portion of the archive to reach a later member.

That means a file can legitimately spend a long time in the extraction/download stage even when it is not frozen. The script emits heartbeat updates during that stage so you can tell the process is still alive.

### 9. Slow BirdNET inference

BirdNET model startup and inference can also take a long time, especially on the first files of a run while Python/model dependencies initialize. The source-processing runner polls that stage from a child R process and keeps updating console and text progress during analysis.

### 10. Interrupted runs

If the process is interrupted, rerun:

```bash
Rscript scripts/process_download_pipeline.R
```

Files with existing BirdNET output CSV pairs skipped automatically.

## Notes

- temporary extracted `.flac` and converted `.wav` files deleted after each file is processed
- in EcoSounds mode, each downloaded recording is stored in a per-recording temporary workspace that is removed before the next download begins
- helper functions live in `scripts/birdnet_helpers.R`
- archive mode mirrors the archive subdirectory structure in the output folder when writing per-file CSV results
- EcoSounds mode writes outputs under stable recorder-based paths such as `GEL_A/<canonical_file_name>_...` so interrupted runs can resume cleanly and skip already processed recordings

<br>
<p><a href="https://www.flinders.edu.au"><img align="bottom-left" src="www/Flinders_University_Logo_Stacked_RGB_Master.jpg" alt="Flinders University" height="90" style="margin-top: 20px"></a> <a href="https://globalecologyflinders.com"><img align="bottom-left" src="www/GEL Logo Kaurna New Transp.png" alt="Global Ecology Laboratory" height="83" style="margin-top: 20px"></a> <a href="https://ciehf.au/"><img align="bottom-left" src="www/CIEHF_Logo_Email_Version.jpg" alt="CIEHF" height="72" style="margin-top: 20px"></a> &nbsp; &nbsp; <a href="http://ngarrindjeri.com.au/"><img align="bottom-left" src="www/NAClogo.png" alt="Ngarrindjeri Aboriginal Corporation" height="78" style="margin-top: 20px"></a></p>
