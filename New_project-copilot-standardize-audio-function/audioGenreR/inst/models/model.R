## audioGenreR — model training script
##
## This script trains a PCA + k-Nearest Neighbours genre classifier from a
## pre-built feature CSV and saves the resulting model artefacts to an .RData
## file that predict_genre() can load at prediction time.
##
## Usage (from the package root or any working directory):
##
##   source(system.file("models", "model.R", package = "audioGenreR"))
##
## Or run it directly:
##
##   Rscript inst/models/model.R \
##       --data  music_dataset.csv \
##       --model genre_model.RData \
##       --k     4
##
## The CSV must contain numeric feature columns followed by a "label" column
## holding the genre string for each row.  The column names must match those
## produced by extract_audio_features().

# ── Argument handling (supports both interactive and Rscript invocation) ──────

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default) {
  idx <- which(args == flag)
  if (length(idx) == 1L && length(args) >= idx + 1L) args[idx + 1L] else default
}

data_path  <- get_arg("--data",  "music_dataset.csv")
model_path <- get_arg("--model", "genre_model.RData")
best_k     <- as.integer(get_arg("--k", "4"))

# ── Load the package ──────────────────────────────────────────────────────────

if (!requireNamespace("audioGenreR", quietly = TRUE)) {
  # Allow running via devtools::load_all() or source() without installing
  pkgload_available <- requireNamespace("pkgload", quietly = TRUE)
  if (pkgload_available) {
    pkgload::load_all(
      system.file(package = "audioGenreR"),
      quiet = TRUE
    )
  } else {
    stop(
      "Package 'audioGenreR' is not installed. ",
      "Install it with: install.packages('audioGenreR')",
      call. = FALSE
    )
  }
}

library(audioGenreR)

# ── Validate inputs ───────────────────────────────────────────────────────────

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path, call. = FALSE)
}
if (!is.integer(best_k) || best_k < 1L) {
  stop("'--k' must be a positive integer.", call. = FALSE)
}

# ── Train and save ────────────────────────────────────────────────────────────

message("Training audioGenreR model ...")
message("  Data  : ", data_path)
message("  Output: ", model_path)
message("  k     : ", best_k)

model_objects <- train_genre_model(
  data_path  = data_path,
  model_path = model_path,
  best_k     = best_k
)

# ── Quick summary ─────────────────────────────────────────────────────────────

message("\nModel summary:")
message("  PCA components retained : ", model_objects$num_components)
message("  Training samples        : ", nrow(model_objects$pca_data))
message("  Genres                  : ",
        paste(sort(unique(model_objects$y)), collapse = ", "))
message("  Model saved to          : ", model_path)
