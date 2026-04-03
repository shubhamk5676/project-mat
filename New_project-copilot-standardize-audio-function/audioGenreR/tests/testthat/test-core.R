test_that("extract_audio_features returns correct structure", {
  # Build a minimal synthetic WAV in a temp file so the test does not
  # require a real audio file on disk.
  skip_if_not_installed("tuneR")
  skip_if_not_installed("seewave")

  sr  <- 22050L
  dur <- 2L  # seconds
  t   <- seq(0, dur - 1 / sr, by = 1 / sr)
  pcm <- as.integer(sin(2 * pi * 440 * t) * 32767)

  wav <- tuneR::Wave(left = pcm, samp.rate = sr, bit = 16L)

  tmp <- tempfile(fileext = ".wav")
  tuneR::writeWave(wav, tmp)
  on.exit(unlink(tmp))

  feat <- extract_audio_features(tmp)

  # Must be a one-row data.frame with the 21 expected columns
  expect_s3_class(feat, "data.frame")
  expect_equal(nrow(feat), 1L)

  expected_cols <- c(
    "rms_mean", "rms_var",
    "spectral_centroid_mean", "spectral_centroid_var",
    "spectral_bandwidth_mean", "spectral_bandwidth_var",
    "rolloff_mean", "rolloff_var",
    "zero_crossing_rate_mean", "zero_crossing_rate_var",
    "tempo",
    paste0("mfcc", 1:5, "_mean"),
    paste0("mfcc", 1:5, "_var")
  )
  expect_true(all(expected_cols %in% colnames(feat)))

  # No NAs should remain
  expect_false(anyNA(feat))

  # RMS should be positive for a non-silent signal
  expect_gt(feat$rms_mean, 0)
})

test_that("train_genre_model writes an RData file with required objects", {
  skip_if_not_installed("tuneR")
  skip_if_not_installed("seewave")
  skip_if_not_installed("class")

  # Create a tiny synthetic dataset (3 genres, 6 samples each = 18 rows)
  set.seed(42)
  n_samples_per_genre <- 6L
  genres              <- rep(c("jazz", "rock", "pop"), each = n_samples_per_genre)

  cols <- c(
    "rms_mean", "rms_var",
    "spectral_centroid_mean", "spectral_centroid_var",
    "spectral_bandwidth_mean", "spectral_bandwidth_var",
    "rolloff_mean", "rolloff_var",
    "zero_crossing_rate_mean", "zero_crossing_rate_var",
    "tempo",
    paste0("mfcc", 1:5, "_mean"),
    paste0("mfcc", 1:5, "_var")
  )

  df        <- as.data.frame(matrix(runif(length(genres) * length(cols)),
                                    nrow = length(genres),
                                    ncol = length(cols)))
  colnames(df) <- cols
  df$label  <- genres

  csv_path   <- tempfile(fileext = ".csv")
  model_path <- tempfile(fileext = ".RData")
  write.csv(df, csv_path, row.names = FALSE)
  on.exit({ unlink(csv_path); unlink(model_path) }, add = TRUE)

  train_genre_model(csv_path, model_path, best_k = 3L)

  expect_true(file.exists(model_path))

  env <- new.env(parent = emptyenv())
  load(model_path, envir = env)

  expect_true(all(c("pca_model", "x_scaled", "pca_data",
                    "y", "best_k", "num_components",
                    "feature_names") %in% ls(env)))
  expect_equal(env$best_k, 3L)
})
