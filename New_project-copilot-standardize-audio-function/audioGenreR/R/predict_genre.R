#' Predict the music genre of an audio file
#'
#' Standardizes the audio, extracts acoustic features, applies the PCA
#' transformation learned during training, and classifies the result with a
#' k-Nearest Neighbours model.
#'
#' A pre-trained model is bundled with the package in
#' \code{inst/models/genre_model.RData} and is used automatically when no
#' \code{model_path} is supplied.  To use your own model, pass its path
#' explicitly or train one with \code{\link{train_genre_model}}.
#'
#' @param audio_file Character string.  Path to the audio file to classify
#'   (any format supported by FFmpeg, e.g. MP3, WAV, FLAC, OGG).
#' @param model_path Character string.  Path to the \code{.RData} model file
#'   produced by \code{\link{train_genre_model}}.  Defaults to the pre-trained
#'   model bundled with the package
#'   (\code{system.file("models", "genre_model.RData", package = "audioGenreR")}).
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{genre}{Character string — the predicted genre label.}
#'     \item{confidence}{Numeric in \[0, 1\] — fraction of the \emph{k}
#'       nearest neighbours that voted for the predicted class.}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Use the bundled model (no model_path needed)
#'   result <- predict_genre("jazz.mp3")
#'   cat("Genre:", result$genre, " Confidence:", result$confidence, "\n")
#'
#'   # Or supply a custom model
#'   result <- predict_genre("jazz.mp3", model_path = "my_model.RData")
#' }
#'
#' @importFrom class knn
#' @export
predict_genre <- function(audio_file,
                          model_path = system.file("models", "genre_model.RData",
                                                   package = "audioGenreR")) {

  if (!nzchar(model_path) || !file.exists(model_path)) {
    stop(
      "No model found. Supply a model_path or place 'genre_model.RData' in ",
      "inst/models/ before building the package.",
      call. = FALSE
    )
  }

  # ── Load model artefacts ────────────────────────────────────────────────────
  env <- new.env(parent = emptyenv())
  load(model_path, envir = env)

  pca_model      <- env$pca_model
  x_scaled       <- env$x_scaled
  pca_data       <- env$pca_data
  y              <- env$y
  best_k         <- env$best_k
  num_components <- env$num_components
  feature_names  <- env$feature_names

  # ── Preprocess new audio ────────────────────────────────────────────────────
  wav_file <- standardize_audio(audio_file)
  features <- extract_audio_features(wav_file)

  # Ensure columns are in the same order as training data
  new_x <- features[, feature_names, drop = FALSE]

  # Apply training-set scaling parameters
  new_x_scaled <- scale(
    new_x,
    center = attr(x_scaled, "scaled:center"),
    scale  = attr(x_scaled, "scaled:scale")
  )

  # Project onto training PCA space
  new_pca <- predict(pca_model, new_x_scaled)
  new_pca <- new_pca[, seq_len(num_components), drop = FALSE]

  # ── kNN classification ──────────────────────────────────────────────────────
  pred <- class::knn(
    train = pca_data,
    test  = new_pca,
    cl    = y,
    k     = best_k,
    prob  = TRUE
  )

  return(list(
    genre      = as.character(pred),
    confidence = attr(pred, "prob")
  ))
}
