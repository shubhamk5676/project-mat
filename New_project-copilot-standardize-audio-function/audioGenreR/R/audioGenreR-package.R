#' audioGenreR: Music Genre Prediction Using PCA and k-NN
#'
#' @description
#' Provides a complete pipeline for music genre classification:
#' \enumerate{
#'   \item \strong{Standardize} audio files to a common format with
#'         \code{\link{standardize_audio}}.
#'   \item \strong{Extract} acoustic features (RMS energy, zero-crossing rate,
#'         spectral centroid/bandwidth/roll-off, MFCC, tempo) with
#'         \code{\link{extract_audio_features}}.
#'   \item \strong{Train} a PCA + k-Nearest Neighbours classifier on a CSV
#'         dataset with \code{\link{train_genre_model}}.
#'   \item \strong{Predict} the genre of new audio files with
#'         \code{\link{predict_genre}}.
#' }
#'
#' @section Typical workflow:
#' \preformatted{
#'   library(audioGenreR)
#'
#'   # 1. Train (once)
#'   train_genre_model("music_dataset.csv", "genre_model.RData")
#'
#'   # 2. Predict
#'   result <- predict_genre("song.mp3")
#'   cat(result$genre, result$confidence)
#' }
#'
#' @docType package
#' @name audioGenreR-package
#' @aliases audioGenreR
"_PACKAGE"
