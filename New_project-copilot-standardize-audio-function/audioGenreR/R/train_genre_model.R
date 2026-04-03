#' Train a music genre classifier and save the model
#'
#' Reads a CSV dataset where the last column is the genre label, scales
#' features, reduces dimensionality with PCA (retaining 95 \% of variance),
#' and stores all training artefacts in an \code{.RData} file so that
#' \code{\link{predict_genre}} can load them later.
#'
#' @param data_path Character string.  Path to a CSV file with feature columns
#'   followed by a \code{label} column containing the genre strings.
#' @param model_path Character string.  Path where the \code{.RData} model
#'   file will be written.  Defaults to \code{"genre_model.RData"} in the
#'   current working directory.
#' @param best_k Integer.  Number of nearest neighbours for the kNN classifier.
#'   Defaults to \code{4}.
#'
#' @return Invisibly returns a named list with the saved objects:
#'   \code{pca_model}, \code{x_scaled}, \code{pca_data}, \code{y},
#'   \code{best_k}, \code{num_components}, \code{feature_names}.
#'
#' @examples
#' \dontrun{
#'   train_genre_model(
#'     data_path  = "music_dataset.csv",
#'     model_path = "genre_model.RData",
#'     best_k     = 4
#'   )
#' }
#'
#' @export
train_genre_model <- function(data_path,
                              model_path = "genre_model.RData",
                              best_k     = 4L) {

  data <- read.csv(data_path, stringsAsFactors = FALSE)

  # Last column is the label
  x <- data[, -ncol(data)]
  y <- data[, ncol(data)]

  feature_names <- colnames(x)

  # Scale features (store centering/scaling parameters for later use)
  x_scaled <- scale(x)

  # PCA
  pca_model <- prcomp(x_scaled)

  # Keep enough components to explain >= 95 % of variance
  variance       <- cumsum(pca_model$sdev^2 / sum(pca_model$sdev^2))
  num_components <- which(variance >= 0.95)[1]

  pca_data <- pca_model$x[, seq_len(num_components), drop = FALSE]

  best_k <- as.integer(best_k)

  save(
    pca_model,
    x_scaled,
    pca_data,
    y,
    best_k,
    num_components,
    feature_names,
    file = model_path
  )

  message("Model trained and saved to: ", model_path)

  invisible(
    list(
      pca_model      = pca_model,
      x_scaled       = x_scaled,
      pca_data       = pca_data,
      y              = y,
      best_k         = best_k,
      num_components = num_components,
      feature_names  = feature_names
    )
  )
}
