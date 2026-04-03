## audioGenreR — end-to-end demo
##
## Prerequisites:
##   install.packages(c("av", "tuneR", "seewave", "class"))
##   install.packages("audioGenreR")   # or devtools::load_all()
##
## You also need:
##   - A CSV file with audio features + a "label" column (music_dataset.csv)
##   - One or more audio files to classify (MP3, WAV, FLAC, …)

library(audioGenreR)

# ── Step 1: Train the model ──────────────────────────────────────────────────

# The CSV must have numeric feature columns followed by a "label" column.
# If you already have the RData file from a previous session, skip this step.

train_genre_model(
  data_path  = "music_dataset.csv",
  model_path = "genre_model.RData",
  best_k     = 4L
)

# ── Step 2: Predict genre for new audio files ────────────────────────────────

result <- predict_genre("jazz.mp3", model_path = "genre_model.RData")
cat("jazz.mp3   ->", result$genre, "(confidence:", round(result$confidence, 2), ")\n")

result <- predict_genre("rock.mp3", model_path = "genre_model.RData")
cat("rock.mp3   ->", result$genre, "(confidence:", round(result$confidence, 2), ")\n")

result <- predict_genre("pop.mp3", model_path = "genre_model.RData")
cat("pop.mp3    ->", result$genre, "(confidence:", round(result$confidence, 2), ")\n")

# ── Step 3: Evaluate on training data (sanity check) ────────────────────────

env <- new.env(parent = emptyenv())
load("genre_model.RData", envir = env)

pred_train <- class::knn(
  train = env$pca_data,
  test  = env$pca_data,
  cl    = env$y,
  k     = env$best_k
)
cat("\nTraining accuracy:", round(mean(pred_train == env$y), 3), "\n")
