#' Extract acoustic features from a WAV audio file
#'
#' Computes a set of 21 acoustic features from a standardized WAV file:
#' RMS energy (mean & variance), zero-crossing rate (mean & variance),
#' spectral centroid (mean & variance), spectral bandwidth (mean & variance),
#' spectral roll-off (mean & variance), tempo approximation, and 5 MFCC
#' coefficients (mean & variance each).
#'
#' @param audio_file Character string.  Path to a mono 22050 Hz WAV file,
#'   typically produced by \code{\link{standardize_audio}}.
#'
#' @return A one-row \code{data.frame} with the following 21 columns:
#'   \describe{
#'     \item{rms_mean, rms_var}{RMS energy statistics.}
#'     \item{spectral_centroid_mean, spectral_centroid_var}{Spectral centroid.}
#'     \item{spectral_bandwidth_mean, spectral_bandwidth_var}{Spectral bandwidth.}
#'     \item{rolloff_mean, rolloff_var}{Spectral roll-off (85 \% threshold).}
#'     \item{zero_crossing_rate_mean, zero_crossing_rate_var}{Zero-crossing rate.}
#'     \item{tempo}{Mean fundamental frequency used as a tempo proxy.}
#'     \item{mfcc1_mean \ldots mfcc5_mean}{Per-coefficient MFCC means.}
#'     \item{mfcc1_var \ldots mfcc5_var}{Per-coefficient MFCC variances.}
#'   }
#'
#' @examples
#' \dontrun{
#'   wav_path  <- standardize_audio("song.mp3")
#'   feat      <- extract_audio_features(wav_path)
#'   str(feat)
#' }
#'
#' @importFrom tuneR readWave melfcc
#' @importFrom seewave meanspec fund
#' @export
extract_audio_features <- function(audio_file) {

  audio  <- tuneR::readWave(audio_file)
  signal <- as.numeric(audio@left)
  sr     <- audio@samp.rate

  # Normalize signal to [-1, 1]
  max_abs <- max(abs(signal))
  if (max_abs > 0) {
    signal <- signal / max_abs
  }

  ################################
  # RMS ENERGY
  ################################
  rms_vals <- abs(signal)
  rms_mean <- mean(rms_vals)
  rms_var  <- var(rms_vals)

  ################################
  # ZERO CROSSING RATE
  ################################
  zcr_vals <- abs(diff(sign(signal))) / 2
  zcr_mean <- mean(zcr_vals)
  zcr_var  <- var(zcr_vals)

  ################################
  # MEAN POWER SPECTRUM
  ################################
  spec  <- seewave::meanspec(signal, f = sr, plot = FALSE)
  freqs <- spec[, 1]
  power <- spec[, 2] + 1e-10   # avoid exact zeros

  ################################
  # SPECTRAL CENTROID
  ################################
  spectral_centroid_mean <- sum(freqs * power) / sum(power)
  spectral_centroid_var  <- var(freqs * power)

  ################################
  # SPECTRAL BANDWIDTH
  ################################
  spectral_bandwidth_mean <- sqrt(
    sum(((freqs - spectral_centroid_mean)^2) * power) / sum(power)
  )
  spectral_bandwidth_var <- var((freqs - spectral_centroid_mean)^2 * power)

  ################################
  # SPECTRAL ROLL-OFF (85 %)
  ################################
  cumulative    <- cumsum(power)
  threshold     <- 0.85 * max(cumulative)
  rolloff_index <- which(cumulative >= threshold)[1]
  rolloff_mean  <- freqs[rolloff_index]
  rolloff_var   <- var(power)

  ################################
  # TEMPO (FUNDAMENTAL FREQUENCY)
  ################################
  tempo_vals <- seewave::fund(audio, plot = FALSE)
  tempo      <- mean(tempo_vals, na.rm = TRUE)
  if (is.na(tempo) || is.nan(tempo)) {
    tempo <- 0
  }

  ################################
  # MFCC (5 COEFFICIENTS)
  ################################
  mfcc_matrix <- tuneR::melfcc(
    audio,
    wintime = 0.025,
    hoptime = 0.01,
    numcep  = 5,
    preemph = 0.97
  )
  mfcc_mean <- colMeans(mfcc_matrix, na.rm = TRUE)
  mfcc_var  <- apply(mfcc_matrix, 2, var, na.rm = TRUE)

  ################################
  # COMBINE INTO A DATA FRAME
  ################################
  features <- data.frame(
    rms_mean                = rms_mean,
    rms_var                 = rms_var,
    spectral_centroid_mean  = spectral_centroid_mean,
    spectral_centroid_var   = spectral_centroid_var,
    spectral_bandwidth_mean = spectral_bandwidth_mean,
    spectral_bandwidth_var  = spectral_bandwidth_var,
    rolloff_mean            = rolloff_mean,
    rolloff_var             = rolloff_var,
    zero_crossing_rate_mean = zcr_mean,
    zero_crossing_rate_var  = zcr_var,
    tempo                   = tempo
  )

  for (i in seq_len(5)) {
    features[paste0("mfcc", i, "_mean")] <- mfcc_mean[i]
    features[paste0("mfcc", i, "_var")]  <- mfcc_var[i]
  }

  # Replace any remaining NAs with 0
  features[is.na(features)] <- 0

  return(features)
}
