#' Standardize an audio file for feature extraction
#'
#' Converts any audio file to a mono, 22050 Hz WAV file capped at 30 seconds.
#' The conversion is performed via \code{av::av_audio_convert()} so any format
#' supported by FFmpeg (MP3, FLAC, OGG, WAV, …) is accepted.
#'
#' @param input_file Character string.  Path to the input audio file.
#'
#' @return Character string.  Path to the standardized temporary WAV file.
#'
#' @examples
#' \dontrun{
#'   wav_path <- standardize_audio("song.mp3")
#' }
#'
#' @importFrom av av_audio_convert
#' @importFrom tuneR readWave mono downsample extractWave writeWave
#' @export
standardize_audio <- function(input_file) {

  temp_file <- tempfile(fileext = ".wav")

  # Convert to WAV, suppressing av's verbose console output
  invisible(
    capture.output(
      capture.output(
        av::av_audio_convert(input_file, output = temp_file),
        type = "message"
      )
    )
  )

  audio <- tuneR::readWave(temp_file)

  # Convert to mono (use left channel)
  audio <- tuneR::mono(audio, "left")

  # Downsample to 22050 Hz
  audio <- tuneR::downsample(audio, 22050)

  # Trim to at most 30 seconds
  duration <- length(audio@left) / audio@samp.rate
  if (duration > 30) {
    audio <- tuneR::extractWave(audio, from = 0, to = 30, xunit = "time")
  }

  output_file <- paste0(tempfile(), ".wav")
  tuneR::writeWave(audio, output_file)

  return(output_file)
}
