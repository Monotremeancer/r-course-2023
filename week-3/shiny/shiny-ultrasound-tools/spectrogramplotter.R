library(audio)

# Convert wave file to stft -----------------------------------------------

wave_to_stft <- function(wave_file_path,
                         from_sec = 0.0,
                         to_sec = NULL,
                         window_size = 1024,
                         overlap = 0.50) {
  wave <- audio::load.wave(wave_file_path)
  
  # Get content from the Wave object.
  sampling_freq <- wave$rate
  # Convert if stored as Time Expansion, TE.
  if (sampling_freq < 90000) {
    sampling_freq <- as.integer(sampling_freq * 10)
  }
  # Assume that the signal is mono.
  signal_int <- wave
  signal_length <- length(signal_int)
  # Extract part.
  from_index <- from_sec * sampling_freq + 1
  to_index <- signal_length
  if (is.null(to_sec)) {
    to_sec <- signal_length / sampling_freq
  } else {
    to_index <- to_sec * sampling_freq
  }
  if ((from_index != 0) || (to_index != signal_length)) {
    signal_int <- signal_int[from_index:to_index]
    signal_length <- length(signal_int)
  }
  # Window function Kaiser is used. Beta 5 is similar to Hamming,
  # 6 is similar to Hann and 8.6 is similar to Blackman.
  window_function <- signal::kaiser(window_size, beta = 8.6)
  hop_size <- floor(window_size * (1 - overlap))
  number_of_time_slots <-
    floor((signal_length - window_size) / hop_size)
  
  # Create an empty STFT matrix.
  result_stft <- matrix(0, ncol = window_size / 2,
                        nrow = number_of_time_slots)
  
  # Iterate and do the FFT for each time slot.
  for (index in 1:number_of_time_slots) {
    # Extract the part of the signal to be used.
    start <- (index - 1) * hop_size + 1
    end <- start + window_size - 1
    part <- signal_int[start:end]
    # Apply the window function and do the FFT.
    part_w <- part * window_function
    ft <- fft(part_w)
    # Get amplitudes part from the complex numbers by using Mod()).
    mag <- Mod(ft[1:(window_size / 2)])
    # Store the magnitude spectrum in the STFT matrix
    result_stft[index, ] <- mag
  }
  
  stft_info <- list(
    "stft" = result_stft,
    "sampling_freq" = sampling_freq,
    "from_sec" = from_sec,
    "to_sec" = to_sec
  )
  return(stft_info)
}

# Plot stft ---------------------------------------------------------------

plot_stft <- function(stft_info,
                      min_freq_khz = 0.0,
                      max_freq_khz = NULL,
                      threshold_dbfs = -60,
                      greyscale = FALSE) {
  # Extract info.
  stft <- stft_info$stft
  sampling_freq <- stft_info$sampling_freq
  from_sec <- stft_info$from_sec
  to_sec <- stft_info$to_sec
  # Limits on y axes.
  ylim_min <- min_freq_khz
  nyquist <- sampling_freq / 2
  nyquist_khz <- nyquist / 1000
  ylim_max <- max_freq_khz
  if (is.null(max_freq_khz)) {
    ylim_max <- nyquist_khz
  }
  # Color schema.
  colors <- hcl.colors(64, "YlOrBr", rev = TRUE)
  if (greyscale) {
    colors <- hcl.colors(64, "Grays", rev = TRUE)
  }
  # Convert the STFT matrix to dBFS.
  stft_dbfs <- stft ^ 2
  stft_dbfs <- 10 * log(stft_dbfs, 10)
  # Check range.
  value_range <- range(stft_dbfs, finite = TRUE)
  min_z_value <- value_range[1]
  max_z_value <- value_range[2]
  # Remove strange low frequency values.
  stft_dbfs[, c(1, 2, 3)] <- threshold_dbfs
  
  # Plot axes.
  stft_size_x <- dim(stft_dbfs)[1]
  stft_size_y <- dim(stft_dbfs)[2]
  times <-
    seq(from = from_sec,
        to = to_sec,
        length.out = stft_size_x)
  # times <- seq(from = 0, to = 6, length.out = stft_size_x)
  freqs_khz <-
    seq(from = 0,
        to = nyquist_khz,
        length.out = stft_size_y)
  
  # Create image ------------------------------------------------------------
  
  # Set margins.
  par(mar = c(2, 4, 0.5, 1))
  
  # Image.
  graphics::image(
    x = times,
    y = freqs_khz,
    z = stft_dbfs,
    ylim = c(ylim_min, ylim_max),
    zlim = c(threshold_dbfs, max_z_value),
    col = colors,
    useRaster = TRUE,
    xlab = "Time (s)",
    ylab = "Frequency (kHz)"
  )
  
  # Add dotted lines for kHz.
  # lty: solid", "dashed", "dotted", "dotdash", "longdash", or "twodash".
  x_distance <- 0.1
  if ((to_sec - from_sec) < 1.0) {
    x_distance <- 0.01
  }
  abline(
    h = seq(ylim_min + 10, ylim_max, 10),
    v = seq(from_sec + x_distance, to_sec - x_distance, x_distance),
    lty = "dotted",
    col = "gray70"
  )
}
