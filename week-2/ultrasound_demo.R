
source("ultrasound_library.R", local = TRUE)

# This demo is using a wave file called "Mnat-long-seq.wav" located 
# in the "data" directory. Results are also stored in "data".


# Filter wave files -------------------------------------------------------

high_pass_filter(file_path_in = "data/Mnat-long-seq.wav",
                 frequency_khz = 50.0,
                 file_path_out = "data/Mnat-HIGH-PASS.wav")

low_pass_filter(file_path_in = "data/Mnat-long-seq.wav",
                frequency_khz = 50.0,
                file_path_out = "data/Mnat-LOW-PASS.wav")

band_pass_filter(file_path_in = "data/Mnat-long-seq.wav",
                 frequency_low_khz = 40.0,
                 frequency_high_khz = 60.0,
                 file_path_out = "data/Mnat-BAND-PASS.wav")

stop_band_filter(file_path_in = "data/Mnat-long-seq.wav",
                 frequency_low_khz = 40.0,
                 frequency_high_khz = 60.0,
                 file_path_out = "data/Mnat-STOP-BAND.wav")


# Convert between FS and TE -----------------------------------------------

convert_to_time_expansion(file_path_in = "data/Mnat-long-seq.wav",
                          file_path_out = "data/Mnat-TE.wav")

convert_to_full_scan(file_path_in = "data/Mnat-TE.wav",
                     file_path_out = "data/Mnat-FS.wav")


# Create and plot spectrograms as PNG, JPEG and PDF. ----------------------

# From 0 to 6.0 sec.
stft_info_full <- convert_wave_to_stft(wave_file_path = "data/Mnat-long-seq.wav",
# stft_info_full <- convert_wave_to_stft(wave_file_path = "data/Mnat-BAND-PASS.wav",
                               window_size = 1024,
                               overlap = 0.50)
plot_stft(stft_info_full,
          plot_file_path = "data/Mnat.png",
          max_freq_khz = 170,
          # greyscale = TRUE,
)
plot_stft(stft_info_full,
          plot_file_path = "data/Mnat.pdf",
          max_freq_khz = 170,
          # greyscale = TRUE,
)
plot_stft(stft_info_full,
          plot_file_path = "data/Mnat.jpeg",
          max_freq_khz = 170,
          # greyscale = TRUE,
)

# From 3.2 to 3.4 sec.
stft_info_part <- convert_wave_to_stft(wave_file_path = "data/Mnat-long-seq.wav",
                                  from_sec = 3.2,
                                  to_sec = 3.4,
                                  window_size = 512,
                                  overlap = 0.95)
plot_stft(stft_info_part,
          plot_file_path = "data/Mnat-PART-BW.png",
          greyscale = TRUE,
)
plot_stft(stft_info_part,
          plot_file_path = "data/Mnat-PART.png",
          max_freq_khz = 170,
          # greyscale = TRUE,
)
plot_stft(stft_info_part,
          plot_file_path = "data/Mnat-PART.pdf",
          max_freq_khz = 170,
          # greyscale = TRUE,
)
plot_stft(stft_info_part,
          plot_file_path = "data/Mnat-PART.jpeg",
          max_freq_khz = 170,
          # greyscale = TRUE,
)

