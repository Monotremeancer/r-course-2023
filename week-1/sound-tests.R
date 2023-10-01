library(tidyverse)
library(av)
library(tuneR)
library(seewave)
library(patchwork)

wave_te_path <- "data-in/Mnat-long-seq-TE.wav"
wave_path <- "data-out/Mnat-long-seq.wav"

# Convert to FS - Full Scan -----------------------------------------------

wave_file <- readWave(wave_te_path)
if (wave_file@samp.rate <= 90000) {
  wave_file@samp.rate <- as.integer(wave_file@samp.rate * 10)
}
writeWave(wave_file, wave_path, extensible = FALSE)

# Test "av" ---------------------------------------------------------------

wave_file <- readWave(wave_te_path)

plot(read_audio_fft(wave_path,
                    window = hanning(1024),
                    overlap = 0))

plot(read_audio_fft(wave_path,
                    start_time = 3,
                    end_time = 3.2,
                    window = blackman(512),
                    overlap = 0.90))

# plot(read_audio_fft(wave_path,
#                     start_time = 3,
#                     end_time = 3.15,
#                     window = blackman(512),
#                     overlap = 0.98),
#      dark = FALSE)

# Test "av" - video -------------------------------------------------------

# wave_file <- readWave(wave_path)
# # Note: 384000 / 48000 is 8 times TE, not 10 times. Wrong scales.
# wave_file@samp.rate <- as.integer(48000)
# writeWave(wave_file, "tmp.wav", extensible = TRUE)
# # Create video. Will take some time.
# av_spectrogram_video("tmp.wav",
#                      output = 'spectrogram-TEx8.mp4',
#                      width = 1280,
#                      height = 720,
#                      res = 144)

# Test seewave ------------------------------------------------------------

wave_file <- readWave(wave_path)

p1 <- ggspectro(wave_file, w1 = 1024, wn = "hanning", ovlp = 50) +
  geom_tile(aes(fill = amplitude)) +
  scale_fill_continuous(name="Amplitude\n(dBFS)\n",
                        limits=c(-70,0),
                        na.value="transparent",
                        low="yellow",
                        high="blue")
p1

interval_s = c(3.0, 3.20)
wave_file <- readWave(wave_path,
                 from = interval_s[1] * as.integer(wave_file@samp.rate),
                 to = interval_s[2] * as.integer(wave_file@samp.rate))

p2 <- ggspectro(wave_file, w1 = 512, wn = "blackman", ovlp = 90) +
  geom_tile(aes(fill = amplitude)) +
  scale_fill_continuous(name="Amplitude\n(dBFS)\n",
                        limits=c(-70,0),
                        na.value="transparent",
                        low="yellow",
                        high="blue")
p2

# Plot both by using patchwork.
p1 / p2
