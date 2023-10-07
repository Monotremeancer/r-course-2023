
library(easycsv)
# library(stringr)

source("ultrasound_library.R", local = TRUE)

# Select source directory. 
source_dir_path = choose_dir()

# Create target directory.
target_dir_path = file.path(source_dir_path, "target")
if(!dir.exists(target_dir_path)) {
  dir.create(target_dir_path)
}

# Get list of wave files.
file_names <- list.files(
  path=source_dir_path,
  # path=".",
  pattern="*.wav",
  full.names=FALSE,
  recursive=FALSE
  # recursive=TRUE
)

# Loop over list of file names.
for (file_name in file_names) {
  print(paste("Processing file: ", file_name))

  # Prepare directories and files.
  source_file_path <- file.path(source_dir_path, file_name)
  
  # First plot.
  print("- First plot.")
  plot_file_name <- str_replace(file_name, ".wav", ".png")
  plot_file_path <- file.path(target_dir_path, plot_file_name)
  
  stft_info_full <- convert_wave_to_stft(wave_file_path = source_file_path,
                                         window_size = 1024,
                                         overlap = 0.50)
  plot_stft(stft_info_full,
            plot_file_path = plot_file_path,
            max_freq_khz = 170,
            # greyscale = TRUE,
  )
  
  # Second plot. From 2.0 to 2.2 sec.
  print("- Second plot.")
  plot_file_name <- str_replace(file_name, ".wav", "_PART.png")
  plot_file_path <- file.path(target_dir_path, plot_file_name)
  
  stft_info_part <- convert_wave_to_stft(wave_file_path = source_file_path,
                                         from_sec = 2.0,
                                         to_sec = 2.2,
                                         window_size = 512,
                                         overlap = 0.90)
  plot_stft(stft_info_part,
            plot_file_path = plot_file_path,
            # greyscale = TRUE,
  )
  
  # Also create TE from FS, or copy if already as TE.
  print("- Wave file as TE.")
  te_file_name <- str_replace(file_name, "_FS", "_TE")
  te_file_path <- file.path(target_dir_path, te_file_name)
    
  if (!str_detect(file_name, "_TE")) {
    convert_to_time_expansion(file_path_in = source_file_path,
                              file_path_out = te_file_path)
  } else {
    file.copy(source_file_path, te_file_path)
  }
}

