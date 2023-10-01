# Used libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

# Import data -------------------------------------------------------------

# night <- read_excel("data-in/Station-1_2023-09-07_report.xlsx",
#                     sheet="Detailed")
night <- read_excel("data-in/Station-1_2023-09-08_report.xlsx",
                    sheet="Detailed")
# night <- read_excel("data-in/Station-1_2023-09-09_report.xlsx",
#                     sheet="Detailed")
# Alternative with dialog.
# night <- read_excel(file.choose(), 2) # 2=second sheet, in this case "Detailed".

# Tidy data ---------------------------------------------------------------

night_tidy <- night |>

  # Automatic conversion to snake_case.
  janitor::clean_names() |>

  #  Names including kHz and dBFS was not handled properly.
  rename(
    peak_khz = peak_k_hz,
    peak_dbfs = peak_d_bfs,
    peak_dbfs = peak_d_bfs,
    sensitivity_dbfs = sensitivity_d_bfs,
  ) |>

  # Reduce the number of used variables.
  select(
    monitoring_night,
    date,
    time,
    quality,
    tags,
    comments,
    peak_khz,
    peak_dbfs,
    latitude_dd,
    longitude_dd,
  ) |>

  # Filter.
  filter(quality != "Q0" & quality != "NA")

# Transform ---------------------------------------------------------------

night_transformed <- night_tidy |>

  # Add a new variable named date_time.
  mutate(date_time = as_datetime(paste(date, time)))

# Visualize ---------------------------------------------------------------

plot1 <- night_transformed |>

  # It is possible to filter more here.
  # filter(peak_dbfs >= -30.0) |>

  # Create plot.
  ggplot() +
  # X and Y axes. Use colours to separate values in "tags".
  geom_point(aes(x=date_time, y=peak_khz, color=tags)) +

  # Fix the scales on X and Y axes.
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H") +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 100), n.breaks=10) +

  # Set titles and labels.
  labs(
    x = "Time (h)",
    y = "Peak frequency (kHz)",
    colour = "Tags",
    title = "Peak values in recorded sound files",
    subtitle = paste("Monitorings night:",
                     distinct(night_transformed, monitoring_night),
                     "-",
                     nrow(night_tidy),
                     "files"),
  )

# Create the plot.
plot1

# Communicate -------------------------------------------------------------

# Create "data-out" if it doesn't exist.
if ( !dir.exists("data-out") ) {
  dir.create("data-out")
}

# Save the plot.
file_name <- paste(distinct(night_transformed, monitoring_night),
                   "-overview",
                   ".png",
                   sep = "")
file_path <- file.path("data-out", file_name)

ggsave(filename = file_path,
       plot=plot1)
