library(tidyverse)
library(readxl)
library(janitor)

# Import data -------------------------------------------------------------

# night <- read_excel("data/Station-1_2023-09-07_report.xlsx", sheet="Detailed")
# night <- read_excel("data/Station-1_2023-09-08_report.xlsx", sheet="Detailed")
# night <- read_excel("data/Station-1_2023-09-09_report.xlsx", sheet="Detailed")
night <- read_excel(file.choose(), 2) # 2=second sheet.

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
    latitude_dd,
    longitude_dd,
  ) |>
  # Filter.
  filter(quality != "Q0" & quality != "NA") |>
  # Add a new variable named date_time.
  mutate(date_time = as_datetime(paste(date, time)))

# Visualise ---------------------------------------------------------------

plot1 = night_tidy |>
  # filter(tags == "FM" | tags == "FM-QCF") |>
  ggplot() +
  geom_point(aes(x=date_time, y=peak_khz, color=tags)) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H") +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 100), n.breaks=10) +
  labs(
    x = "Time",
    y = "Peak frequency (kHz)",
    colour = "Tags",
    title = "Peak values in recorded sound files",
    subtitle = paste("Monitorings night:", 
                     distinct(night_tidy, monitoring_night),
                     "-",
                     nrow(night_tidy), 
                     "files"),
  )

plot1

ggsave(filename = paste(distinct(night_tidy, monitoring_night), 
                        "-overview", 
                        ".png", 
                        sep = ""), 
       plot=plot1)
