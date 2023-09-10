library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)

# Import data -------------------------------------------------------------

excel_files = c(
  "data/Station-1_2023-09-07_report.xlsx",
  "data/Station-1_2023-09-08_report.xlsx",
  "data/Station-1_2023-09-09_report.xlsx"
)
all_nights = NULL
for(excel_file in excel_files) {
  excel_table <- read_excel(excel_file, sheet = "Detailed")
  all_nights = bind_rows(all_nights, excel_table)
}

# Tidy data ---------------------------------------------------------------

all_nights_tidy <- all_nights |>
  # Automatic conversion to snake_case.
  janitor::clean_names() |>
  #  Names including kHz and dBFS was not handled properly.
  rename(
    peak_khz = peak_k_hz,
    peak_dbfs = peak_d_bfs,
    peak_dbfs = peak_d_bfs,
    sensitivity_dbfs = sensitivity_d_bfs,
  )

# Plot1 - all nights ------------------------------------------------------

plot1 <- all_nights_tidy |>
  # Filter.
  filter(quality != "Q0" & quality != "NA") |>
  # 
  mutate(lat_m = latitude_dd / 360.0 * 40000000.0) |> 
  mutate(long_m = longitude_dd / 360.0 * 40000000.0) |> 
  ggplot() +
  geom_point(aes(y = lat_m - mean(lat_m), x = as_datetime(paste(date, time)), colour = "Latitude (m)"))  +
  geom_point(aes(y = long_m- mean(long_m), x = as_datetime(paste(date, time)), colour = "Longitude (m)")) +
  labs(
    x = "Time",
    y = "Deviations (m)",
    colour = "Direction",
    title = "GPS deviations from mean value in meter",
    subtitle = "Example used in R course",
  )
plot1

# Plot2 - as boxplot ------------------------------------------------------

plot2 <- all_nights_tidy |>
  # Filter.
  filter(quality != "Q0" & quality != "NA") |>
  # 
  mutate(lat_m = latitude_dd / 360.0 * 40000000.0) |> 
  mutate(long_m = longitude_dd / 360.0 * 40000000.0) |> 
  ggplot() +
  geom_boxplot(aes(y = lat_m - mean(lat_m), x = "Latitude (m)"))  +
  geom_boxplot(aes(y = long_m- mean(long_m), x = "Longitude (m)")) +
  labs(
    x = "Direction",
    y = "Deviations (m)",
  )
plot2

# Plot3 - as violin plot --------------------------------------------------

plot3 <- all_nights_tidy |>
  # Filter.
  filter(quality != "Q0" & quality != "NA") |>
  # 
  mutate(lat_m = latitude_dd / 360.0 * 40000000.0) |> 
  mutate(long_m = longitude_dd / 360.0 * 40000000.0) |> 
  ggplot() +
  geom_violin(aes(y = lat_m - mean(lat_m), x = "Latitude (m)"))  +
  geom_violin(aes(y = long_m- mean(long_m), x = "Longitude (m)")) +
  labs(
    x = "Direction",
    y = "Deviations (m)",
  )
plot3

# Plot4 - single night ----------------------------------------------------

plot4 <- all_nights_tidy |>
  # Filter.
  filter(quality != "Q0" & quality != "NA") |>
  filter(monitoring_night == "Station-1_2023-09-07") |>
  # 
  mutate(lat_m = latitude_dd / 360.0 * 40000000.0) |> 
  mutate(long_m = longitude_dd / 360.0 * 40000000.0) |> 
  ggplot() +
  geom_point(aes(x = as_datetime(paste(date, time)), y = lat_m - mean(lat_m), colour = "Latitude (m)"))  +
  geom_point(aes(x = as_datetime(paste(date, time)), y = long_m- mean(long_m), colour = "Longitude (m)")) +
  labs(
    x = "Time (for Station-1_2023-09-07 only)",
    y = "Deviations (m)",
    colour = "Direction",
  )
plot4

# Plot5 - Longitude vs latitude -------------------------------------------

plot5 <- all_nights_tidy |>
  # Filter.
  filter(quality != "Q0" & quality != "NA") |>
  # 
  mutate(lat_m = latitude_dd / 360.0 * 40000000.0) |> 
  mutate(long_m = longitude_dd / 360.0 * 40000000.0) |> 
  ggplot() +
  geom_point(aes(x = long_m - mean(long_m), y = lat_m - mean(lat_m)))  +
  geom_smooth(aes(x = long_m - mean(long_m), y = lat_m - mean(lat_m))) +
  geom_smooth(method = "lm", colour = "#FF0000", aes(x = long_m - mean(long_m), y = lat_m - mean(lat_m))) +
  labs(
    x = "Longitude - deviations (m)",
    y = "Latitude -deviations (m)",
    caption = "Source: GitHub...",
  )
plot5

# All plots together ------------------------------------------------------

# Uses "patchwork" for layout.
plot1 / (plot2 | plot3) / plot4 / plot5

