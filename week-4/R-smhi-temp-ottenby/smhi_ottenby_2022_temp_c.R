library(tidyverse)

# Temperature data near Ottenby station.
# SMHI station: "Ölands södra udde A".
# License: CC-BY.
# Data source: SMHI.
# https://www.smhi.se/data/meteorologi/ladda-ner-meteorologiska-observationer
# Note: Data for 2022 was manually extracted from downloaded data.

# If pdf_file_name is empty then run the script row-by-row, 
# otherwise all diagrams will be stored in a PDF document.
pdf_file_name = ""
# pdf_file_name = "demo-temperature-data.pdf"

# Load data.
smhi_ottenby_2022_temp <-
  read_delim("data-in/smhi-opendata_1_66110_2022_temp.csv",
             delim = ";",
             skip = 10)

# Tidy headers.
smhi_temp_tidy <- smhi_ottenby_2022_temp |>
  rename(
    date = "Datum",
    time = "Tid (UTC)",
    air_temp_c = "Lufttemperatur",
    quality = "Kvalitet"
  )

# Create a combined date/time column.
smhi_temp <- smhi_temp_tidy |>
  mutate(date_time = as_datetime(paste(date, time)))

# Only "smhi_temp" will be used. Remove others.
rm(smhi_ottenby_2022_temp, smhi_temp_tidy)

# Extract one month (August) as an example.
smhi_temp_august <- smhi_temp |>
  filter(month(date_time) == 8)

# Extract summer month as an example.
smhi_temp_summer <- smhi_temp |>
  filter(month(date_time) %in% c(6, 7, 8))

# Check if PDF should be created.
if (str_length(pdf_file_name) != 0) {
  # Turn printing device on.
  pdf(file = pdf_file_name)
}

# Plot by hour, 2022.
smhi_temp |>
  ggplot() +
  geom_point(aes(x = date_time, y = air_temp_c), size = 0.1)

# Plot by hour, with GAM.
smhi_temp |>
  ggplot() +
  geom_point(aes(x = date_time, y = air_temp_c), size = 0.1) +
  geom_smooth(aes(x = date_time, y = air_temp_c), method = "gam")

# Plot by hour, summer 2022, with GAM.
smhi_temp_summer |>
  ggplot() +
  geom_line(aes(x = date_time, y = air_temp_c), size = 0.1) +
  geom_smooth(aes(x = date_time, y = air_temp_c), method = "gam")

# Plot by hour, August 2022, with GAM.
smhi_temp_august |>
  ggplot() +
  geom_line(aes(x = date_time, y = air_temp_c), size = 0.1) +
  geom_smooth(aes(x = date_time, y = air_temp_c), method = "gam")

# Plot min max.
smhi_temp |>
  group_by(date) |>
  summarize(min_temp_c = min(air_temp_c),
            max_temp_c = max(air_temp_c)) |>
  ggplot() +
  geom_point(aes(x = date, y = min_temp_c, col = "Min"), size = 0.5) +
  geom_point(aes(x = date, y = max_temp_c, col = "Max"), size = 0.5)

# Plot min max.
smhi_temp |>
  group_by(date) |>
  summarize(min_temp_c = min(air_temp_c),
            max_temp_c = max(air_temp_c)) |>
  ggplot() +
  geom_linerange(aes(x = date, ymin = min_temp_c, ymax = max_temp_c), size = 0.1)

# Plot min-mean-max. GAM. 2022.
smhi_temp |>
  group_by(date) |>
  summarise(
    min_temp_c = min(air_temp_c),
    max_temp_c = max(air_temp_c),
    mean_temp_c = mean(air_temp_c)
  ) |>
  ggplot() +
  geom_linerange(aes(x = date, ymin = min_temp_c, ymax = max_temp_c), size = 0.1) +
  geom_line(aes(x = date, y = mean_temp_c), size = 0.2) +
  geom_smooth(aes(x = date, y = mean_temp_c, color = "Mean"), method = "gam") +
  geom_smooth(aes(x = date, y = min_temp_c, color = "Min"), method = "gam") +
  geom_smooth(aes(x = date, y = max_temp_c, color = "Max"), method = "gam")

# Plot min-mean-max. GAM. Summer 2022.
smhi_temp_summer |>
  group_by(date) |>
  summarise(
    min_temp_c = min(air_temp_c),
    max_temp_c = max(air_temp_c),
    mean_temp_c = mean(air_temp_c)
  ) |>
  ggplot() +
  geom_linerange(aes(x = date, ymin = min_temp_c, ymax = max_temp_c), size = 0.1) +
  geom_line(aes(x = date, y = mean_temp_c), size = 0.2) +
  geom_smooth(aes(x = date, y = mean_temp_c, col = "Mean"), method = "gam") +
  geom_smooth(aes(x = date, y = min_temp_c, col = "Min"), method = "gam") +
  geom_smooth(aes(x = date, y = max_temp_c, col = "Max"), method = "gam")

# Plot min-mean-max. GAM. August 2022, geom_pointrange.
smhi_temp_august |>
  group_by(date) |>
  summarise(
    min_temp_c = min(air_temp_c),
    max_temp_c = max(air_temp_c),
    mean_temp_c = mean(air_temp_c)
  ) |>
  ggplot() +
  geom_pointrange(aes(
    x = date,
    y = mean_temp_c,
    ymin = min_temp_c,
    ymax = max_temp_c
  )) +
  # geom_linerange(aes(x = date, ymin = min_temp_c, ymax = max_temp_c), size = 0.1) +
  # geom_line(aes(x = date, y = mean_temp_c)) +
  geom_smooth(aes(x = date, y = mean_temp_c, col = "Mean"), method = "gam") +
  geom_smooth(aes(x = date, y = min_temp_c, col = "Min"), method = "gam") +
  geom_smooth(aes(x = date, y = max_temp_c, col = "Max"), method = "gam")

if (str_length(pdf_file_name) != 0) {
  # Turn printing device off. PDF will be automatically saved.
  dev.off()
}
  
