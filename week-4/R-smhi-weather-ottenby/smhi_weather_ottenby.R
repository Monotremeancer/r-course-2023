library(tidyverse)
library(writexl)
library(patchwork)

# Weather data near Ottenby station.
# SMHI station: "Ölands södra udde A".
# License: CC-BY.
# Data source: SMHI.
# https://www.smhi.se/data/meteorologi/ladda-ner-meteorologiska-observationer
# Note: Data for 2022 was manually extracted from downloaded data.

# Load data. All data sets are based on hours.
smhi_temp_in <-
  read_delim("data-in/smhi-opendata_1_66110_2022_temp.csv",
             delim = ";",
             skip = 10)
smhi_wind_in <-
  read_delim(
    "data-in/smhi-opendata_3_4_66110_2022_wind.csv",
    delim = ";",
    skip = 10
  )
smhi_humidity_in <-
  read_delim(
    "data-in/smhi-opendata_6_66110_2022_humidity.csv",
    delim = ";",
    skip = 10
  )
smhi_rain_in <-
  read_delim("data-in/smhi-opendata_7_66110_2022_rain.csv",
             delim = ";",
             skip = 10)
smhi_pressure_in <-
  read_delim(
    "data-in/smhi-opendata_9_66110_2022_pressure.csv",
    delim = ";",
    skip = 10
  )
smhi_fog_in <-
  read_delim("data-in/smhi-opendata_12_66110_2022_fog.csv",
             delim = ";",
             skip = 10)

# Tidy headers and columns. Add date_time.
smhi_temp <- smhi_temp_in |>
  select(-Kvalitet) |>
  rename(date = "Datum",
         time = "Tid (UTC)",
         air_temp_c = "Lufttemperatur") |>
  mutate(date_time = as_datetime(paste(date, time)))

smhi_wind <- smhi_wind_in |>
  select(-Kvalitet...4,-Kvalitet...6) |>
  rename(
    date = "Datum",
    time = "Tid (UTC)",
    wind_direction = "Vindriktning",
    wind_speed = "Vindhastighet"
  ) |>
  mutate(date_time = as_datetime(paste(date, time)))

smhi_humidity <- smhi_humidity_in |>
  select(-Kvalitet) |>
  rename(date = "Datum",
         time = "Tid (UTC)",
         humidity = "Relativ Luftfuktighet") |>
  mutate(date_time = as_datetime(paste(date, time)))

smhi_rain <- smhi_rain_in |>
  select(-Kvalitet) |>
  rename(date = "Datum",
         time = "Tid (UTC)",
         precipitation = "Nederbördsmängd") |>
  mutate(date_time = as_datetime(paste(date, time)))

smhi_pressure <- smhi_pressure_in |>
  select(-Kvalitet) |>
  rename(date = "Datum",
         time = "Tid (UTC)",
         air_pressure = "Lufttryck reducerat havsytans nivå") |>
  mutate(date_time = as_datetime(paste(date, time)))

smhi_fog <- smhi_fog_in |>
  select(-Kvalitet) |>
  rename(date = "Datum",
         time = "Tid (UTC)",
         free_sight = "Sikt") |>
  mutate(date_time = as_datetime(paste(date, time)))

# Remove from "Environment" list. They are not used any longer.
rm(
  smhi_temp_in,
  smhi_wind_in,
  smhi_humidity_in,
  smhi_rain_in,
  smhi_pressure_in,
  smhi_fog_in
)

# Create an array with 365*24 hours for 2022.
date_time <- seq(as_datetime("2022-01-01 00:00:00"),
                 by = "1 hour",
                 length.out = 365 * 24)

# Create data table.
weather_data <- tibble(date_time)

# Add columns with weather data.
weather_data <- weather_data |>
  left_join(smhi_temp) |>
  left_join(smhi_wind) |>
  left_join(smhi_humidity) |>
  left_join(smhi_rain) |>
  left_join(smhi_pressure) |>
  left_join(smhi_fog)

# Create directory 
dir_out = "data-out"
if (!dir.exists(dir_out)) {
  dir.create(dir_out)
}

# Save the data table as .xlsx.
write_xlsx(weather_data, file.path(dir_out, "ottenby_2022_weather.xlsx"))

# Visualise the data. Don't execute them now.
temp_plot <- weather_data |>
  ggplot() +
  geom_point(aes(x = date_time, y = air_temp_c), size=0.1)

wind_plot <- weather_data |>
  ggplot() +
  geom_point(aes(x = date_time, y = wind_speed), size=0.1)

humidity_plot <- weather_data |>
  ggplot() +
  geom_point(aes(x = date_time, y = humidity), size=0.1)

rain_plot <- weather_data |>
  ggplot() +
  geom_point(aes(x = date_time, y = precipitation), size=0.1)

pressure_plot <- weather_data |>
  ggplot() +
  geom_point(aes(x = date_time, y = air_pressure), size=0.1)

fog_plot <- weather_data |>
  ggplot() +
  geom_point(aes(x = date_time, y = free_sight), size=0.1)

# Execute the plots.
temp_plot
wind_plot
humidity_plot
rain_plot
pressure_plot
fog_plot

# Check if rain is better with a logarithmic y-axis. Not saved.
weather_data |>
  # Filter since log scales does not like zero values.
  filter(precipitation > 0.1) |>
  ggplot() +
  geom_point(aes(x = air_pressure, y = precipitation), size=0.5) +
  scale_y_log10() 

# Save as PDF. Activate printing device.
pdf(file = "ottenby_2022_weather.pdf",
    width = 25, 
    height = 25)

# Use library "patchwork" for organising the plots.
temp_plot / wind_plot / humidity_plot / rain_plot / pressure_plot / fog_plot
  
# Finish plotting. Deactivate printing device.
dev.off()


