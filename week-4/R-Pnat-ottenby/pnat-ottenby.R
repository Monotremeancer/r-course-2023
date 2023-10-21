library(tidyverse)
library(readxl)

# Weather data ------------------------------------------------------------

weather_data <- read_excel("data-in/ottenby_2022_weather.xlsx")

weather_by_day <- weather_data |>
  group_by(date) |>
  summarise(
    air_temp_mean = mean(air_temp_c),
    precipitation_sum = sum(precipitation),
    wind_speed_max = max(wind_speed)
  ) |>
  # To get rid of one outlier at 80 mm.
  filter(precipitation_sum <= 60.0)

# Bats --------------------------------------------------------------------

bats_data <- read_excel("data-in/ottenby_2022_tidy.xlsx")

# # Check which taxa is used.
# unique(bats_data$taxa_ok)
# # Example of combining taxa.
# bats_data <- bats_data |>
#   mutate(taxa_ok = recode(taxa_ok,
#                           "Mdau" = "Myotis",
#                           "M.sp" = "Myotis"))

list_of_taxa <- c("Pnat")
# list_of_taxa <- c("Nnoc", "Pnat", "Ppyg")

bats_by_day <- bats_data |>
  filter(taxa_ok %in% list_of_taxa) |>
  select(date, taxa_ok, number_of_ind) |>
  group_by(date, taxa_ok) |>
  summarise(observations = sum(number_of_ind)) |>
  # IMPORTANT: Converts from row format (narrow) to column format (wide).
  spread(taxa_ok, observations)

# Combine bats and weather data. Both in column format.
bats_vs_weather <- full_join(bats_by_day, weather_by_day)

# Visualise bat activites with weather data.
pairs(bats_vs_weather)


# If the result should be saved as PDF.
pdf(file = "ottenby_2022_Pnat_vs_weather.pdf")
pairs(bats_vs_weather)
dev.off()
