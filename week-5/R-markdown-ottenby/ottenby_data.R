require(tidyverse)
require(readxl)
library(leaflet)
library(htmltools)

# Detector deployment -----------------------------------------------------

start_date <- as_date("2022-04-01")
end_date <- as_date("2022-12-01")
total_number_of_files = as.integer("22650")
# number_of_files_with_bats = as.integer("13938")
# first_bat_date <- as_date("2022-04-11")
# last_bat_date <- as_date("2022-11-01")

# Bats --------------------------------------------------------------------

bats_data <- read_excel("data-in/ottenby_2022_tidy.xlsx")

number_of_files_with_bats = nrow(bats_data)
first_bat_date <- date(min(bats_data$datetime))
last_bat_date <- date(max(bats_data$datetime))

bats_by_day_table <- bats_data |>
  # filter(taxa_ok %in% list_of_taxa) |>
  select(date, taxa_ok, number_of_ind) |>
  group_by(date, taxa_ok) |>
  summarise(observations = sum(number_of_ind)) |>
  spread(taxa_ok, observations)

bats_by_month_table <- bats_data |>
  # filter(taxa_ok %in% list_of_taxa) |>
  select(month, taxa_ok, number_of_ind) |>
  group_by(month, taxa_ok) |>
  summarise(observations = sum(number_of_ind)) |>
  spread(taxa_ok, observations)

bat_activity_by_day <- 
  bats_data |>
  group_by(date) |>
  summarize(observations = sum(number_of_ind)) |>
  ggplot(aes(x = date, y = observations)) +
  geom_line() +
  labs(x = "Datum", y = "Totalt antal registrerade fladdermöss",
       caption = "Källa: BatLife Sweden")
# bat_activity_by_day

# list_of_taxa <- c("Pnat")
list_of_taxa <- c("Nnoc", "Pnat", "Ppyg")
bats_by_day_table2 <- bats_data |>
  filter(taxa_ok %in% list_of_taxa) |>
  select(date, taxa_ok, number_of_ind) |>
  group_by(date, taxa_ok) |>
  summarise(observations = sum(number_of_ind)) |>
  # IMPORTANT: Converts from row format (narrow) to column format (wide).
  spread(taxa_ok, observations)


# Note: This one counts rows only, not "number_of_ind".
colorful_bats <- bats_data |>
  ggplot(aes(x = date, col = taxa_ok, fill = taxa_ok)) +
  geom_bar()
# colorful_bats

# Weather -----------------------------------------------------------------

weather_data <- read_excel("data-in/ottenby_2022_weather.xlsx")

weather_temperature <- weather_data |>
  group_by(date) |>
  summarise(
    min_temp_c = min(air_temp_c),
    max_temp_c = max(air_temp_c),
    mean_temp_c = mean(air_temp_c)
  ) |>
  ggplot() +
  geom_linerange(aes(x = date, ymin = min_temp_c, ymax = max_temp_c), linewidth = 0.1) +
  geom_line(aes(x = date, y = mean_temp_c), size = 0.2) +
  # geom_smooth(aes(x = date, y = min_temp_c, color = "Min"), method = "gam") +
  # geom_smooth(aes(x = date, y = max_temp_c, color = "Max"), method = "gam") +
  # geom_smooth(aes(x = date, y = mean_temp_c), method = "gam") +
  labs(x = "Datum", y = "Temperatur",
       caption = "Källa: SMHI")
# weather_temperature

# Map (leaflet) -----------------------------------------------------------

map_df <- data.frame(
  latitude = c(56.1977, 56.1965),
  longitude = c(16.4005, 16.399),
  description = c("SMHI: Ölands södra udde A", "BatLife Sweden: Ottenby")
)

map_ottenby_smhi <- leaflet(data = map_df) |> 
  addTiles() |>
  setView(16.4, 56.1977, zoom = 14) |>
  addMarkers(~longitude, ~latitude, popup = ~htmlEscape(description))
  # addPopups(~longitude, ~latitude, ~description)
# map_ottenby_smhi

