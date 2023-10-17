library(tidyverse)
library(googlesheets4)
library(readxl)
library(writexl)
library(janitor)

# Clear workspace.
rm(list=ls())

# Import ------------------------------------------------------------------

# # Import from Google Sheets.
# google_sheet_id <- REPLACE-THIS-WITH-DOCUMENT-ID
# gs4_deauth() # Used for read only access, no Google Drive login needed.
# ottenby <- read_sheet(google_sheet_id)

# Similar, but from Excel. 
# Should be save from the Google sheet as ".xlsx".
ottenby = read_excel("data-in/Copy of Artanalyserna Ottenby 2022.xlsx")

# Tidy --------------------------------------------------------------------

ottenby_tidy = ottenby |>
  
  # Use the first 10 columns.
  select(Filename:Kommentar) |>
  
  # Remove not used columns.
  select(-Vem, -Buzz, -Social) |>
  
  # Automatic conversion to snake_case...
  janitor::clean_names() |>
  
  # ...and manually fix the rest.
  rename(
    auto_id = contains,
    taxa_ok = klar,
    social_ok = soc_klar,
    number_of_ind = antal_indiv,
    comments = kommentar
  ) |>
  
  # Remove rows not containing bat calls.
  filter(
    !taxa_ok %in% c(
      "nobat",
      "noBAT",
      "Nobat",
      "NoBAT",
      "NObat",
      "NOBAt",
      "NOBAT",
      "NOISE",
      "x",
      "NA",
      "TETTIGONIIDAE",
      "Stopp Nytt År"
    )
  ) |>
  
  # Also remove with no taxa.
  filter(!is.na(taxa_ok)) |>
  filter(taxa_ok != "") |>
  
  # Remove rows with missing date_time.
  filter(!is.na(date_time)) |>
  filter(date_time != "") |>
  
  # Clean up values.
  mutate(
    taxa_ok = recode(
      taxa_ok,
      # FROM OLD = TO NEW,
      "Chir" = "Chiroptera",
      "chir" = "Chiroptera",
      "enil" = "Enil",
      "ENIL" = "Enil",
      "M sp" = "M.sp",
      "myotis sp" = "M.sp",
      "Myotis sp" = "M.sp",
      "N sp" = "N.sp",
      "nnoc" = "Nnoc",
      "NNOC" = "Nnoc",
      "nyc?" = "Nyctaloid",
      "Nyc" = "Nyctaloid",
      "Nyc?" = "Nyctaloid",
      "Nyctaloid" = "Nyctaloid",
      "Ob. nyctaloid" = "Nyctaloid",
      "p pip" = "Ppip",
      "P pip" = "Ppip",
      "P sp" = "P.sp",
      "p sp" = "P.sp",
      "P SP" = "P.sp",
      "P. SP." = "P.sp",
      "pnat" = "Pnat",
      "PNAT" = "Pnat",
      "ppip" = "Ppip",
      "ppyg" = "Ppyg",
      "PPyg" = "Ppyg",
      "PPYG" = "Ppyg",
      "Ppyg, Psp" = "P.sp",
      "Psp" = "P.sp",
      "Psp, Mchi" = "P.sp",
    )
  )

# Replace NAs and spaces in column "number_of_ind".
ottenby_tidy$number_of_ind[ottenby_tidy$number_of_ind == ""] <- 1
ottenby_tidy$number_of_ind[is.na(ottenby_tidy$number_of_ind)] <- 1

# Transform ---------------------------------------------------------------

ottenby_transform <- ottenby_tidy |>
  
  # Add some new columns.
  mutate(datetime = parse_date_time(date_time, "Ymd HM")) |>
  mutate(month = month(datetime)) |>
  mutate(date = date(datetime))

# Convert from long format to wide format, by day.
ottenby_columns_by_day <- ottenby_transform |>
  select(date, taxa_ok, number_of_ind) |>
  group_by(date, taxa_ok) |>
  summarize(observations = sum(number_of_ind)) |>
  spread(taxa_ok, observations)

# Convert from long format to wide format, by month.
ottenby_columns_by_month <- ottenby_transform |>
  select(month, taxa_ok, number_of_ind) |>
  group_by(month, taxa_ok) |>
  summarize(observations = sum(number_of_ind)) |>
  spread(taxa_ok, observations)

# Visualize ---------------------------------------------------------------

# Just a single example.
ottenby_transform |>
  group_by(month) |>
  summarize(observations = sum(number_of_ind)) |>
  ggplot(aes(x = month, y = observations)) +
  # geom_point() +
  geom_line()

# ...and another one.
ottenby_transform |>
  group_by(date) |>
  summarize(observations = sum(number_of_ind)) |>
  ggplot(aes(x = date, y = observations)) +
  # geom_point() +
  geom_line()

# Warning: This one counts rows only, not "number_of_ind".
ottenby_transform |>
  ggplot(aes(x = date, col = taxa_ok, fill = taxa_ok)) +
  geom_bar()

# Communicate -------------------------------------------------------------

# Save the resulting datasets.

dir_out = "data-out"
if (!dir.exists(dir_out)) {
  dir.create(dir_out)
}
write_xlsx(ottenby_transform,
           file.path(dir_out, "ottenby_2022_tidy.xlsx"))

write_xlsx(ottenby_columns_by_day,
           file.path(dir_out, "ottenby_2022_columns_by_day.xlsx"))

write_xlsx(ottenby_columns_by_month,
           file.path(dir_out, "ottenby_2022_columns_by_month.xlsx"))
