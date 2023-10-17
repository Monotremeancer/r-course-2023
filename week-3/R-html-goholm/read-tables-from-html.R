library(tidyverse)
library(rvest)
library(writexl)

# Clear workspace.
rm(list = ls())

# Create directory 
dir_out = "data-out"
if (!dir.exists(dir_out)) {
  dir.create(dir_out)
}
# -------------------------------------------------------------------------

filenames = c("data-in/Goholm2023_1_3000.html",
              "data-in/Goholm2023_3001_6000.html")

# Loop over indata files.
result_table <- NULL
index = 0
for (filename in filenames) {
  # Read HTML file.
  response = read_html(filename)
  # Extract tables from HTML file.
  tables = response |>
    rvest::html_table()
  
  # Loop over tables.
  for (table in tables) {
    index = index + 1
    table_name = paste("table", as.character(index), sep = "")
    assign(table_name, table)
    # # Activate this part if you want to save all tables.
    # file_name = paste("html_table-", as.character(index), ".xlsx", sep = "")
    # write_xlsx(table, file.path(dir_out, file_name))
  }
  
  # Remember last table, or concatenate with the earlier table.
  if (is.null(result_table)) {
    result_table <- table
  } else {
    result_table <- rbind(result_table, table)
  }
  
  # Save to file as ".xlsx".
  write_xlsx(result_table, file.path(dir_out, "result_table.xlsx"))
}
