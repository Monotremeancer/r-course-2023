#Load the package containing any required function to load datafiles 
require(openxlsx)

"
Takes a starting directory and recursively loads all of the files of a given 
name and extention in that, and all subdirectories. Then returns a dataframe
with all of the appended loaded files appended to it.
Default is to load all .xlsx files in current working directory .

Starting directory: sets the directory from which to load all files.
document_name: sets the document name for which to search. Accepts regex
file_format: the file extention to look for in the directory/ies
loading_function: the function to use to load the files.

Currently the loading function takes to optional arguments, so ideally rely
on xlsx or csv files (with read.csv).

Example use:
# Load all files called batcalls.csv in the MyData directory
data <- compile_spreadsheet(starting_directory = './MyData',
                            document_name = 'batcalls',
                            file_format = 'csv',
                            loading_function = read.csv)
                            
# Load all .xlsx contained in the current working directory and it's
# subdirectories
data <- compile_spreadsheet()
"



compile_spreadsheets <- function(starting_directory =".",
                         document_name = "*",
                         file_format = "xlsx",
                         loading_function = read.xlsx,
                         ...){

    #Set the working directory to appropriate directory 
  setwd(starting_directory)
  
  #List all files to load
  to_load <- list.files(pattern = paste0(document_name,".",file_format), 
                        recursive = TRUE)
  
  #Populate a dataframe with the information from all the desired files
  data <- data.frame()

    for( curr_file_num in 1:length(to_load)){
      curr_file <- to_load[[curr_file_num]]
      
      # Load current file and add a column with the filename to allow tracking
      data_to_append <- loading_function(curr_file, ...)
      data_to_append$source_file <- rep(curr_file, nrow(data_to_append))
      
      # Append new data
      data <- rbind(data, data_to_append)
      
      writeLines(paste("Loaded file:", curr_file))
    }

  
  #Return the complete dataframe  
  data
}
compile_spreadsheets()
