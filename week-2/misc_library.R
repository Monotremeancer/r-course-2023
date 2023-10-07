

# Miscellaneous -----------------------------------------------------------

file_copy <- function(dir_from, 
                      file_from, 
                      dir_to = dir_from, 
                      file_to = file_from) {
  if(!dir.exists(path_to)) {
    dir.create(path_to)
  }
  result <- file.copy(file.path(path_from, file_from), 
                      file.path(path_to, file_to))
  return(result)
}

file_move <- function(dir_from, 
                      file_from, 
                      dir_to = dir_from, 
                      file_to = file_from) {
  if(!dir.exists(path_to)) {
    dir.create(path_to)
  }
  result <- file.rename(file.path(path_from, file_from), 
                        file.path(path_to, file_to))
  return(result)
}
