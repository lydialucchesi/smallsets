#' Retrieve dimensions
#' @description Retrieves the maximum and minimum number of rows and columns in the Smallset Timeline.
#' @keywords internal

retrieve_dimensions <- function(tabs) {
  rows <- numeric()
  cols <- numeric()
  for (i in 1:length(tabs)) {
    rows <- c(rows, nrow(tabs[[i]][[1]]))
    cols <- c(cols, ncol(tabs[[i]][[1]]))
  }
  
  # Find longest table
  maxX <- max(cols)
  
  # Find widest table
  maxY <- max(rows)
  
  maxDims <- c(maxX, maxY)
  
  return(maxDims)
  
}
