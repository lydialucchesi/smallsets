#' Get timeline dimensions
#' @description A function to find the maximum and minimum number of rows and columns in the timeline
#' @keywords internal

get_timeline_dimensions <- function(tabs) {
  
  rows <- numeric()
  cols <- numeric()
  for (i in 1:length(tabs)) {
    rows <- c(rows, nrow(tabs[[i]][[1]]))
    cols <- c(cols, ncol(tabs[[i]][[1]]))
  }
  
  maxX <- max(cols)
  maxY <- max(rows)
  
  maxDims <- c(maxX, maxY)
  
  return(maxDims)
  
}
