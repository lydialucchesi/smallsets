#' Get timeline dimensions
#' @description A function to find the maximum and minimum number of rows and columns in the timeline
#' @keywords internal
#' @export

get_timeline_dimensions <- function(ftsList) {
  
  rows <- numeric()
  cols <- numeric()
  for (i in 1:length(ftsList[[1]])) {
    rows <- c(rows, nrow(ftsList[[1]][[i]]$body$dataset))
    cols <- c(cols, ncol(ftsList[[1]][[i]]$body$dataset))
  }
  
  maxX <- max(cols)
  maxY <- max(rows)
  
  maxDims <- c(maxX, maxY)
  
  return(maxDims)
  
}
