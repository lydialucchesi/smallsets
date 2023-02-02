#' Retrieve dimensions
#' @description Retrieves the maximum and minimum number of rows and columns in
#'   the Smallset Timeline.
#' @keywords internal

retrieve_dimensions <- function(tabs) {
  rows <- numeric()
  cols <- numeric()
  # Find number of rows/columns in each table
  for (i in 1:length(tabs)) {
    rows <- c(rows, nrow(tabs[[i]][[1]]))
    cols <- c(cols, ncol(tabs[[i]][[1]]))
  }
  
  # Return dimensions of widest/longest tables
  return(c(max(cols), max(rows)))
}
