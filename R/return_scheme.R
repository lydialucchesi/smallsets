#' Return scheme
#' @description Returns the colours in a colour scheme.
#' @noRd

return_scheme <- function(colScheme = 1) {
  if (colScheme == 1) {
    CS <- list(
      added = "#B385E5",
      deleted = "#F9D76C",
      edited = "#87BCEF",
      unchanged = "#E6E4DF"
    )
  } else if (colScheme == 2) {
    CS <-
      list(
        added = "#2C64B8",
        deleted = "#FCBD2D",
        edited = "#51A81D",
        unchanged = "#E6E4DF"
      )
  } else if (colScheme == 3) {
    CS <-
      list(
        added = "#33CCCC",
        deleted = "#FF2F40",
        edited = "#FF9C17",
        unchanged = "#5c5e66"
      )
  } else {
    stop("Please choose scheme 1, 2, or 3 or provide a list of four colours.")
  }
  
  return(CS)
}
