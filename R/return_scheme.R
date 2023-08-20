#' Return scheme
#' @description Returns the colours in a colour scheme.
#' @keywords internal

return_scheme <- function(colScheme = 1) {
  if (colScheme == 1) {
    CS <-
      list(
        same = "#E6E4DF",
        add = "#CDAFEE",
        delete = "#FBE49D",
        edit = "#B4D5F5"
      )
  } else if (colScheme == 2) {
    CS <-
      list(
        same = "#4F5353",
        add = "#978878",
        delete = "#708F90",
        edit = "#BE8F52"
      )
  } else if (colScheme == 3) {
    CS <- list(
      same = "#E0E0E0",
      add = "#D000E3",
      delete = "#2F329C",
      edit = "#36E342"
    )
  } else {
    stop("Please choose scheme 1, 2, or 3 or provide a list of four colours.")
  }
  
  return(CS)
}
