#' Return scheme
#' @description Returns the colours in a colour scheme.
#' @keywords internal

return_scheme <- function(colScheme = 1) {
  if (colScheme == 1) {
    CS <-
      list(
        same = "#E6E4DF",
        edit = "#B4D5F5",
        add = "#CDAFEE",
        delete = "#FBE49D"
      )
  } else if (colScheme == 2) {
    CS <-
      list(
        same = "#4F5353",
        edit = "#BE8F52",
        add = "#978878",
        delete = "#708F90"
      )
  } else if (colScheme == 3) {
    CS <- list(
      same = "#E0E0E0",
      edit = "#36E342",
      add = "#D000E3",
      delete = "#2F329C"
    )
  } else {
    stop("Please choose scheme 1, 2, or 3 or provide a list of four colours.")
  }
  
  return(CS)
}
