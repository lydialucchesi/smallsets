#' Return selected colour scheme
#' @description Returns the colours in a pre-built scheme.
#' @keywords internal

return_scheme <- function(colScheme = "colScheme1") {
  if (colScheme == "colScheme1") {
    CS <-
      list(
        same = "#D3D2CC",
        edit = "#C9D5F5",
        add = "#CDAFEE",
        delete = "#FBE4B5"
      )
  }
  if (colScheme == "colScheme2") {
    CS <-
      list(
        same = "#4F5353",
        edit = "#BE8F52",
        add = "#978878",
        delete = "#708F90"
      )
  }
  
  return(CS)
}
