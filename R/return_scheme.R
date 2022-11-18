#' Return selected colour scheme
#' @description The function returns a selected colour scheme.
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
  } else if (colScheme == "colScheme2") {
    CS <-
      list(
        same = "#4F5353",
        edit = "#BE8F52",
        add = "#978878",
        delete = "#708F90"
      )
  } else if (colScheme == "colScheme3") {
    CS <-
      list(
        same = "#E3D4C3",
        edit = "#4c4cff",
        add = "#FEE004",
        delete = "#FF0000"
      )
  } else {
    stop(
      "Please choose one of the available colour schemes: colScheme1, colScheme2, or colScheme3."
    )
  }
  
  return(CS)
}
