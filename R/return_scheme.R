#' Return selected colour scheme
#' @description The function returns a selected colour scheme.
#' @keywords internal

return_scheme <- function(colScheme = "colScheme1") {
  if (colScheme == "colScheme1") {
    CS <-
      list(
        constant = list("#D3D2CC", 1),
        changed = list("#C9D5F5", 1),
        added = list("#CDAFEE", 1),
        deleted = list("#FBE4B5", 1)
      )
  } else if (colScheme == "colScheme2") {
    CS <-
      list(
        constant = list("#4F5353", 1),
        changed = list("#BE8F52", 1),
        added = list("#978878", 1),
        deleted = list("#6C7C7D", 1)
      )
  } else if (colScheme == "colScheme3") {
    CS <-
      list(
        constant = list("#E3D4C3", .7),
        changed = list("#4c4cff", .7),
        added = list("#FEE004", .7),
        deleted = list("#FF0000", .7)
      )
  } else {
    stop(
      "Please choose a colour scheme: colScheme1, colScheme2, or colScheme3. See colScheme argument in ?create_timeline."
    )
  }
  
  return(CS)
}
