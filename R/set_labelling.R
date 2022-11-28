#' Set labelling
#' @description Sets labelling parameters if not all specified in list object.
#' @keywords internal

set_labelling <- function(labelling) {
  if (is.null(labelling$labelsCol)) {
    labelling$labelsCol <- "darker"
  }
  if (is.null(labelling$labelsColDif)) {
    labelling$labelsColDif <- .5
  }
  return(labelling)
}
