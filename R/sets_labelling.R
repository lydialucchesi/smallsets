#' Sets labelling
#'
#' @description Sets labelling parameters for the Smallset Timeline.
#' @param labelCol "lighter" or "darker" for colour of column names and printed
#'   data, in comparison to tile colours.
#' @param labelColDif Value between 0-1 determining how much lighter or darker.
#'
#' @details Passed to \code{labelling} in \link{Smallset_Timeline}.
#'
#' @return Returns a list with two elements (the labelling parameters).
#'
#' @examples
#' # labels are black
#' Smallset_Timeline(
#'   data = s_data,
#'   code = system.file("s_data_preprocess.R", package = "smallsets"),
#'   labelling = sets_labelling(labelCol = "darker", labelColDif = 1)
#' )
#'
#' # labels are midpoint between tile colour and white
#' Smallset_Timeline(
#'   data = s_data,
#'   code = system.file("s_data_preprocess.R", package = "smallsets"),
#'   labelling = sets_labelling(labelCol = "lighter", labelColDif = .5)
#' )
#'
#' @export

sets_labelling <- function(labelCol = NULL,
                           labelColDif = NULL) {
  labelling <- list()
  
  if (is.null(labelCol)) {
    labelling$labelCol <- "darker"
  } else {
    labelling$labelCol <- labelCol
  }
  
  if (is.null(labelColDif)) {
    labelling$labelColDif <- .5
  } else {
    labelling$labelColDif <- labelColDif
  }
  
  return(labelling)
}
