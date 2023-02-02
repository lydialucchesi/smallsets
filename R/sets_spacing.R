#' Sets spacing
#'
#' @description Sets spacing parameters for the Smallset Timeline.
#'
#' @param captions Positive numeric value for amount of caption space below
#'   snapshots.
#' @param header Positive numeric value for amount of column name space.
#' @param degree Integer between 0-90 (degrees) to rotate column names.
#' @param right Positive numeric value (>=.5) for amount of space to the right
#'   of each snapshot.
#' @param rows Integer for number of Timeline rows.
#'
#' @details Passed to \code{spacing} in \link{Smallset_Timeline}.
#'
#' @return Returns a list with five elements (the spacing parameters).
#'
#' @examples
#' # increase space for captions and rotate column names
#' Smallset_Timeline(
#'    data = s_data,
#'    code = system.file("s_data_preprocess.R", package = "smallsets"),
#'    spacing = sets_spacing(captions = 5, degree = 45)
#' )
#'
#' @export

sets_spacing <- function(captions = NULL,
                         degree = NULL,
                         header = NULL,
                         right = NULL,
                         rows = NULL) {
  spacing <- list()
  
  if (is.null(captions)) {
    spacing$captions <- 3
  } else {
    spacing$captions <- captions
  }
  
  if (is.null(degree)) {
    spacing$degree <- 0
  } else {
    spacing$degree <- degree
  }
  
  if (is.null(header)) {
    spacing$header <- 1
  } else {
    spacing$header <- header
  }
  
  if (is.null(right)) {
    spacing$right <- .5
  } else {
    if (right < .5) {
      spacing$right <- .5
    } else {
      spacing$right <- right
    }
  }
  
  if (is.null(rows)) {
    spacing$rows <- 1
  } else {
    spacing$rows <- rows
  }
  
  return(spacing)
}
