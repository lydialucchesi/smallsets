#' Sets spacing
#' @description Sets spacing parameters for the Smallset Timeline.
#' @param captions Positive numeric value for amount of caption space below snapshots.
#' @param columnT Positive numeric value for amount of column name space.
#' @param degree Integer between 0-90 (degrees) to rotate column names.
#' @param rows Integer for number of Timeline rows.
#' @param tableR Positive numeric value for amount of space between snapshots.
#'
#' @examples sets_spacing(captions = 6, degree = 45, rows = 2)
#'
#'@export

sets_spacing <- function(captions = NULL,
                         degree = NULL,
                         columnT = NULL,
                         rows = NULL,
                         tableR = NULL) {
  spacing <- list()
  
  if (is.null(captions)) {
    spacing$captions <- 2
  } else {
    spacing$captions <- captions
  }
  
  if (is.null(degree)) {
    spacing$degree <- 0
  } else {
    spacing$degree <- degree
  }
  
  if (is.null(columnT)) {
    spacing$columnT <- 1
  } else {
    spacing$columnT <- columnT
  }
  
  if (is.null(rows)) {
    spacing$rows <- 1
  } else {
    spacing$rows <- rows
  }
  
  if (is.null(tableR)) {
    spacing$tableR <- .5
  } else {
    spacing$tableR <- tableR
  }
  
  return(spacing)
}
