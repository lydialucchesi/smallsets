#' Set spacing
#' @description Sets spacing parameters if not all specified in list object.
#' @keywords internal

set_spacing <- function(spacing) {
  if (is.null(spacing$captionB)) {
    spacing$captionB = 3
  }
  if (is.null(spacing$columnsT)) {
    spacing$columnsT = 1
  }
  if (is.null(spacing$tablesR)) {
    spacing$tablesR = .5
  }
  if (is.null(spacing$rows)) {
    spacing$rows = 1
  }
  if (is.null(spacing$columnsDeg)) {
    spacing$columnsDeg = 0
  }
  return(spacing)
}
