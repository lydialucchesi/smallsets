#' Set sizing
#' @description Sets sizing parameters if not all specified in list object.
#' @keywords internal

set_sizing <- function(sizing) {
  if (is.null(sizing$columns)) {
    sizing$columns <- 2.5
  }
  if (is.null(sizing$tiles)) {
    sizing$tiles <- .2
  }
  if (is.null(sizing$captions)) {
    sizing$captions <- 2.5
  }
  if (is.null(sizing$data)) {
    sizing$data <- 2.5
  }
  if (is.null(sizing$legendText)) {
    sizing$legendText <- 10
  }
  if (is.null(sizing$legendIcons)) {
    sizing$legendIcons <- 1
  }
  if (is.null(sizing$resume)) {
    sizing$resume <- .25
  }
  return(sizing)
}
