#' Sets sizing
#'
#' @description Sets sizing parameters for the Smallset Timeline.
#'
#' @param captions Positive numeric value for caption text size.
#' @param columns Positive numeric value for column name text size.
#' @param data Positive numeric value for printed data text size.
#' @param icons Positive numeric value for legend icon size.
#' @param legend Positive numeric value for legend text size.
#' @param resume Positive numeric value for resume marker size.
#' @param tiles Positive numeric value for Smallset tile size.
#'
#' @details Passed to \code{sizing} in \link{Smallset_Timeline}.
#'
#' @return Returns a list with seven elements (the sizing parameters).
#'
#' @examples
#' # increase size of caption text
#' Smallset_Timeline(
#'    data = s_data,
#'    code = system.file("s_data_preprocess.R", package = "smallsets"),
#'    sizing = sets_sizing(captions = 3.5)
#' )
#'
#' @export

sets_sizing <- function(captions = NULL,
                        columns = NULL,
                        data = NULL,
                        icons = NULL,
                        legend = NULL,
                        resume = NULL,
                        tiles = NULL) {
  sizing <- list()
  
  if (is.null(captions)) {
    sizing$captions <- 2.5
  } else {
    sizing$captions <- captions
  }
  
  if (is.null(columns)) {
    sizing$columns <- 2.5
  } else {
    sizing$columns <- columns
  }
  
  if (is.null(data)) {
    sizing$data <- 2.5
  } else {
    sizing$data <- data
  }
  
  if (is.null(icons)) {
    sizing$icons <- 1
  } else {
    sizing$icons <- icons
  }
  
  if (is.null(legend)) {
    sizing$legend <- 10
  } else {
    sizing$legend <- legend
  }
  
  if (is.null(resume)) {
    sizing$resume <- 1
  } else {
    sizing$resume <- resume
  }
  
  if (is.null(tiles)) {
    sizing$tiles <- .2
  } else {
    sizing$tiles <- tiles
  }
  
  return(sizing)
}
