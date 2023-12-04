#' Sets sizing
#'
#' @description Sets sizing parameters for the Smallset Timeline.
#'
#' @param captions Positive numeric value for caption text size. Default is 3.
#' @param columns Positive numeric value for column name text size. Default is 3.
#' @param data Positive numeric value for printed data text size. Default is 2.
#' @param icons Positive numeric value for legend icon size. Default is 1.
#' @param legend Positive numeric value for legend text size. Default is 10.
#' @param resume Positive numeric value for resume marker size. Default is 1.
#' @param tiles Positive numeric value for Smallset tile size. Default is .2.
#'
#' @details Passed to \code{sizing} in \link{Smallset_Timeline}.
#'
#' @return Returns a list with seven elements (the sizing parameters).
#'
#' @examples
#' # increase size of caption text
#' # and add more caption space, so larger caption text fits
#' Smallset_Timeline(
#'    data = s_data,
#'    code = system.file("s_data_preprocess.R", package = "smallsets"),
#'    sizing = sets_sizing(captions = 4),
#'    spacing = sets_spacing(captions = 4)
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
    sizing$captions <- 3
  } else {
    sizing$captions <- captions
  }
  
  if (is.null(columns)) {
    sizing$columns <- 3
  } else {
    sizing$columns <- columns
  }
  
  if (is.null(data)) {
    sizing$data <- 2
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
