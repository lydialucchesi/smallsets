#' Abstract the timeline
#'
#' @param ftsList A list from \code{highlight_changes}
#' @param sizing A list of size specifications for the column names, the tiles, the captions, the cirlces, and the symbols
#' @export

abstract_timeline <- function(ftsList, sizing = list("columns" = 2, "tiles" = 1, "captions" = 8, "circles" = .15, "symbols" = 2.5)) {
  items <- seq(1, length(ftsList[[1]]), 1)
  
  if (is.null(sizing[["columns"]])) {
    sizing[["columns"]] = 2
  }
  
  if (is.null(sizing[["tiles"]])) {
    sizing[["tiles"]] = 1
  }
  
  if (is.null(sizing[["captions"]])) {
    sizing[["captions"]] = 8
  }
  
  if (is.null(sizing[["circles"]])) {
    sizing[["circles"]] = .15
  }
  
  if (is.null(sizing[["symbols"]])) {
    sizing[["symbols"]] = 2.5
  }
  
  l <- lapply(items, ftsList, sizing, FUN = flex_to_colour_plot)
  
  return(l)
  
}
