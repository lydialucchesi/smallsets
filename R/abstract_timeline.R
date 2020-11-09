#' Abstract the timeline
#'
#' @param ftsList A list from \code{highlight_changes}
#' @param sizing A list of size specifications for the column names, the tiles, the captions, the cirlces, and the symbols
#' @param stampCols Either "darker" or "lighter" for stamp colour. Can enter a list corresponding to specific actions.
#' @param stampColsDif Degree to which stamp colour is darker or lighter. Can enter a list corresponding to specific actions.
#' @param stampLoc Location of stamp. 1 = top left. 2 = top right. 3 = bottom left. 4 = bottom right. 5 = center.
#' @param timelineRows Number of rows to divide the smallset timeline into.
#' @export
#' @import "patchwork"

abstract_timeline <- function(ftsList, sizing = list("columns" = 2, "tiles" = 1, "captions" = 8, "circles" = .15, "symbols" = 2.5), stampCols = "darker", stampColsDif = .5, stampLoc = 1, timelineRows = NULL) {
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
  
  if (is.list(stampCols)) {
    if (is.null(stampCols[["changed"]])) {
      stampCols[["changed"]] = "darker"
    }
    
    if (is.null(stampCols[["added"]])) {
      stampCols[["added"]] = "darker"
    }
    
    if (is.null(stampCols[["deleted"]])) {
      stampCols[["deleted"]] = "darker"
    }
  } else {
    if (stampCols == "darker") {
      stampCols <- list(changed = "darker", added = "darker", deleted = "darker")
    } else {
      stampCols <- list(changed = "lighter", added = "lighter", deleted = "lighter")
    }
  }
  
  if (is.list(stampColsDif)) {
    if (is.null(stampColsDif[["changed"]])) {
      stampColsDif[["changed"]] = .5
    }
    
    if (is.null(stampColsDif[["added"]])) {
      stampColsDif[["added"]] = .5
    }
    
    if (is.null(stampColsDif[["deleted"]])) {
      stampColsDif[["deleted"]] = .5
    }
  } else {
    stampColsDif <- list(changed = stampColsDif, added = stampColsDif, deleted = stampColsDif)
  }

  maxDims <- get_timeline_dimensions(ftsList)
  
  l <- lapply(items, ftsList, sizing, stampCols, stampColsDif, stampLoc, maxDims, FUN = flex_to_colour_plot)
  
  patchedPlots <- ""
  for (s in 1:length(l)) {
    addPlot <- paste0("l[[", as.character(s), "]] + ")
    patchedPlots <- paste0(patchedPlots, addPlot)
  }
  
  if (is.null(timelineRows)) {
    patchedPlots <- paste0(patchedPlots, "plot_layout()")
  } else {
    patchedPlots <- paste0(patchedPlots, "plot_layout(nrow = ", as.character(timelineRows), ")")
  }
  
  return(eval(parse(text = patchedPlots)))
  
}
