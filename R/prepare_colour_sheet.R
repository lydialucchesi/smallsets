#' Prepare colour sheet
#' @description Generates a visual appearance matrix for automated Smallset selection (autoSelect = 2).
#' @keywords internal
#' @import "flextable" "tibble"

prepare_colour_sheet <- function(smallsetList,
                                 fourCols) {
  first <- smallsetList[[1]]
  first[] <- lapply(first, as.character)
  
  last <- smallsetList[[length(smallsetList)]]
  last[] <- lapply(last, as.character)
  
  colsDrop <- setdiff(colnames(first), colnames(last))
  rowsDrop <- setdiff(rownames(first), rownames(last))
  
  for (c in colsDrop) {
    if (!c %in% colnames(last)) {
      origCols <- colnames(first)
      place <- which(origCols == c) - 1
      last <-
        add_column(.data = last,
                   addingCol = NA,
                   .after = place)
      names(last)[names(last) == 'addingCol'] <- c
    }
  }
  
  for (r in rowsDrop) {
    if (!r %in% rownames(last)) {
      origRows <- rownames(first)
      place <- which(origRows == r) - 1
      rownameslist <- append(rownames(last), r, after = place)
      last <-
        add_row(.data = last,
                .after = place)
      rownames(last) <- rownameslist
    }
  }
  
  colours <- last
  colours[,] <- "#808080"
  
  tables <- find_data_changes(
    smallsetList = smallsetList,
    fourCols = fourCols,
    altText = FALSE
  )
  
  for (t in 1:length(tables)) {
    t_colours <- as.data.frame(tables[[t]]$body$styles$text$color$data)
    rownames(t_colours) <- rownames(tables[[t]]$body$dataset)
    
    for (i in rownames(t_colours)) {
      for (j in colnames(t_colours)) {
        c1 <- t_colours[i, j]
        if (c1 != "#808080") {
          colours[i, j] <- c1
        }
      }
    }
  }
  
  return(colours)
  
}
