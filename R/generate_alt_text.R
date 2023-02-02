#' Generate alt text
#' @description Generates alternative text (alt text) for the Smallset Timeline.
#' @keywords internal

generate_alt_text <-
  function(tables,
           fourCols,
           legendDF,
           atInfo,
           l,
           printedData,
           ghostData) {
    # Create empty vector for alt text
    at <- c()
    
    # Write how many snapshots
    at <- c(at, write_snapshots(tables))
    
    # Write about colour legend
    if (sum(grepl("Data has been edited.", legendDF)) == 1) {
      colourEdit <- sapply(fourCols[2], plotrix::color.id)[1]
      at <- c(at, write_colourEdit(colourEdit))
    }
    if (sum(grepl("Data has been added.", legendDF)) == 1) {
      colourAdd <- sapply(fourCols[3], plotrix::color.id)[1]
      at <- c(at, write_colourAdd(colourAdd))
    }
    if (sum(grepl("Data will be deleted.", legendDF)) == 1) {
      colourDelete <- sapply(fourCols[4], plotrix::color.id)[1]
      at <- c(at, write_colourDelete(colourDelete))
    }
    
    # Loop through snapshots and describe each one
    for (i in 1:length(tables)) {
      at <- c(at, write_dimensions(i, tables))
      
      if (i == 1) {
        # Write out Smallset column names
        at <-
          c(at, write_columnNames(colnames(tables[[1]]$body$dataset)))
      }
      
      # Write about row deletions
      if (length(atInfo$rowsDrop) != 0) {
        at <- c(at, write_rowsDelete(atInfo$rowsDrop, colourDelete))
      }
      atInfo$rowsDrop <- NULL
      
      # Write about column deletions
      if (length(atInfo$colsDrop) != 0) {
        at <- c(at, write_columnsDelete(atInfo$colsDrop, colourDelete))
      }
      atInfo$colsDrop <- NULL
      
      if (i > 1) {
        # Write about data edits
        if (nrow(atInfo$adjData) > 0) {
          at <- c(at, write_cellsEdit(atInfo$adjData, colourEdit))
        }
        atInfo$adjData <- NULL
        
        # Write about row additions
        if (length(atInfo$rowsAdd) != 0) {
          at <- c(at, write_rowsAdd(atInfo$rowsAdd, colourAdd))
        }
        atInfo$rowsAdd <- NULL
        
        # Write about column additions
        if (length(atInfo$colsAdd) != 0) {
          at <- c(at, write_columnsAdd(atInfo$colsAdd, colourAdd))
        }
        atInfo$colsAdd <- NULL
      }
      
      # Write the snapshot caption
      if (isFALSE(printedData) & isTRUE(ghostData)) {
        if (layer_data(l[[i]], 3)$label != "") {
          at <- c(at, write_caption(layer_data(l[[i]], 3)$label))
        }
      } else if (isTRUE(printedData) & isFALSE(ghostData)) {
        if (layer_data(l[[i]], 5)$label != "") {
          at <- c(at, write_caption(layer_data(l[[i]], 5)$label))
        }
      } else {
        if (layer_data(l[[i]], 4)$label != "") {
          at <- c(at, write_caption(layer_data(l[[i]], 4)$label))
        }
      }
    }
    
    cat(paste0(at, collapse = " "))
    
  }
