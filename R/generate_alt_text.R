#' Generate alt text
#' @description Generates alternative text (alt text) for the Smallset Timeline.
#' @noRd

generate_alt_text <-
  function(tables,
           fourCols,
           legendDF,
           atInfo,
           l,
           printedData,
           ghostData,
           resume) {
    # Create empty vector for alt text
    at <- c()
    
    # Write how many snapshots
    at <- c(at, write_snapshots(tables))
    
    # Write about colour legend
    if (sum(grepl("Added   ", legendDF)) == 1) {
      colourAdded <- sapply(fourCols[1], plotrix::color.id)[1]
      at <- c(at, write_colourAdded(colourAdded))
    }
    if (sum(grepl("Deleted   ", legendDF)) == 1) {
      colourDeleted <- sapply(fourCols[2], plotrix::color.id)[1]
      at <- c(at, write_colourDeleted(colourDeleted))
    }
    if (sum(grepl("Edited   ", legendDF)) == 1) {
      colourEdited <- sapply(fourCols[3], plotrix::color.id)[1]
      at <- c(at, write_colourEdited(colourEdited))
    }
    if (sum(grepl("Unchanged   ", legendDF)) == 1) {
      colourUnchanged <- sapply(fourCols[4], plotrix::color.id)[1]
      at <- c(at, write_colourUnchanged(colourUnchanged))
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
        at <- c(at, write_rowsDelete(atInfo$rowsDrop, colourDeleted))
      }
      atInfo$rowsDrop <- NULL
      
      # Write about column deletions
      if (length(atInfo$colsDrop) != 0) {
        at <- c(at,
                write_columnsDelete(atInfo$colsDrop, colourDeleted))
      }
      atInfo$colsDrop <- NULL
      
      if (i > 1) {
        # Write about data edits
        if (nrow(atInfo$adjData) > 0) {
          at <- c(at, write_cellsEdit(atInfo$adjData, colourEdited))
        }
        atInfo$adjData <- NULL
        
        # Write about row additions
        if (length(atInfo$rowsAdd) != 0) {
          at <- c(at, write_rowsAdd(atInfo$rowsAdd, colourAdded))
        }
        atInfo$rowsAdd <- NULL
        
        # Write about column additions
        if (length(atInfo$colsAdd) != 0) {
          at <- c(at, write_columnsAdd(atInfo$colsAdd, colourAdded))
        }
        atInfo$colsAdd <- NULL
      }
      
      if ((i - 1) %in% resume) {
        c <- i + 1
      } else {
        c <- i
      }
      # Write the snapshot caption
      if (isFALSE(printedData) & isTRUE(ghostData)) {
        if (layer_data(l[[c]], 3)$label != "") {
          at <- c(at, write_caption(layer_data(l[[c]], 3)$label))
        }
      } else if (isTRUE(printedData) & isFALSE(ghostData)) {
        if (layer_data(l[[c]], 5)$label != "") {
          at <- c(at, write_caption(layer_data(l[[i]], 5)$label))
        }
      } else {
        if (layer_data(l[[c]], 4)$label != "") {
          at <- c(at, write_caption(layer_data(l[[i]], 4)$label))
        }
      }
      
      if (i %in% resume) {
        at <- c(at, write_resumeMarker(i, layer_data(l[[i + 1]], 2)$label))
      }
    }
    
    cat(paste0(at, collapse = " "))
    
  }
