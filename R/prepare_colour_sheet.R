#' Prepare colour sheet
#' @description Prepares the visual appearance matrix for automated Smallset
#'   selection.
#' @noRd

prepare_colour_sheet <- function(smallsetList,
                                 fourCols,
                                 ignoreCols) {
  first <- smallsetList[[1]]
  first[] <- lapply(first, as.character)
  
  last <- smallsetList[[length(smallsetList)]]
  last[] <- lapply(last, as.character)
  
  # Find data deletions
  colsDrop <- setdiff(colnames(first), colnames(last))
  rowsDrop <- setdiff(rownames(first), rownames(last))
  
  # Add dropped columns to final data frame
  for (c in colsDrop) {
    if (!c %in% colnames(last)) {
      origCols <- colnames(first)
      place <- which(origCols == c) - 1
      addingCol <- rep(NA, nrow(last))
      
      if (place == 0) {
        last <- data.frame(cbind(addingCol,
                                 last[, seq((place + 1), ncol(last))]))
      } else if (place == ncol(last)) {
        last <- data.frame(cbind(last[, seq(1, place)],
                                 addingCol))
      } else {
        last <- data.frame(cbind(last[, seq(1, place)],
                                 addingCol,
                                 last[, seq((place + 1), ncol(last))]))
      }
      
      names(last)[names(last) == 'addingCol'] <- c
    }
  }
  
  # Add dropped rows to final data frame
  for (r in rowsDrop) {
    if (!r %in% rownames(last)) {
      origRows <- rownames(first)
      place <- which(origRows == r) - 1
      rownameslist <- append(rownames(last), r, after = place)
      
      if (place == 0) {
        last <- rbind(rep(NA, ncol(last)),
                      last[(place + 1):nrow(last),])
      } else if (place == nrow(last)) {
        last <- rbind(last[1:place,],
                      rep(NA, ncol(last)))
      } else {
        last <- rbind(last[1:place,],
                      rep(NA, ncol(last)),
                      last[(place + 1):nrow(last),])
      }
      
      rownames(last) <- rownameslist
    }
  }
  
  colours <- last
  colours[,] <- fourCols[4]
  colours <- colours[,!colnames(colours) %in% ignoreCols]
  
  # Find data changes between snapshots
  tables <- find_data_changes(smallsetList, fourCols, FALSE)
  # Remove ignored columns
  if (!is.null(ignoreCols)) {
    for (t in 1:length(tables[[1]])) {
      ignore <-
        ignoreCols[ignoreCols %in% colnames(tables[[1]][[t]]$body$dataset)]
      if (length(ignore) > 0) {
        tables[[1]][[t]] <-
          delete_columns(tables[[1]][[t]], j = ignore)
      }
    }
  }
  tables <-
    lapply(seq(1:length(tables[[1]])), tables, ignoreCols, FUN = retrieve_tables)
  
  # Update colours in visual appearance matrix
  for (t in 1:length(tables)) {
    t_colours <- tables[[t]][[1]]
    rownames(t_colours) <- rownames(tables[[t]][[2]])
    
    for (i in rownames(t_colours)) {
      for (j in colnames(t_colours)) {
        c1 <- t_colours[i, j]
        if (c1 != fourCols[4]) {
          colours[i, j] <- c1
        }
      }
    }
  }
  
  return(colours)
  
}
