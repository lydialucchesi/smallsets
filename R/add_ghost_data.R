#' Add ghost data
#' @description Adds ghost data (blank squares) where data have been removed.
#' @noRd

add_ghost_data <-
  function(itemNum, ghostDF1, ghostDF2, smallsetTables, ignoreCols) {
    # Retrieve colour and data tables
    tabs <- retrieve_tables(itemNum = itemNum, 
                            smallsetTables = smallsetTables,
                            ignoreCols = ignoreCols)
    tab1 <- tabs[[1]]
    tab1[] <- lapply(tab1, as.character)
    tab2 <- tabs[[2]]
    tab2[] <- lapply(tab2, as.character)
    
    # Insert ghost rows in colour data frame
    row.names(tab1) <- row.names(tab2)
    difRows <- setdiff(row.names(ghostDF1), row.names(tab1))
    if (length(difRows) > 0) {
      newColCheck <- setdiff(colnames(tab1), colnames(ghostDF1))
      if (length(newColCheck > 0)) {
        for (c in 1:length(newColCheck)) {
          ghostDF1[, paste0(newColCheck[c])] <- "#FFFFFF"
        }
      }
      tab1 <- rbind(tab1, ghostDF1[difRows, colnames(tab1)])
      tab1 <- tab1[match(rownames(ghostDF1), rownames(tab1)), ]
    }
    
    # Insert ghost columns in colour data frame
    difCols <- setdiff(colnames(ghostDF1), colnames(tab1))
    if (length(difCols) > 0) {
      ghostDF1[row.names(tab1), difCols]
      tab1 <- cbind(tab1, ghostDF1[row.names(tab1), difCols, drop = FALSE])
      newNames <- setdiff(colnames(tab1), colnames(ghostDF1))
      newNames <- c(names(ghostDF1), newNames)
      tab1 <- tab1[newNames]
      colnames(tab1)[colnames(tab1) %in% difCols] <- " "
    }
    
    # Insert ghost rows in data data frame
    difRows <- setdiff(row.names(ghostDF2), row.names(tab2))
    if (length(difRows) > 0) {
      newColCheck <- setdiff(colnames(tab2), colnames(ghostDF2))
      if (length(newColCheck > 0)) {
        for (c in 1:length(newColCheck)) {
          ghostDF2[, paste0(newColCheck[c])] <- ""
        }
      }
      tab2 <- rbind(tab2, ghostDF2[difRows, colnames(tab2)])
      tab2 <- tab2[match(rownames(ghostDF2), rownames(tab2)), ]
    }
    
    # Insert ghost columns in data data frame
    difCols <- setdiff(colnames(ghostDF2), colnames(tab2))
    if (length(difCols) > 0) {
      tab2 <- cbind(tab2, ghostDF2[row.names(tab2), difCols, drop = FALSE])
      newNames <- setdiff(colnames(tab2), colnames(ghostDF2))
      newNames <- c(names(ghostDF2), newNames)
      tab2 <- tab2[newNames]
      colnames(tab2)[colnames(tab2) %in% difCols] <- " "
    }
    
    return(list(tab1, tab2))
    
  }
