#' Add ghost data
#' @description The function inserts blank squares where data have been
#'   removed.
#' @keywords internal

add_ghost_data <-
  function(itemNum, ghostDF1, ghostDF2, snapshotList) {
    tab1 <-
      as.data.frame(snapshotList[[1]][[itemNum]]$body$styles$text$color$data)
    tab2 <-
      as.data.frame(snapshotList[[1]][[itemNum]]$body$dataset)
    
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
    
    difCols <- setdiff(colnames(ghostDF1), colnames(tab1))
    if (length(difCols) > 0) {
      tab1 <- cbind(tab1, ghostDF1[row.names(tab1), difCols])
      tab1 <- tab1[names(ghostDF1)]
    }
    
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
    
    difCols <- setdiff(colnames(ghostDF2), colnames(tab2))
    if (length(difCols) > 0) {
      tab2 <- cbind(tab2, ghostDF2[row.names(tab2), difCols])
      tab2 <- tab2[names(ghostDF2)]
    }
    
    return(list(tab1, tab2))
    
  }
