#' Select Smallset
#' @description Selects the rows included in the Smallset.
#' @keywords internal

select_smallset <- function(data,
                            rowCount = 6,
                            rowIDs = NULL) {
  # Extract the specified rows and randomly sample the rest
  data$smallsetRowID <- row.names(data)
  smallset2 <- sample(subset(data,!(as.numeric(data$smallsetRowID) %in% rowIDs))$smallsetRowID, rowCount - length(rowIDs))
  smallset <- c(as.character(rowIDs), smallset2)
  
  # Order the Smallset based on the order in the original dataset
  return(smallset[order(match(smallset, data$smallsetRowID))])
}
