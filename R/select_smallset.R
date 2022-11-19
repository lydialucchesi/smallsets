#' Select smallset
#' @description The function selects the rows included in the smallset.
#' @keywords internal

select_smallset <- function(data,
                            rowCount = 6,
                            rowNums = NULL,
                            ignoreCols = ignoreCols) {
  # Randomly sample rows from the original dataset
  if (is.null(rowNums)) {
    data$smallsetRowID <- row.names(data)
    smallset <- sample(data$smallsetRowID, size = rowCount)
    smallset <- data[c(smallset), ]
    smallset <- smallset[,!(names(smallset) %in% ignoreCols)]
  }
  
  # Extract the specified rows and randomly sample the rest
  if (!is.null(rowNums)) {
    data$smallsetRowID <- row.names(data)
    smallset1 <- data[rowNums,]
    smallset2 <- sample(subset(data, !(as.numeric(data$smallsetRowID) %in% rowNums))$smallsetRowID, rowCount - length(rowNums))
    smallset2 <- data[c(smallset2), ]
    smallset <- rbind(smallset1, smallset2)
    smallset <- smallset[,!(names(smallset) %in% ignoreCols)]
  }
  
  # Order the smallset based on the row number in original dataset
  smallsetRowIDs <- sort(as.numeric(smallset$smallsetRowID))
  return(smallsetRowIDs)
  
}
