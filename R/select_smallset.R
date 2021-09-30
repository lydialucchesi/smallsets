#' Select smallset
#' @description The function selects the rows included in the smallset.
#' @keywords internal
#' @import "dplyr"

select_smallset <- function(data,
                            rowCount = 6,
                            rowNums = NULL,
                            runBig = FALSE,
                            ignoreCols = ignoreCols) {
  # Randomly sample rows from the original dataset
  if (is.null(rowNums)) {
    data$smallsetRowID <- row.names(data)
    smallset <- dplyr::sample_n(data, size = rowCount)
    smallset <- smallset[,!(names(smallset) %in% ignoreCols)]
  }
  
  # Extract the specified rows and randomly sample the rest
  if (!is.null(rowNums)) {
    data$smallsetRowID <- row.names(data)
    smallset1 <- data[rowNums,]
    smallset2 <-
      dplyr::sample_n(data[-rowNums,], size = (rowCount - length(rowNums)))
    smallset <- rbind(smallset1, smallset2)
    smallset <- smallset[,!(names(smallset) %in% ignoreCols)]
  }
  
  # Order the smallset based on the row number in original dataset
  if (isTRUE(runBig)) {
    smallsetRowIDs <- sort(as.numeric(smallset$smallsetRowID))
    return(smallsetRowIDs)
  } else {
    smallset <- smallset[order(as.numeric(smallset$smallsetRowID)),]
    rownames(smallset) <- smallset$smallsetRowID
    smallset$smallsetRowID <- NULL
    return(smallset)
  }
  
}
