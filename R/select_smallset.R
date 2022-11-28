#' Select Smallset
#' @description Selects the rows included in the Smallset.
#' @keywords internal

select_smallset <- function(data,
                            rowCount = 6,
                            rowNums = NULL) {
  # Extract the specified rows and randomly sample the rest
  data$smallsetRowID <- row.names(data)
  smallset2 <-
    sample(subset(data,!(
      as.numeric(data$smallsetRowID) %in% rowNums
    ))$smallsetRowID, rowCount - length(rowNums))
  smallset <- c(as.character(rowNums), smallset2)
  
  # Order the Smallset based on the row number in original dataset
  return(as.character(sort(as.numeric(smallset))))
}
