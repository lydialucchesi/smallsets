#' Select Smallset
#' @description Selects the rows included in the Smallset.
#' @noRd

select_smallset <- function(data,
                            rowCount = 6,
                            rowIDs = NULL,
                            lang = lang) {
  # Extract the specified rows and randomly sample the rest
  if (lang == "R") {
    data$smallsetRowID <- rownames(data)
  } else {
    data$smallsetRowID <- as.numeric(rownames(data)) - 1
  }
  smallset2 <-
    sample(subset(data,!(data$smallsetRowID %in% rowIDs))$smallsetRowID, rowCount - length(rowIDs))
  smallset <- c(as.character(rowIDs), smallset2)
  
  # Order the Smallset based on the order in the original dataset
  return(smallset[order(match(smallset, data$smallsetRowID))])
}
