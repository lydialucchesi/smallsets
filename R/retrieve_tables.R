#' Retrieve snapshot tables
#' @description Retrieves data and colour information from the snapshot tables.
#' @noRd

retrieve_tables <- function(itemNum, smallsetTables, ignoreCols) {
  # Retrieve table colours
  tab1 <-
    as.data.frame(smallsetTables[[1]][[itemNum]]$body$styles$text$color$data)
  
  # Retrieve table data
  tab2 <-
    as.data.frame(smallsetTables[[1]][[itemNum]]$body$dataset)
  if (!is.null(ignoreCols)) {
    tab2 <- tab2[,!colnames(tab2) %in% ignoreCols]
  }
  
  return(list(tab1, tab2))
  
}
