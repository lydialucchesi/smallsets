#' Retrieve snapshot tables
#' @description Retrieves data and colour information from the snapshot tables.
#' @keywords internal

retrieve_tables <- function(itemNum, smallsetTables) {
  # Retrieve table colours
  tab1 <-
    as.data.frame(smallsetTables[[1]][[itemNum]]$body$styles$text$color$data)
  
  # Retrieve table data
  tab2 <-
    as.data.frame(smallsetTables[[1]][[itemNum]]$body$dataset)
  
  return(list(tab1, tab2))
  
}
