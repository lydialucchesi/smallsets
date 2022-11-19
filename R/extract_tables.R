#' Extract snapshot tables
#' @description The function extracts data and colour information from the
#'   flextable objects.
#' @keywords internal

extract_tables <- function(itemNum, smallsetTables) {
  tab1 <-
    as.data.frame(smallsetTables[[1]][[itemNum]]$body$styles$text$color$data)
  
  tab2 <-
    as.data.frame(smallsetTables[[1]][[itemNum]]$body$dataset)
  
  return(list(tab1, tab2))
  
}
