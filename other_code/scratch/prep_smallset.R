#' Prepare smallset
#' 
#' @param data A data set
#' @param prepCode A script to prepare data for use in modelling
#' @param rowCount The number of rows to include in the smallset
#' @param rowNums A numeric vector of row numbers indicating particular rows from the data set to be included in the smallset
#' @param dir A file path for the smallset script
#' @export

prep_smallset <- function(data, prepCode, rowCount = 6, rowNums = NULL, dir = getwd()) {
  
  smallset <- select_smallset(data = data, 
                              rowCount = rowCount, 
                              rowNums = rowNums)
  
  write_smallset_code(scriptName = prepCode, dir = dir)
  
  source(paste0(dir, "/smallset_code.R"))
  smallsetList <- apply_code(smallset)

  return(smallsetList)
  
}





