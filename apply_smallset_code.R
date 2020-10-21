source("select_smallset.R")

# is it okay to use same argument names in prep_smallset as in select_smallset?

prep_smallset <- function(data, prepCode, size = 6, rowNums = NULL, startCol = "black", changeCol = "cornflowerblue") {
  
  smallset <- select_smallset(data = data, size = size, rowNums = rowNums)
  
  source("write_smallset_code.R")
  writeSmallsetCode(scriptName = prepCode)
  
  source("smallset_code.R")
  smallsetList <- applyCode(smallset)
  
  return(smallsetList)
  
}





