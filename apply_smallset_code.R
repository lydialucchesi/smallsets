source("select_smallset.R")

# is it okay to use same argument names in prep_smallset as in select_smallset?

prep_smallset <- function(data, prepCode, rowCount = 6, rowNums = NULL) {
  
  smallset <- select_smallset(data = data, 
                              rowCount = rowCount, 
                              rowNums = rowNums)
  
  source("write_smallset_code.R")
  writeSmallsetCode(scriptName = prepCode)
  
  source("smallset_code.R")
  smallsetList <- applyCode(smallset)

  return(smallsetList)
  
}





