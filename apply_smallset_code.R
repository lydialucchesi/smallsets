source("gen_data.R")

prep_smallset <- function(data, prepCode) {
  
  smallset <- head(data)
  
  source("write_smallset_code.R")
  writeSmallsetCode(scriptName = prepCode)
  
  source("smallset_code.R")
  smallsetList <- applyCode(smallset)
  
  return(smallsetList)
  
}

prep_smallset(data = df, prepCode = "prep_data.R")


