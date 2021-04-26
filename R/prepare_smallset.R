#' Prepare smallset
#'
#' @param data A data set
#' @param code A script to prepare data for use in modelling
#' @param rowCount The number of rows to include in the smallset
#' @param rowNums A numeric vector of row numbers indicating particular rows from the data set to be included in the smallset
#' @param runBig Default is FALSE (preprocessing code is run on smallset) - if TRUE, preprocessing code is run on the original dataset and smallset snapshots are extracted from that
#' @param ignoreCols A vector of column names that you do not want included in the timeline (cannot be referenced in preprocessing code)
#' @param dir A file path for the smallset script
#' @export

prepare_smallset <-
  function(data,
           code,
           rowCount = 6,
           rowNums = NULL,
           runBig = FALSE,
           ignoreCols = NULL,
           dir = getwd()) {
    smallset <- select_smallset(
      data = data,
      rowCount = rowCount,
      rowNums = rowNums,
      runBig = runBig,
      ignoreCols = ignoreCols
    )
    
    resumeLocs <-
      write_smallset_code(
        scriptName = code,
        dir = dir,
        runBig = runBig,
        smallset = smallset
      )
    
    source(paste0(dir, "/smallset_code.R"))
    
    if (isTRUE(runBig)) {
      if (!is.null(ignoreCols)) {
        data <- data[,!(names(data) %in% ignoreCols)]
      }
      smallsetList <- apply_code(data)
      for (i in 1:length(smallsetList)) {
        smallsetList[[i]] <-
          smallsetList[[i]][!(row.names(smallsetList[[i]]) %in% c("NA")),]
      }
    } else {
      smallsetList <- apply_code(smallset)
    }
    
    return(list(smallsetList, resumeLocs))
    
  }
