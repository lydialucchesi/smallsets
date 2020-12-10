#' Prepare smallset
#'
#' @param data A data set or a list of data sets. Index name must be same as data frame name (list(df1 = df1)).
#' @param code A script to prepare data for use in modelling
#' @param rowCount The number of rows to include in the smallset
#' @param rowNums A numeric vector of row numbers indicating particular rows from the data set to be included in the smallset
#' @param dir A file path for the smallset script
#' @export

prepare_smallset <-
  function(data,
           code,
           rowCount = 6,
           rowNums = NULL,
           dir = getwd()) {
    if (class(data) != "list") {
      dataList <- list(data)
    } else {
      dataList <- data
    }
    
    # fix this so that you can specify one set of rowNums and others are randomly chosen
    if (!is.list(rowCount)) {
      rowCount <- rep(list(rowCount), length(dataList))
    }
    
    # fix this so that you can specify one set of rowNums and others are randomly chosen
    if (!is.list(rowNums)) {
      rowNums <- rep(list(rowNums), length(dataList))
    }
    
    dataNum <- 1:length(dataList)
    
    smallsets <-
      lapply(dataNum, dataList, rowCount, rowNums, FUN = select_smallset)
    
    if (length(dataList) == 1) {
      dataNames <- NULL
    } else {
      dataNames <- names(dataList)
    }
    
    write_smallset_code(scriptName = code,
                        dir = dir,
                        dataNames = dataNames)
    
    source(paste0(dir, "/smallset_code.R"))
    
    if (is.null(dataNames)) {
      smallsetList <- apply_code(smallsets[[1]])
    } else {
      smallsetArgs <- c()
      for (s in 1:length(dataNames)) {
        addArg <- paste0("smallsets[[", as.character(s), "]]")
        smallsetArgs <- c(smallsetArgs, addArg)
      }
      smallsetArgs <- paste(smallsetArgs, collapse = ", ")

      smallsetList <-
        eval(parse(text = paste0("apply_code(", smallsetArgs, ")")))
    }
    
    return(smallsetList)
    
  }

