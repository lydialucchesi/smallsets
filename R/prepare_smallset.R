#' Prepare smallset
#'
#' @param data A data set
#' @param code A script to prepare data for use in modelling
#' @param dir A file path for the smallset script
#' @param rowCount The number of rows to include in the smallset
#' @param rowNums A numeric vector of row numbers indicating particular rows from the data set to be included in the smallset
#' @param runBig Default is FALSE (preprocessing code is run on smallset) - if TRUE, preprocessing code is run on the original dataset and smallset snapshots are extracted from that
#' @param ignoreCols A vector of column names that you do not want included in the timeline (cannot be referenced in preprocessing code)
#' @param captionTemplateName A file name for the caption .Rmd template
#' @param captionTemplateDir A file path for the caption .Rmd template
#' @param captionTemplateAuthor The author's name for the caption .Rmd file
#' @export

prepare_smallset <-
  function(data,
           code,
           dir = getwd(),
           rowCount = 6,
           rowNums = NULL,
           runBig = FALSE,
           ignoreCols = NULL,
           captionTemplateName = "captionTemplate",
           captionTemplateDir = getwd(),
           captionTemplateAuthor = NULL) {
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
        data <- data[, !(names(data) %in% ignoreCols)]
      }
      smallsetList <- apply_code(data)
      for (i in 1:length(smallsetList)) {
        smallsetList[[i]] <-
          smallsetList[[i]][!(row.names(smallsetList[[i]]) %in% c("NA")), ]
      }
    } else {
      smallsetList <- apply_code(smallset)
    }
    
    print(paste0("Summary: ", as.character(length(smallsetList)), " snapshots taken"))
    print("First snapshot:")
    print(smallsetList[[1]])
    print("Last snapshot:")
    print(smallsetList[[length(smallsetList)]])
    
    smallsetTables <- highlight_changes(
      smallsetList = smallsetList,
      tempName = captionTemplateName,
      tempDir = captionTemplateDir,
      tempAuthor = captionTemplateAuthor
    )
    
    return(list(
      smallsetTables,
      captionTemplateName,
      captionTemplateDir,
      resumeLocs
    ))
    
  }
