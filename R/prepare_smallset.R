#' Prepare smallset
#'
#' @description The function selects a smallset, takes snapshots, and
#'   identifies the changes between snapshots. The output can be passed to
#'   \code{create_timeline} to create a smallset timeline.
#'
#' @param data Data set.
#' @param code Script with data preprocessing code for data set.
#' @param dir File path to data preprocessing code.
#' @param rowCount Integer greater than or equal to 5. Number of rows to include
#'   in the smallset.
#' @param rowNums Numeric vector of row numbers. Indicates particular rows from
#'   the data set to be included in the smallset.
#' @param runBig TRUE or FALSE. FALSE means preprocessing code will be run on
#'   smallset. TRUE means preprocessing code will be run on the big data set,
#'   and the smallset will be extracted from that output at each snap point.
#' @param ignoreCols Character vector of column names. Indicates which columns
#'   from the data set should not be included in the smallset. Columns in this
#'   vector should usually not be referenced in the data preprocessing code.
#' @param captionTemplateName File name for the caption template.
#' @param captionTemplateDir File path for the caption template.
#' @param captionTemplateAuthor Name of author for the caption template.
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
    if (missing(data)) {
      print("Must specify a data set. See data argument in ?prepare_smallset.")
    }
    
    if (missing(code)) {
      print("Must specify preprocessing code. See code argument in ?prepare_smallset.")
    }
    
    # Make sure data is of class data frame
    if (class(data)[1] == "data.table") {
      print(
        "Converting data object from class data.table to data.frame with as.data.frame(data)."
      )
      data <- as.data.frame(data)
    }
    if (class(data)[1] == "tbl_df") {
      print("Converting data object from class tibble to data.frame with as.data.frame(data).")
      data <- as.data.frame(data)
    }
    if (class(data) != "data.frame") {
      stop("Data was not of class data frame, data table, or tibble.")
    }
    
    # Generate a smallset
    smallset <- select_smallset(
      data = data,
      rowCount = rowCount,
      rowNums = rowNums,
      runBig = runBig,
      ignoreCols = ignoreCols
    )
    
    # Prepare the preprocessing function that takes snapshots
    resumeLocs <-
      write_smallset_code(
        scriptName = code,
        dir = dir,
        runBig = runBig,
        smallset = smallset
      )
    
    source(paste0(dir, "/smallset_code.R"))
    
    # Apply the preprocessing function
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
    
    # Return summary information related to the above tasks
    print(paste0("Summary: ", as.character(length(smallsetList)), " snapshots taken"))
    print("First snapshot:")
    print(smallsetList[[1]])
    print("Last snapshot:")
    print(smallsetList[[length(smallsetList)]])
    
    # Identify differences between snapshots
    smallsetTables <- highlight_changes(
      smallsetList = smallsetList,
      tempName = captionTemplateName,
      tempDir = captionTemplateDir,
      tempAuthor = captionTemplateAuthor
    )
    
    o <- (list(
      smallsetTables,
      captionTemplateName,
      captionTemplateDir,
      resumeLocs
    ))
    
    oldClass(o) <- c("smallsetSnapshots", class(o))
    
    o
    
  }
