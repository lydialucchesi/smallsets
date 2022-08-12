#' Prepare smallset
#'
#' @description The function selects a smallset, takes snapshots, and
#'   identifies the changes between snapshots. The output can be passed to
#'   \code{create_timeline} to create a smallset timeline.
#'
#' @param data Data set.
#' @param code R or Python script with data preprocessing code for the data set. Filename extension should be included (e.g., "my_code.R" or "my_code.py").
#' @param dir File path to the data preprocessing code. Default is the working directory.
#' @param rowCount Integer greater than or equal to 5. Number of rows to include
#'   in the smallset.
#' @param rowNums Numeric vector of row numbers. Indicates particular rows from
#'   the data set to be included in the smallset.
#' @param auto 1 or 2. 1 = simple gurobi model selection. 2 = advanced gurobi
#'   model selction.
#' @param runBig TRUE or FALSE. FALSE means preprocessing code will be run on
#'   smallset. TRUE means preprocessing code will be run on the big data set,
#'   and the smallset will be extracted from that output at each snap point.
#' @param ignoreCols Character vector of column names. Indicates which columns
#'   from the data set should not be included in the smallset. Columns in this
#'   vector should usually not be referenced in the data preprocessing code.
#' @param captionTemplateName File name for the caption template.
#' @param captionTemplateDir File path for the caption template.
#' @param captionTemplateAuthor Name of author for the caption template.
#' @import "reticulate"
#' @importFrom tools file_ext
#' @export

prepare_smallset <-
  function(data,
           code,
           dir = getwd(),
           rowCount = 6,
           rowNums = NULL,
           auto = NULL,
           runBig = TRUE,
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
    
    lang <- file_ext(code)
    if (!lang %in% c("R", "py")) {
      stop("Preprocessing code must be in R or Python. Filename extension should be included (e.g., 'my_code.R' or 'my_code.py').")
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
    
    if (!is.null(auto)) {
      if (!requireNamespace("gurobi", quietly = TRUE)) {
        stop(
        "This Smallset selection method uses a gurobi optimization model. 
        Please visit https://www.gurobi.com to obtain a gurobi license (free academic licenses are available) 
        and then install and load the gurobi R package. smallsets will then be able to run the selection model. 
        Otherwise, please visit the help documentation for information about other Smallset selection options."
        )
      } else {
        if (auto == 1) {
          rowNums <-
            run_simple_gurobi(
              data = data,
              code = code,
              dir = dir,
              rowCount = rowCount,
              lang = lang
            )
          if (isTRUE(runBig)) {
            smallset <- rowNums
          } else {
            smallset <- data[rownames(data) %in% rowNums,]
          }
        } else {
          rowNums <-
            run_advanced_gurobi(
              data = data,
              code = code,
              dir = dir,
              rowCount = rowCount,
              lang = lang
            )
          if (isTRUE(runBig)) {
            smallset <- rowNums
          } else {
            smallset <- data[rownames(data) %in% rowNums,]
          }
        }
      }
      
    } else {
      # Generate a smallset
      smallset <- select_smallset(
        data = data,
        rowCount = rowCount,
        rowNums = rowNums,
        runBig = runBig,
        ignoreCols = ignoreCols
      )
    }
    
    # Prepare the preprocessing function that takes snapshots
    resumeLocs <-
      write_smallset_code(
        scriptName = code,
        dir = dir,
        runBig = runBig,
        ignoreCols = ignoreCols,
        smallset = smallset,
        lang = lang
      )
    
    if (lang == "py") {
      source_python(paste0(dir, "/smallset_code.py"))
      
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
    } else {
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
      tempAuthor = captionTemplateAuthor,
      lang = lang
    )
    
    o <- (
      list(
        smallsetTables[[1]],
        captionTemplateName,
        captionTemplateDir,
        resumeLocs,
        smallsetTables[[2]]
      )
    )
    
    oldClass(o) <- c("smallsetSnapshots", class(o))
    
    o
    
  }
