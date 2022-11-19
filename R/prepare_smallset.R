#' Prepare Smallset
#' @description Prepares the Smallset.
#' @keywords internal
#' @import "reticulate"

prepare_smallset <-
  function(data,
           code,
           dir,
           rowCount,
           rowNums,
           auto,
           runBig,
           ignoreCols) {
    if (missing(data)) {
      print("Must specify a data set")
    }
    
    if (missing(code)) {
      print("Must specify preprocessing code")
    }
    
    lang <- tools::file_ext(code)
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
    output <-
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
    print(paste0(as.character(length(smallsetList)), " snapshots taken"))
    
    # Identify differences between snapshots
    tables <- list()
    altTextInfo <- list()
    smallsetTables <-
      find_data_changes(smallsetList = smallsetList,
                        tables = tables,
                        altText = TRUE,
                        altTextInfo = altTextInfo)
    
    o <- (
      list(
        smallsetTables[[1]],
        smallsetTables[[2]],
        output[[1]],
        output[[2]]
      )
    )
    
    oldClass(o) <- c("smallsetSnapshots", class(o))
    
    o
    
  }
