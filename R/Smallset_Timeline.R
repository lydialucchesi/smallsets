#' Smallset Timeline
#'
#' @description Builds a Smallset Timeline to visualise data preprocessing
#'   decisions.
#'
#' @param data Dataset that is being preprocessed.
#' @param code R, R Markdown, Python, or Jupyter Notebook data preprocessing script. 
#'   Include the filename extension (e.g., "my_code.R", "my_code.Rmd", 
#'   "my_code.py", or "my_code.ipynb"). 
#'   If the script is not in the working directory, include the full file path.
#' @param rowCount Integer between 5-15 for number of Smallset rows.
#' @param rowSelect NULL, 1, or 2. If NULL, Smallset rows are randomly sampled.
#'   If 1, Smallset rows are selected using the coverage optimisation model. If
#'   2, Smallset rows are selected using the coverage + variety optimisation
#'   model, which has a long run time for large datasets. Options 1 and 2 use
#'   the Gurobi solver (v9.1.2) and require a Gurobi license. Please visit
#'   https://www.gurobi.com to obtain a license (free academic licenses are
#'   available).
#' @param rowReturn A logical. TRUE prints, to the console, the row numbers of
#'   the rows selected for the Smallset.
#' @param rowNums Numeric vector indicating particular rows from the dataset to
#'   include in the Smallset.
#' @param ignoreCols Character vector of column names indicating which to
#'   exclude from the Smallset. These columns cannot be referenced in the data
#'   preprocessing code.
#' @param colours Either 1, 2, or 3 for one of the pre-built colour schemes (all
#'   are colour-blind-friendly and 3 is black/white-printer-friendly) or a list
#'   with four hex colour codes for keep, edit, add, and delete (e.g., list(keep
#'   = "#E6E3DF", add = "#5BA2A6", delete = "#DDC492", edit = "#FFC500")).
#' @param altText A logical. TRUE generates alternative text (alt text) for the
#'   Smallset Timeline and prints it to the console.
#' @param printedData A logical. TRUE prints data values in the Smallset
#'   snapshots.
#' @param truncateData Integer for the number of characters in each printed data
#'   value. Results in characters plus an ellipsis.
#' @param ghostData A logical. TRUE includes empty tiles where data have been
#'   removed.
#' @param missingDataTints A logical. TRUE plots a lighter colour value for a
#'   missing data value.
#' @param font Any font installed in R. Default is sans.
#' @param sizing \link{sets_sizing} for size specifications.
#' @param spacing \link{sets_spacing} for space specifications.
#' @param labelling \link{sets_labelling} for label specifications.
#'
#' @details Prior to running this command, structured comments with snapshot
#'   instructions must be added to the preprocessing script passed to
#'   \code{code}. See section titled "Structured comments" in 
#'   \code{vignette("smallsets")} or in the 
#'   \href{https://lydialucchesi.github.io/smallsets/articles/smallsets.html#comments}{online user guide}.
#'
#' @return Returns a Smallset Timeline object, which is a plot consisting of
#'   `ggplot` objects assembled with `patchwork`.
#'
#' @examples
#' set.seed(145)
#'
#' Smallset_Timeline(
#'   data = s_data,
#'   code = system.file("s_data_preprocess.R", package = "smallsets")
#' )
#'
#' @import patchwork 
#' @importFrom callr r
#' @importFrom knitr purl
#' @importFrom rmarkdown convert_ipynb
#' @export

Smallset_Timeline <- function(data,
                              code,
                              rowCount = 5,
                              rowSelect = NULL,
                              rowReturn = FALSE,
                              rowNums = NULL,
                              ignoreCols = NULL,
                              colours = 1,
                              altText = FALSE,
                              printedData = FALSE,
                              truncateData = NULL,
                              ghostData = TRUE,
                              missingDataTints = FALSE,
                              font = "sans",
                              sizing = sets_sizing(),
                              spacing = sets_spacing(),
                              labelling = sets_labelling()
                              ) {
  # Check that dataset and R/Python preprocessing code were provided
  if (missing(data)) {
    stop("Must specify data. See ?Smallset_Timeline.")
  }
  
  if (missing(code)) {
    stop("Must specify code. See ?Smallset_Timeline.")
  }
  
  lang <- tools::file_ext(code)
  if (!lang %in% c("R", "Rmd", "py", "ipynb")) {
    stop(
      "Preprocessing code must be in an R, Rmd, py, or, ipynb file.
       Include filename extension (e.g., 'my_code.R', 'my_code.Rmd',
      'my_code.py', or 'my_code.ipynb')."
    )
  }
  
  rmdSwitch <- NULL
  # Convert Rmd / ipynb to R / Python
  if (lang == "Rmd" | lang == "ipynb") {
    rmdSwitch <- TRUE
    lang <- ifelse(lang == "Rmd", "R", "py")
    
    # First convert ipynb to Rmd
    if (lang == "py") {
      ipynb2Rmd <-
        tempfile(pattern = "ipynb2Rmd", fileext = paste0(".Rmd"))
      rmarkdown::convert_ipynb(paste0(getwd(), "/", code), output = ipynb2Rmd)
      code <- ipynb2Rmd
    }
    
    # Convert Rmd file to R or Python file
    converted_file <-
      tempfile(pattern = "rmd2R", fileext = paste0(".", lang))
    writeRmd2 <- callr::r(
      function(code, converted_file)
        knitr::purl(
          input = code,
          output = converted_file,
          quiet = TRUE,
          documentation = 0
        ),
      args = list(code, converted_file)
    )
    
    # Extra formatting tasks if Python
    if (lang == "py") {
      
      # Remove double hash for comments
      with_comments <- readLines(converted_file)
      without_comments <- gsub("^.{0,3}", "", with_comments)
      
      # Remove extra line added after each non-empty line
      for (i in 1:length(without_comments)) {
        if (without_comments[i] != "") {
            without_comments <- without_comments[-(i + 1)]
        }
        if (i == length(without_comments)) {
          break
        }
      }
      
      converted_file <-
        tempfile(pattern = "ipynb2py", fileext = paste0(".py"))
      fileConn <- file(converted_file)
      writeLines(without_comments, fileConn)
      close(fileConn)
    }
    
    code <- converted_file
  }
  
  if (inherits(data, "data.table")) {
    warning("Converting data object from a data table to a data frame.")
    data <- as.data.frame(data)
  }
  if (inherits(data, "tbl_df")) {
      warning("Converting data object from a tibble to a data frame.")
      data <- as.data.frame(data)
  }
  if (!inherits(data, "data.frame")) {
      stop("Data was not of class data frame, data table, or tibble.")
  }
  
  # Check Smallset size
  if (rowCount < 5 | rowCount > 15) {
    stop("Please choose a rowCount between 5-15.")
  }
  
  # Get four tile colours ready
  if (inherits(colours, "list")) {
    k <- colours$keep
    a <- colours$add
    d <- colours$delete
    e <- colours$edit
    if (length(c(k, a, d, e)) != 4) {
      stop("When specifying custom colours, 
      must specify four colours for 
      keep, add, delete, and edit. 
      See ?Smallset_Timeline.")
    }
    fourCols <- c(k, a, d, e)
  } else {
    fourCols <- unlist(return_scheme(colours),
                       use.names = FALSE)
  }
  
  # Select the Smallset rows
  if (!is.null(rowSelect)) {
    if (!requireNamespace("gurobi", quietly = TRUE)) {
      stop(
        "This Smallset selection method uses the Gurobi solver. 
        Please visit <https://www.gurobi.com> to get a Gurobi license. 
        Then install and load the gurobi R package. Otherwise, leave NULL."
      )
    } else {
      # Use an optimisation algorithm
      if (rowSelect == 1) {
        smallset <-
          run_simple_gurobi(data, code, rowCount, lang, fourCols)
      } else {
        smallset <-
          run_advanced_gurobi(data, code, rowCount, lang, fourCols)
      }
    }
  } else {
    # Use random sampling and/or manual selection
    smallset <- select_smallset(data, rowCount, rowNums)
  }
  if (isTRUE(rowReturn)) {
    cat(paste0("Smallset rows: ", paste0(smallset, collapse = ", ")))
  }

  # Write preprocessing function with snapshots
  output <- write_smallset_code(code, smallset, lang)
  
  # Subset data to columns of interest
  if (!is.null(ignoreCols)) {
    data <- data[,!(names(data) %in% ignoreCols)]
  }
  
  # Run function to take snapshots
  apply_code <- NULL
  if (lang == "py") {
    reticulate::source_python(output[[3]])
  } else {
    source(output[[3]], local = TRUE)
  }
  # Source function
  smallsetList <- apply_code(data)
  for (i in 1:length(smallsetList)) {
    smallsetList[[i]] <-
      smallsetList[[i]][!(row.names(smallsetList[[i]]) %in% c("NA")),]
  }
  # Delete temp file
  unlink(output[[3]])
  
  # Find data differences between snapshots
  smallsetTables <- find_data_changes(smallsetList, fourCols, altText)
  items <- seq(1, length(smallsetTables[[1]]), 1)
  
  # Prepare snapshot label colours
  if (labelling$labelCol == "darker") {
    colValue2 <- colorspace::darken(fourCols, labelling$labelColDif)
  } else {
    colValue2 <- colorspace::lighten(fourCols, labelling$labelColDif)
  }
  accents <-
    data.frame(colValue = fourCols,
               colValue2 = colValue2)
  
  # Find which colours are present in Timeline
  colsPresent <- c()
  tables <- lapply(items, smallsetTables, FUN = retrieve_tables)
  for (t in 1:length(tables)) {
    colsPresent <-
      c(colsPresent, unique(as.vector(t(tables[[t]][[1]]))))
  }
  colsPresent <- unique(colsPresent)
  
  # Prepare colour legend
  descriptions <-
    c(
      "Keep   ",
      "Add   ",
      "Delete   ",
      "Edit   "
    )
  legendDF <-
    data.frame(colValue = fourCols, description = descriptions)
  legendDF <- subset(legendDF, legendDF$colValue %in% colsPresent)
  
  # Insert ghost data rows/columns
  if (isTRUE(ghostData)) {
    ghostDFs <- retrieve_tables(1, smallsetTables)
    ghostDF1 <- ghostDFs[[1]]
    ghostDF1[] <- "#FFFFFF"
    ghostDF2 <- ghostDFs[[2]]
    ghostDF2[] <- ""
    row.names(ghostDF1) <- row.names(ghostDF2)
    
    extTables <- lapply(items,
                        ghostDF1,
                        ghostDF2,
                        smallsetTables,
                        FUN = add_ghost_data)
  } else {
    extTables <- lapply(items,
                        smallsetTables,
                        FUN = retrieve_tables)
  }
  
  # Find Timeline dimensions
  maxDims <- retrieve_dimensions(extTables)
  
  # Make snapshot and resume marker plots
  l <-
    lapply(
      items,
      accents,
      extTables,
      fourCols,
      ghostData,
      legendDF,
      maxDims,
      missingDataTints,
      output,
      printedData,
      sizing,
      smallsetTables,
      spacing,
      font,
      truncateData,
      FUN = build_plot
    )
  
  # Expand list of plots
  plots <- list()
  p <- 1
  for (i in 1:length(l)) {
    if (length(l[[i]]) == 2) {
      plots[[p]] <- l[[i]][[1]]
      p <- p + 1
      plots[[p]] <- l[[i]][[2]]
      p <- p + 1
    } else {
      plots[[p]] <- l[[i]]
      p <- p + 1
    }
  }
  
  # Finalise the plots with captions and standard dimensions
  items <- 1:length(plots)
  l <-
    lapply(
      items,
      plots,
      output,
      maxDims,
      font,
      sizing,
      spacing,
      FUN = finalise_plot
    )
  
  # Assemble plots into Timeline
  patchedPlots <- ""
  for (p in 1:length(l)) {
    addPlot <- paste0("l[[", as.character(p), "]] + ")
    patchedPlots <- paste0(patchedPlots, addPlot)
  }
  
  # Add design info
  patchedPlots <-
    paste0(
      patchedPlots,
      "plot_layout(nrow = ",
      as.character(spacing$rows),
      ", guides = 'collect')",
      " & theme(text = element_text(family = '",
      font,
      "', colour = 'black'),",
      "legend.key.size = unit(",
      sizing[["icons"]],
      ", 'line'), 
      legend.position = 'bottom', 
      legend.title = element_blank(),
      legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'))"
    )
  
  # Generate alt text for the Timeline
  if (isTRUE(altText)) {
    generate_alt_text(smallsetTables[[1]],
                      fourCols,
                      legendDF,
                      smallsetTables[[2]],
                      l,
                      printedData,
                      ghostData)
  }

  # Delete tempfiles from file conversion
  if (isTRUE(rmdSwitch)) {
    if (lang == "py") {
      unlink(ipynb2Rmd)
      unlink(converted_file)
    } else {
      unlink(code)
    }
  }
  
  o <- eval(parse(text = patchedPlots))
  oldClass(o) <- c("Smallset Timeline", class(o))
  return(o)
  
}
