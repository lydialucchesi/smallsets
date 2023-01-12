#'Smallset Timeline
#'
#'@description Creates a Smallset Timeline to visualise data preprocessing decisions.
#'
#'@param data Dataset that is being preprocessed.
#'@param code R or Python data preprocessing script. Include the filename extension 
#'  (e.g., "my_code.R" or "my_code.py"). If the script is not located in the working 
#'  directory, include the file path.
#'@param rowCount Integer between 5-15 for number of Smallset rows.
#'@param rowSelect NULL, 1, or 2. When NULL, Smallset rows are randomly sampled.
#'  When 1, Smallset rows are selected using the coverage optimisation model. 
#'  When 2, Smallset rows are selected using the coverage + variety optimisation model, 
#'  which has a long run time for large datasets. Options 1 and 2 use the
#'  Gurobi solver and require a Gurobi license. Please visit https://www.gurobi.com to obtain 
#'  a license (free academic licenses are available).
#'@param rowReturn A logical. TRUE prints, to the console, the row numbers 
#' of the rows selected for the Smallset.
#'@param rowNums Numeric vector indicating particular rows from the dataset 
#' to include in the Smallset.
#'@param ignoreCols Character vector of column names indicating which to exclude 
#' from the Smallset. These columns can't be referenced in the data preprocessing code.
#'@param colours Either 1, 2, or 3 for one of the pre-built colour schemes (all are 
#' colourblind friendly and 3 is black/white printer friendly) or a list with 
#' four hex colour codes for same, edit, add, and delete (e.g., list(same = "#E6E3DF", 
#' edit = "#FFC500", add = "#5BA2A6", delete = "#DDC492")).
#'@param altText A logical. TRUE generates alternative text (alt text)
#' for the Smallset Timeline and prints it to the console.
#'@param printedData A logical. TRUE prints data values in the Smallset
#'  snapshots.
#'@param truncateData Integer specifying the number of characters to print for each
#'  data value (results in characters + "..."). Default is NULL, where entire data 
#'  value is printed.
#'@param ghostData A logical. TRUE includes blank spaces where data have
#'  been removed.
#'@param missingDataTints A logical. TRUE plots a lighter colour value to
#'  signal a missing data value.
#'@param font Any font you have installed in R. Default is "sans".
#'@param sizing List of size specifications: column names (columns), caption
#'  text (captions), tile size, (tiles), printed data (data), legend text
#'  (legendText), legend icons (legendIcons), and resume markers (resume). List
#'  of defaults: list(columns = 2.5, captions = 2.5, tiles = .2, data = 2.5,
#'  legendText = 10, legendIcons = 1, resume = .25).
#'@param spacing List of spacing specifications: space below captions
#'  (captionB), space above columns (columnsT), space to the right of Smallset
#'  tables (tablesR), number of Timeline rows (rows), and degree rotation of
#'  column names (columnsDeg). List of defaults: list(captionB = 2, columnsT =
#'  1, tablesR = .5, rows = 1, columnsDeg = 0).
#'@param labelling List of colour specifications for labelling, which includes
#'  column names and printed data. First specification is whether the labels
#'  should be lighter or darker than the tile colours (labelsCol). Second
#'  specification is a value between 0 and 1 for how much lighter or darker it
#'  should be (labelsColDif). Example 1: list(labelsCol = "darker", labelsColDif
#'  = 1) means labels are black. Example 2: list(labelsCol = "lighter",
#'  labelsColDif = 1) means labels are white. Default is list(labelsCol =
#'  "darker", labelsColDif = .5), which is midpoint between tile colours and
#'  black.
#'
#'@details Prior to running this command, you will need to add structured
#'  comments to your R or Python data preprocessing script, providing snapshot
#'  points and captions. \itemize{\item{`# smallsets start mydata` - start
#'  tracking code and take the first data snapshot, where "mydata" is the name
#'  of your data object} \item{`# smallsets snap mydata` - take a data snapshot after the
#'  next line of code} \item{`# smallsets end mydata` - stop tracking code and
#'  take the last data snapshot} \item{Snapshot captions are added between
#'  caption brackets, `caption[...]caption`, at the end of the comments listed
#'  above}}
#'
#'@return A plot.
#'
#'@examples
#'set.seed(145)
#'
#'Smallset_Timeline(
#'   data = mydata,
#'   code = system.file("preprocess_data.R", package = "smallsets")
#')
#'@import patchwork
#'@export

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
                              sizing = list(
                                columns = 2.5,
                                captions = 2.5,
                                tiles = .2,
                                data = 2.5,
                                legendText = 10,
                                legendIcons = 1,
                                resume = .25
                              ),
                              spacing = list(
                                captionB = 2,
                                columnsT = 1,
                                tablesR = .5,
                                rows = 1,
                                columnsDeg = 0
                              ),
                              labelling = list(labelsCol = "darker",
                                               labelsColDif = .5)) {
  # Check that dataset and R/Python preprocessing code were provided
  if (missing(data)) {
    print("Must specify a data set")
  }
  
  if (missing(code)) {
    print("Must specify preprocessing code")
  }
  
  lang <- tools::file_ext(code)
  if (!lang %in% c("R", "py")) {
    stop(
      "Preprocessing code must be in R or Python.
      Include filename extension (e.g., 'my_code.R'
      or 'my_code.py')."
    )
  }
  
  if (inherits(data, "data.table")) {
    print("Converting data object from a
          data table to a data frame.")
    data <- as.data.frame(data)
  }
  else
    if (inherits(data, "tbl_df")) {
      print("Converting data object from a
          tibble to a data frame.")
      data <- as.data.frame(data)
    }
  else
    if (!inherits(data, "data.frame")) {
      stop("Data was not of class
         data frame, data table, or tibble.")
    }
  
  # Check Smallset size
  if (rowCount < 5 | rowCount > 15) {
    stop("Please choose a rowCount between 5-15.")
  }
  
  # Fill in any parameters not specified
  sizing <- set_sizing(sizing)
  spacing <- set_spacing(spacing)
  labelling <- set_labelling(labelling)
  
  # Get four tile colours ready
  if (inherits(colours, "list")) {
    fourCols <- unlist(colours,
                       use.names = FALSE)
  } else {
    fourCols <- unlist(return_scheme(colours),
                       use.names = FALSE)
  }
  
  # Select the Smallset rows
  if (!is.null(rowSelect)) {
    if (!requireNamespace("gurobi", quietly = TRUE)) {
      stop(
        "This Smallset selection method uses a gurobi optimisation model.
        Please visit https://www.gurobi.com to obtain a gurobi license
        (free academic licenses are available) and then install and load
        the gurobi R package. smallsets will then be able to run the
        selection model. Otherwise, please visit the help documentation
        for information about other Smallset selection options."
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
    print(paste0("Smallset rows: ",
                 paste0(smallset, collapse = ", ")))
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
  if (labelling$labelsCol == "darker") {
    colValue2 <- colorspace::darken(fourCols, labelling$labelsColDif)
  } else {
    colValue2 <- colorspace::lighten(fourCols, labelling$labelsColDif)
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
      "Data has not changed.",
      "Data has been edited.",
      "Data has been added.",
      "Data will be deleted."
    )
  if (isTRUE(missingDataTints)) {
    descriptions <- paste0(descriptions, "\nTint is missing data.")
  }
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
  
  # Make each snapshot plot
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
      FUN = plot_snapshots
    )
  # Assemble snapshots into Timeline
  patchedPlots <- ""
  for (s in 1:length(l)) {
    # Adjust plot widths if multiple Timeline rows
    if (spacing$rows > 1) {
      l[[s]] <- l[[s]] + xlim(c(.5, maxDims[1] + .5))
    }
    addPlot <- paste0("l[[", as.character(s), "]] + ")
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
      sizing[["legendIcons"]],
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

  o <- eval(parse(text = patchedPlots))
  oldClass(o) <- c("SmallsetTimeline", class(o))
  return(o)
  
}
