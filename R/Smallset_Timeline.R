#' Smallset Timeline
#'
#'@description This is the command for creating a Smallset Timeline to visualise
#' data preprocessing decisions.
#'
#'@param data The dataset that is being preprocessed.
#'@param code The R or Python data preprocessing script.
#'   Filename extension should be included (e.g., "my_code.R" or "my_code.py").
#'@param dir File path to the data preprocessing code. Default is the working directory.
#'@param rowCount Integer between 5-15 (number rows in the Smallset).
#'@param rowNums Numeric vector of row numbers. Indicates particular rows from
#'   the data set to be included in the Smallset.
#'@param autoSelect 1, 2, or NULL (default). 1 = select Smallset rows using the coverage optimisation model.
#' 2 = select Smallset rows using the coverage+variety optimisation model (can take a long time to run).
#' These optimisation problems are solved using Gurobi. Please visit https://www.gurobi.com to obtain
#' a gurobi license (free academic licenses are available) in order to use these selection methods. Otherwise,
#' leave auto = NULL, and the rows will be randomly sampled from the dataset.
#'@param ignoreCols Character vector of column names. Indicates columns
#'   from the dataset to exclude from the Smallset. These columns can't be
#'   referenced in the data preprocessing code.
#'@param colours Either one of the pre-built colour schemes ("colScheme1", "colScheme2", or
#' "colScheme3") or a list with four hex colour codes for same, edit, add, and delete
#' (e.g., list(same = "#E6E3DF", edit = "#FFC500", add = "#5BA2A6", delete = "#DDC492")).
#'@param printedData TRUE or FALSE. TRUE prints data values in the Smallset snapshots.
#'@param truncateData Integer specifying width of data
#'   value in each table cell (results in width + "...").
#'   Default is NULL, where entire data value is printed.
#'@param ghostData TRUE or FALSE. TRUE includes blank spaces where data have
#'   been removed.
#'@param missingDataTints TRUE or FALSE. TRUE plots a lighter colour value to signal
#'   a missing data value.
#'@param timelineFont Any font you have installed in R. Default is "sans".
#'@param sizing List of size specifications: column names (columns), caption text (captions),
#' tile size, (tiles), printed data (data), legend text (legendText), legend icons (legendIcons),
#' and resume markers (resume).
#' List of defaults: list(columns = 3, captions = 3, tiles = .1, data = 2.5, legendText = 10,
#' legendIcons = 1, resume = .25).
#'@param spacing List of spacing specifications: space below captions (captionB),
#' space above columns (columnsT), space to the right of Smallset tables (tablesR),
#' number of Timeline rows (rows), and degree rotation of column names (columnsDeg). List of
#' defaults: list(captionB = 3, columnsT = 1, tablesR = .5, rows = 1, columnsDeg = 0).
#'@param labelling List of colour specifications for labelling, which includes column names and 
#'printed data. First specification is whether the labels should be lighter or 
#'darker than the tile colours (labelsCol). Second specification is a value between 0 and 1 for 
#'how much lighter or darker it should be (labelsColDif). Example 1: list(labelsCol = "darker", labelsColDif = 1) means 
#'labels are black. Example 2: list(labelsCol = "lighter", labelsColDif = 1) means labels are white.
#'Example 3: list(labelsCol = "lighter", labelsColDif = 0) means labels are same colour as tile colours. 
#'Example 4: list(labelsCol = "lighter", labelsColDif = .5) means labels are midpoint between white 
#'and tile colours. Default is list(labelsCol = "darker", labelsColDif = .5), 
#'which is midpoint between tile colours and black.
#'
#'@details Prior to running this command,
#' you will need to add structured comments to your R or Python data preprocessing script,
#' providing snapshot points and captions.
#'\itemize{
#'\item{`# start smallset mydata` - start tracking code and take the first 
#'data snapshot, where "mydata" is the name of your data object}
#'\item{`# snap mydata` - take a data snapshot after the next line of code}
#'\item{`# end smallset mydata` - stop tracking code and take the last data snapshot}
#'\item{Snapshot captions are added between caption brackets, `caption[...]caption`, 
#'at the end of the comments listed above}}
#'
#'@return A plot.
#'
#'@examples
#'set.seed(107)
#'
#'data(mydata)
#'
#'Smallset_Timeline(
#'   data = mydata,
#'   code = system.file("preprocess_data.R", package = "smallsets")
#')
#'
#'@import "patchwork"
#'@export

Smallset_Timeline <- function(data,
                              code,
                              dir = getwd(),
                              rowCount = 6,
                              rowNums = NULL,
                              autoSelect = NULL,
                              ignoreCols = NULL,
                              colours = "colScheme1",
                              printedData = FALSE,
                              truncateData = NULL,
                              ghostData = TRUE,
                              missingDataTints = FALSE,
                              timelineFont = "sans",
                              sizing = list(
                                columns = 3,
                                captions = 3,
                                tiles = .1,
                                data = 2.5,
                                legendText = 10,
                                legendIcons = 1,
                                resume = .25
                              ),
                              spacing = list(
                                captionB = 3,
                                columnsT = 1,
                                tablesR = .5,
                                rows = 1,
                                columnsDeg = 0
                              ),
                              labelling = list(
                                labelsCol = "darker",
                                labelsColDif = .5
                              )
                              ) {
  if (missing(data)) {
    print("Must specify a data set")
  }
  
  if (missing(code)) {
    print("Must specify preprocessing code")
  }
  
  lang <- tools::file_ext(code)
  if (!lang %in% c("R", "py")) {
    stop(
      "Preprocessing code must be in R or Python. Filename extension should be included (e.g., 'my_code.R' or 'my_code.py')."
    )
  }
  
  # Make sure data is of class data frame
  if (class(data)[1] == "data.table") {
    print("Converting data object from a data table to a data frame.")
    data <- as.data.frame(data)
  }
  if (class(data)[1] == "tbl_df") {
    print("Converting data object from class tibble to a data frame.")
    data <- as.data.frame(data)
  }
  if (class(data) != "data.frame") {
    stop("Data was not of class data frame, data table, or tibble.")
  }
  
  # Fill in any sizing, spacing, and labelling parameters not specified
  sizing <- set_sizing(sizing = sizing)
  spacing <- set_spacing(spacing = spacing)
  labelling <- set_labelling(labelling = labelling)
  
  # Select the Smallset rows
  if (!is.null(autoSelect)) {
    if (!requireNamespace("gurobi", quietly = TRUE)) {
      stop(
        "This Smallset selection method uses a gurobi optimisation model.
        Please visit https://www.gurobi.com to obtain a gurobi license (free academic licenses are available)
        and then install and load the gurobi R package. smallsets will then be able to run the selection model.
        Otherwise, please visit the help documentation for information about other Smallset selection options."
      )
    } else {
      if (autoSelect == 1) {
        rowNums <-
          run_simple_gurobi(
            data = data,
            code = code,
            dir = dir,
            rowCount = rowCount,
            lang = lang
          )
        
        smallset <- rowNums
      } else {
        rowNums <-
          run_advanced_gurobi(
            data = data,
            code = code,
            dir = dir,
            rowCount = rowCount,
            lang = lang
          )
        
        smallset <- rowNums
        
      }
    }
    
  } else {
    smallset <- select_smallset(
      data = data,
      rowCount = rowCount,
      rowNums = rowNums,
      ignoreCols = ignoreCols
    )
  }
  
  # Prepare the preprocessing function that takes snapshots
  output <-
    write_smallset_code(
      scriptName = code,
      dir = dir,
      ignoreCols = ignoreCols,
      smallset = smallset,
      lang = lang
    )
  
  # Apply the preprocessing function
  if (lang == "py") {
    source_python(paste0(dir, "/smallset_code.py"))
    if (!is.null(ignoreCols)) {
      data <- data[,!(names(data) %in% ignoreCols)]
    }
    smallsetList <- apply_code(data)
    for (i in 1:length(smallsetList)) {
      smallsetList[[i]] <-
        smallsetList[[i]][!(row.names(smallsetList[[i]]) %in% c("NA")),]
    }
    
  } else {
    source(paste0(dir, "/smallset_code.R"))
    if (!is.null(ignoreCols)) {
      data <- data[,!(names(data) %in% ignoreCols)]
    }
    smallsetList <- apply_code(data)
    for (i in 1:length(smallsetList)) {
      smallsetList[[i]] <-
        smallsetList[[i]][!(row.names(smallsetList[[i]]) %in% c("NA")),]
    }
  }
  
  # Print Smallset and snapshot information to console
  print(paste0("Selected Smallset rows: ", paste0(smallset, collapse = ", ")))
  print(paste0("Number of snapshots: ", as.character(length(smallsetList))))
  
  # Identify data differences between snapshots
  tables <- list()
  altTextInfo <- list()
  smallsetTables <-
    find_data_changes(
      smallsetList = smallsetList,
      tables = tables,
      altText = TRUE,
      altTextInfo = altTextInfo
    )
  items <- seq(1, length(smallsetTables[[1]]), 1)
  
  # Get four colours ready
  colClass <- class(colours)
  
  if (colClass == "character") {
    chosenScheme <- return_scheme(colScheme = colours)
    same <- chosenScheme$same
    edit <- chosenScheme$edit
    add <- chosenScheme$add
    delete <- chosenScheme$delete
  } else {
    same <- colours$same
    edit <- colours$edit
    add <- colours$add
    delete <- colours$delete
  }
  
  tileColours <- data.frame(colValue = c(same, edit, add, delete))
  
  for (i in 1:length(smallsetTables[[1]])) {
    temp <- smallsetTables[[1]][[i]]$body$styles$text$color$data
    for (c in colnames(temp)) {
      temp[, c] <- replace(temp[, c], temp[, c] == "#808080", same)
      temp[, c] <- replace(temp[, c], temp[, c] == "#008000", edit)
      temp[, c] <- replace(temp[, c], temp[, c] == "#0000FF", add)
      temp[, c] <-
        replace(temp[, c], temp[, c] == "#FF0000", delete)
    }
    smallsetTables[[1]][[i]]$body$styles$text$color$data <- temp
  }
  
  # Prepare timeline accent colours
  accents <-
    data.frame(
      colValue = c(same, edit, add, delete),
      accent = rep(labelling$labelsCol, 4),
      degree = rep(labelling$labelsColDif, 4)
    )
  accents$colValue2 <-
    ifelse(
      accents$accent == "darker",
      darken(accents$colValue, accents$degree),
      lighten(accents$colValue, accents$degree)
    )
  
  # Identify which colours are present in the timeline
  colsPresent <- c()
  for (u in 1:length(smallsetTables[[1]])) {
    uniqueCols <- smallsetTables[[1]][[u]]$body$styles$text$color$data
    uniqueCols <- as.vector(as.matrix(uniqueCols))
    uniqueCols <- unique(uniqueCols)
    colsPresent <- c(colsPresent, uniqueCols)
  }
  
  colsPresent <- unique(colsPresent)
  tileColours <-
    subset(tileColours, tileColours$colValue %in% colsPresent)
  
  # Prepare colour legend
  if (isTRUE(missingDataTints)) {
    descriptions <-
      c(
        "Data has not changed.\nTint is missing data.",
        "Data has been edited.\nTint is missing data.",
        "Data has been added.\nTint is missing data.",
        "Data will be deleted.\nTint is missing data."
      )
  } else {
    descriptions <-
      c(
        "Data has not changed.",
        "Data has been edited.",
        "Data has been added.",
        "Data will be deleted."
      )
  }
  
  legendDF <- data.frame(colValue = c(), description = c())
  colItems <- c(same, edit, add, delete)
  for (colItemNum in 1:length(colItems)) {
    if (colItems[colItemNum] %in% colsPresent) {
      legendAddition <-
        data.frame(colValue = c(colItems[colItemNum]),
                   description = descriptions[colItemNum])
      legendDF <- rbind(legendDF, legendAddition)
    }
  }
  
  # Insert ghost data rows/columns
  if (isTRUE(ghostData)) {
    ghostDF1 <-
      as.data.frame(smallsetTables[[1]][[1]]$body$styles$text$color$data)
    ghostDF1[] <- "#FFFFFF"
    
    ghostDF2 <- as.data.frame(smallsetTables[[1]][[1]]$body$dataset)
    ghostDF2[] <- ""
    
    row.names(ghostDF1) <- row.names(ghostDF2)
    
    extTables <-
      lapply(items, ghostDF1, ghostDF2, smallsetTables, FUN = add_ghost_data)
  } else {
    extTables <- lapply(items, smallsetTables, FUN = extract_tables)
  }
  
  # Find the Timeline's dimensions
  maxDims <- get_timeline_dimensions(extTables)
  
  # Make the graphic for each snapshot
  l <-
    lapply(
      items,
      extTables,
      smallsetTables,
      output,
      tileColours,
      printedData,
      ghostData,
      sizing,
      spacing,
      truncateData,
      maxDims,
      timelineFont,
      accents,
      legendDF,
      missingDataTints,
      FUN = plot_snapshots
    )
  
  # Set limits for the x-axis
  if (spacing$rows > 1) {
    m <- maxDims[[1]] + spacing$tablesR
    if (!is.null(output[[2]])) {
      m <- maxDims[[1]] + 2.51
    }
    for (p in 1:length(l)) {
      l[[p]] <- l[[p]] + xlim(c(0, m))
    }
  }
  
  if ((spacing$rows == 1) & (spacing$tablesR != .5)) {
    m <- maxDims[[1]] + spacing$tablesR
    for (p in 1:length(l)) {
      l[[p]] <- l[[p]] + xlim(c(0, m))
    }
  }
  
  # Assemble snapshots into a Smallset Timeline
  patchedPlots <- ""
  for (s in 1:length(l)) {
    addPlot <- paste0("l[[", as.character(s), "]] + ")
    patchedPlots <- paste0(patchedPlots, addPlot)
  }
  
  patchedPlots <- paste0(
    patchedPlots,
    "plot_layout(nrow = ",
    as.character(spacing$rows),
    ", guides = 'collect')"
    )
  
  # Set Timeline design choices
  fontChoice <-
    paste0(
      " & theme(text = element_text(family = '",
      timelineFont,
      "', colour = 'black'),",
      "legend.key.size = unit(",
      sizing[["legendIcons"]],
      ", 'line'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'))"
    )
  
  patchedPlots <-
    paste0(patchedPlots, fontChoice)
  
  # Generate alternative text for the Smallset Timeline
  generate_alt_text(
    smallsetTables = smallsetTables[[1]],
    tileColours = tileColours,
    legendDF = legendDF,
    altTextInfo = smallsetTables[[2]],
    l = l,
    printedData = printedData,
    ghostData = ghostData
  )
  
  o <- eval(parse(text = patchedPlots))
  oldClass(o) <- c("SmallsetTimeline", class(o))
  return(o)
  
}
