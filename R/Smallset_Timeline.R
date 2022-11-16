#' Smallset Timeline
#'
#' @description The command for creating a Smallset Timeline.
#'
#' @param data Dataset.
#' @param code R or Python script with data preprocessing code for the dataset.
#'   Filename extension should be included (e.g., "my_code.R" or "my_code.py").
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
#' @param constant Hex colour code. Colour represents data that have not changed
#'   since previous snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param changed Hex colour code. Colour represents data that have changed
#'   since previous snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param added Hex colour code. Colour represents data that have been added
#'   since previous snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param deleted Hex colour code. Colour represents data that will be deleted
#'   prior to next snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param colScheme NULL, colour scheme name, or vector. If NULL, uses four
#'   colour arguments above. If colour scheme name, uses built-in scheme with
#'   colours pre-assigned to the four preprocessing states (constant, changed,
#'   added, deleted). If vector, it must be a vector of length five, with the
#'   first element being the colour scheme name followed by the four
#'   preprocessing states in the order that they should be assigned to scheme
#'   colours (e.g.,, c("colScheme1", "changed", "constant", "deleted",
#'   "added")).
#' @param printedData TRUE or FALSE. TRUE prints data values in tables.
#' @param ghostData TRUE or FALSE. TRUE includes blank spaces where data have
#'   been removed.
#' @param missingDataTints TRUE or FALSE. TRUE plots a lighter colour value to signal
#'   data value is missing.
#' @param sizing List of size specifications.
#' @param truncateData TRUE or FALSE. FALSE if data do not need to be truncated
#'   to fit within table tiles. Otherwise, an integer specifying width of data
#'   value (width includes "...").
#' @param rotateHeader TRUE or FALSE. If TRUE, column names are printed at 45
#'   degree angle. Use if column names overlap when set to FALSE.
#' @param headerSpace Vector. Default is c(1, .5). First value corresponds to
#'   room above the table and second to the right of the table.
#' @param accentCols Either "darker" or "lighter" for stamp colour. Can enter a
#'   list corresponding to specific actions.
#' @param accentColsDif Value between 0 and 1. Corresponds to how much lighter
#'   or darker accent colour will be. Can pass a list with different accent
#'   values for different colours.
#' @param otherTextCol Value between 0 and 1. Default is 1, which is when column
#'   names are black. 0 means columns will be the constant colour.
#' @param timelineRows Integer greater than or equal to one. Number of rows to
#'   divide the smallset timeline into.
#' @param timelineFont One of "sans", "serif", or "mono".
#' @param captionSpace Value greater than or equal to .5. Higher values create
#'   more caption space. Default is 1.
#' @export

Smallset_Timeline <- function(data,
                              code,
                              dir = getwd(),
                              rowCount = 6,
                              rowNums = NULL,
                              auto = NULL,
                              runBig = TRUE,
                              ignoreCols = NULL,
                              constant = NULL,
                              changed = NULL,
                              added = NULL,
                              deleted = NULL,
                              colScheme = "colScheme1",
                              printedData = FALSE,
                              ghostData = TRUE,
                              missingDataTints = FALSE,
                              sizing = list(
                                columns = 3,
                                captions = 3,
                                tiles = .1,
                                data = 2.5,
                                legendText = 10,
                                legendIcons = 1,
                                resume = .25
                              ),
                              truncateData = FALSE,
                              rotateHeader = FALSE,
                              headerSpace = c(1, .5),
                              accentCols = "darker",
                              accentColsDif = .8,
                              otherTextCol = 1,
                              timelineRows = 1,
                              timelineFont = "sans",
                              captionSpace = 3) {
  snapshots <- prepare_smallset(
    data = data,
    code = code,
    dir = dir,
    rowCount = rowCount,
    rowNums = rowNums,
    auto = auto,
    runBig = runBig,
    ignoreCols = ignoreCols
  )
  
  figure <- create_timeline(
    snapshotList = snapshots,
    constant = constant,
    changed = changed,
    added = added,
    deleted = deleted,
    colScheme = colScheme,
    printedData = printedData,
    ghostData = ghostData,
    missingDataTints = missingDataTints,
    sizing = sizing,
    truncateData = truncateData,
    rotateHeader = rotateHeader,
    headerSpace = headerSpace,
    accentCols = accentCols,
    accentColsDif = accentColsDif,
    otherTextCol = otherTextCol,
    timelineRows = timelineRows,
    timelineFont = timelineFont,
    captionSpace = captionSpace
    
  )
  return(figure)
}
