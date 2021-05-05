#' Select smallset
#' @description A function to prepare a smallset
#' @keywords internal
#' @import "dplyr"

select_smallset <- function(data,
                            rowCount = 6,
                            rowNums = NULL,
                            runBig = FALSE,
                            ignoreCols = ignoreCols) {
  if (is.null(rowNums)) {
    smallset <- dplyr::sample_n(data, size = rowCount)
    smallset$smallsetRowID <- row.names(smallset)
    smallset <- smallset[,!(names(smallset) %in% ignoreCols)]
  }
  
  if (!is.null(rowNums)) {
    data$smallsetRowID <- row.names(data)
    smallset1 <- data[rowNums,]
    smallset2 <-
      dplyr::sample_n(data[-rowNums,], size = (rowCount - length(rowNums)))
    smallset <- rbind(smallset1, smallset2)
    smallset <- smallset[,!(names(smallset) %in% ignoreCols)]
  }
  
  if (isTRUE(runBig)) {
    smallsetRowIDs <- sort(as.numeric(smallset$smallsetRowID))
    return(smallsetRowIDs)
  } else {
    smallset <- smallset[order(as.numeric(smallset$smallsetRowID)),]
    rownames(smallset) <- smallset$smallsetRowID
    smallset$smallsetRowID <- NULL
    return(smallset)
  }
  
}
