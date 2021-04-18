#' Select smallset
#' @description A function to prepare a smallset
#' @keywords internal
#' @export
#' @import "dplyr"

select_smallset <- function(data,
                            rowCount = 6,
                            rowNums = NULL) {
  if (is.null(rowNums)) {
    smallset <- dplyr::sample_n(data, size = rowCount)
    smallset$smallsetRowID <- row.names(smallset)
  }
  
  if (!is.null(rowNums)) {
    data$smallsetRowID <- row.names(data)
    smallset1 <- data[rowNums,]
    smallset2 <-
      dplyr::sample_n(data[-rowNums,], size = (rowCount - length(rowNums)))
    smallset <- rbind(smallset1, smallset2)
    
  }
  
  smallset <- smallset[order(as.numeric(smallset$smallsetRowID)),]
  smallset$smallsetRowID <- NULL
  rownames(smallset) <- seq(1, nrow(smallset), 1)
  return(smallset)
}
