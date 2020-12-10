#' Select smallset
#' @description A function to prepare a smallset
#' @keywords internal
#' @export
#' @import "dplyr"

select_smallset <- function(dataNum,
                            data,
                            rowCount = 6,
                            rowNums = NULL) {
  
  data <- data[[dataNum]]
  rowCount <- rowCount[[dataNum]]
  rowNums <- rowNums[[dataNum]]
  
  if (is.null(rowNums)) {
    smallset <- dplyr::sample_n(data, size = rowCount)
  }
  
  if (!is.null(rowNums)) {
    smallset <- rbind(data[rowNums,],
                      dplyr::sample_n(data[-rowNums,], size = (rowCount - length(rowNums))))
  }
  
  rownames(smallset) <- seq(1, nrow(smallset), 1)
  return(smallset)
}
