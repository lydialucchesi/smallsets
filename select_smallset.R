library(dplyr)

# should there be a minimum size of a smallset?
# add error: size cannot be less than rowNums
# add error: size cannot be greater than size of data
# add error: rowNums has to be a numeric vector
# should there be a maximum size of a smallset?

select_smallset <- function(data,
                            rowCount = 6,
                            rowNums = NULL) {
  if (is.null(rowNums)) {
    smallset <- sample_n(data, size = rowCount)
  }
  
  if (!is.null(rowNums)) {
    smallset <- rbind(data[rowNums, ],
                      sample_n(data[-rowNums, ], size = (rowCount - length(rowNums))))
  }
  
  rownames(smallset) <- seq(1, nrow(smallset), 1)
  return(smallset)
}

