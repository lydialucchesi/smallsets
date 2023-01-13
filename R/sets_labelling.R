#' Sets labelling
#' @description Sets labelling parameters for the Smallset Timeline.
#' @param labelCol "lighter" or "darker" for column names 
#' and printed data, in comparison to tile colours. 
#' @param labelColDif Value between 0-1 determining how much lighter or 
#' darker.
#' @examples
#' # labels are black
#' sets_labelling(labelCol = "darker", labelColDif = 1)
#' 
#' # labels are white 
#' sets_labelling(labelCol = "lighter", labelColDif = 1)
#' 
#' # midpoint between tile colours and black
#' sets_labelling(labelCol = "darker", labelColDif = .5)
#' 
#' @export

sets_labelling <- function(labelCol = NULL,
                           labelColDif = NULL) {
  
  labelling <- list()
  
  if (is.null(labelCol)) {
    labelling$labelCol <- "darker"
  } else {
    labelling$labelCol <- labelCol
  }
  
  if (is.null(labelColDif)) {
    labelling$labelColDif <- .5
  } else {
    labelling$labelColDif <- labelColDif
  }
  
  return(labelling)
}
