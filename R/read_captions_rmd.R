#' Read the caption R markdown file
#' @description A function to interpret the caption R markdown file
#' @keywords internal
#' @export

read_captions_rmd <- function(script, pathway) {

  plotInfo <- as.data.frame(readLines(paste0(pathway, "/", script, ".Rmd")))
  colnames(plotInfo) <- c("lines")
  
  captions <- subset(plotInfo, grepl("Caption:", plotInfo$lines))
  captions$id <- seq(1, nrow(captions), 1)
  row.names(captions) <- seq(1, nrow(captions), 1)
  colnames(captions) <- c("caption", "id")
  captions$caption <- sub("Caption: ", "", captions$caption)
  
  return(captions)
  
}
