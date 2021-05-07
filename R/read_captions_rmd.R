#' Read the caption R markdown file
#' @description The function extracts information from the caption template.
#' @keywords internal

read_captions_rmd <- function(script, pathway) {
  
  # Import the caption template
  plotInfo <-
    as.data.frame(readLines(paste0(pathway, "/", script, ".Rmd")))
  colnames(plotInfo) <- c("lines")
  
  # Extract captions written for the timeline
  captions <- subset(plotInfo, grepl("Caption:", plotInfo$lines))
  captions$id <- seq(1, nrow(captions), 1)
  row.names(captions) <- seq(1, nrow(captions), 1)
  colnames(captions) <- c("caption", "id")
  captions$caption <- sub("Caption: ", "", captions$caption)
  
  return(captions)
  
}
