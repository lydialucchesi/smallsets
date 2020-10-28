

addCaptionBlock <- function(i) {
  snapshot <- c("",
                paste0("### `", code$lines[i], "`"),
                "",
                "Symbol: ",
                "",
                "Caption: ")
}

write_caption_template <- function(author = NULL) {
  
  if (is.null(author)) {
    author <- ""
  }
  
  heading <- c(
    "---",
    "title: \"Captions for the smallset timeline\"",
    paste0("author: ", author),
    paste0("date: ", Sys.Date()),
    "output: html_document",
    "---"
  )
  
  code <- as.data.frame(readLines("smallset_code.R"))
  colnames(code) <- c("lines")
  
  firstPlot <- c("",
                "### Starting smallset",
                "",
                "Caption: ")
  
  
  
  snaps <- subset(code, grepl("# snap ", code$lines))
  refRows <- as.integer(row.names(snaps)) + 1
  m <- max(refRows)
  refRows[refRows == m] <- refRows[refRows == m] - 2
  
  
  captionBlocks <- lapply(refRows, addCaptionBlock)
  captionBlocks <- unlist(captionBlocks)
  
  rmdTotal <- c(heading, firstPlot, captionBlocks)
  
  
  fileConn <- file("captions.Rmd")
  writeLines(rmdTotal, fileConn)
  close(fileConn)
  
  return(print("captions.Rmd file created"))
}
