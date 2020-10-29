addCaptionBlock <- function(i, col1, col2, col3, code) {
  snapshot <- c(
    "",
    paste0("### `", code$lines[i], "`"),
    "",
    "Symbols",
    paste0("\n1. Changed (", col1, "):"),
    paste0("\n2. Added (", col2, "):"),
    paste0("\n3. Deleted (", col3, "):"),
    "",
    "Caption: "
  )
}

write_caption_template <-
  function(authorName = author, col1, col2, col3, script) {
    if (is.null(authorName)) {
      authorName <- ""
    }
    
    heading <- c(
      "---",
      "title: \"Captions for the smallset timeline\"",
      paste0("author: ", authorName),
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
    
    
    captionBlocks <-
      lapply(refRows, col1, col2, col3, code, FUN = addCaptionBlock)
    captionBlocks <- unlist(captionBlocks)
    
    rmdTotal <- c(heading, firstPlot, captionBlocks)
    
    
    fileConn <- file(paste0(script, ".Rmd"))
    writeLines(rmdTotal, fileConn)
    close(fileConn)
    
    return(print(paste0(paste0(script, ".Rmd"), " file created")))
  }
