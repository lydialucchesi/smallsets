#' Write caption template
#' @description A function to prepare the caption template
#' @keywords internal
#' @export

write_caption_template <-
  function(authorName = author, script, pathway) {
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
    
    titleBlock <- c("",
                    "Timeline title: ",
                    "",
                    "Timeline subtitle: ",
                    "",
                    "Timeline footnote: ")
    
    firstPlot <- c("",
                   "### Starting smallset",
                   "",
                   "Caption: ")
    
    
    
    snaps <- subset(code, grepl("# snap ", code$lines))
    refRows <- as.integer(row.names(snaps)) + 1
    m <- max(refRows)
    refRows[refRows == m] <- refRows[refRows == m] - 2
    
    
    captionBlocks <-
      lapply(refRows, code, FUN = add_caption_block)
    captionBlocks <- unlist(captionBlocks)
    
    rmdTotal <- c(heading, titleBlock, firstPlot, captionBlocks)
    
    
    fileConn <- file(paste0(script, ".Rmd"))
    writeLines(rmdTotal, fileConn)
    close(fileConn)
    
    return((paste0(paste0(script, ".Rmd"), " file created")))
  }
