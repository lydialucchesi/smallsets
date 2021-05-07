#' Write caption template
#' @description This function creates a caption template (Rmd file).
#' @keywords internal

write_caption_template <-
  function(authorName, script, pathway) {
    if (is.null(authorName)) {
      authorName <- ""
    }
    
    # Prepare some Rmd template sections
    heading <- c(
      "---",
      "title: \"Captions for the smallset timeline\"",
      paste0("author: ", authorName),
      paste0("date: ", Sys.Date()),
      "output: html_document",
      "---"
    )
    
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
    
    # Import the preprocessing function as text
    code <- as.data.frame(readLines("smallset_code.R"))
    colnames(code) <- c("lines")
    
    # Find snap points and subsequent line of code
    snaps <- subset(code, grepl("# snap ", code$lines))
    refRows <- as.integer(row.names(snaps)) + 1
    m <- max(refRows)
    refRows[refRows == m] <- refRows[refRows == m] - 2
    
    # Create a template section for each snap point
    captionBlocks <-
      lapply(refRows, code, FUN = add_caption_block)
    captionBlocks <- unlist(captionBlocks)
    
    # Compile template parts
    rmdTotal <- c(heading, titleBlock, firstPlot, captionBlocks)
    
    # Save the template to directory
    fileConn <- file(paste0(script, ".Rmd"))
    writeLines(rmdTotal, fileConn)
    close(fileConn)
    
    return((paste0(paste0(script, ".Rmd"), " file created")))
  }
