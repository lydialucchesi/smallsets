#' Highlight changes
#' @description A function to identify changes, additions, and deletions between snapshots
#' @keywords internal
#' @import "flextable"
#' @importFrom gdata cbindX


highlight_changes <-
  function(smallsetList = smallsetList,
           tempName = captionTemplateName,
           tempDir = captionTemplateDir,
           tempAuthor = captionTemplateAuthor) {
    constant = "#808080"
    changed = "#008000"
    added = "#0000FF"
    deleted = "#FF0000"
    
    printMessage <- write_caption_template(
      authorName = tempAuthor,
      script = tempName,
      pathway = tempDir
    )
    
    tables <- list()
    for (p in 1:(length(smallsetList) - 1)) {
      c <- p + 1
      
      lprior <- smallsetList[[p]]
      lcurrent <- smallsetList[[c]]
      
      if (p > 1) {
        tprior <- tables[[p]]
      } else {
        tprior <- flextable::flextable(lprior)
        tprior <- flextable::color(tprior, color = constant)
      }
      
      tcurrent <- flextable::flextable(lcurrent)
      tcurrent <- flextable::color(tcurrent, color = constant)
      
      rowsDrop <- setdiff(rownames(lprior), rownames(lcurrent))
      if (length(rowsDrop) > 0) {
        tprior <- flextable::color(tprior, color = deleted, i = rowsDrop)
      }
      
      rowsAdd <- setdiff(rownames(lcurrent), rownames(lprior))
      if (length(rowsAdd) > 0) {
        tcurrent <-
          flextable::color(tcurrent, color = added, i = rowsAdd)
      }
      
      colsDrop <- setdiff(colnames(lprior), colnames(lcurrent))
      if (length(colsDrop) > 0) {
        tprior <-
          flextable::color(tprior,
                           color = deleted,
                           j = colsDrop,
                           part = "all")
      }
      
      colsAdd <- setdiff(colnames(lcurrent), colnames(lprior))
      if (length(colsAdd) > 0) {
        tcurrent <-
          flextable::color(tcurrent,
                           color = added,
                           j = colsAdd,
                           part = "all")
      }
      
      if (length(rowsDrop) > 0) {
        lpriorAdj <- subset(lprior, !(row.names(lprior) %in% rowsDrop))
      } else {
        lpriorAdj <- lprior
      }
      
      if (length(colsAdd) > 0) {
        lcurrentAdj <- subset(lcurrent, select = colnames(lprior))
      } else {
        lcurrentAdj <- lcurrent
      }
      
      row.names(lpriorAdj) <- as.numeric(row.names(lpriorAdj))
      row.names(lcurrentAdj) <- as.numeric(row.names(lcurrentAdj))
      
      original <-
        setdiff(subset(lpriorAdj, select = colnames(lcurrentAdj)), lcurrentAdj)
      update <-
        setdiff(lcurrentAdj, subset(lpriorAdj, select = colnames(lcurrentAdj)))
      
      adjData <- data.frame(r = numeric(), c = numeric())
      for (i in 1:nrow(original)) {
        for (j in 1:ncol(original)) {
          if (identical(original[i, j], update[i, j]) == FALSE) {
            adjDatum <- data.frame(r = c(row.names(update)[i]), c = c(j))
            adjData <- rbind(adjData, adjDatum)
          }
        }
      }
      
      adjData$r <- (as.character(adjData$r))
      
      # tcurrent <-
      #   flextable::color(
      #     tcurrent,
      #     color = changed,
      #     i = (adjData$r == row.names(tcurrent$body$dataset)),
      #     j = adjData$c
      #   )
      
      if (nrow(adjData) > 0) {
        tcurrent <-
          flextable::color(tcurrent,
                           color = changed,
                           i = adjData$r,
                           j = adjData$c)
      }
      
      tables[[p]] <- tprior
      tables[[c]] <- tcurrent
      
    }
    
    print(
      paste0(
        "Edits, additions, and deletions identified. ",
        printMessage,
        " at ",
        tempDir,
        "."
      )
    )
    
    return(tables)
    
  }
