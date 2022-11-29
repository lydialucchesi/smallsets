#' Find data changes
#' @description Finds data edits, additions, and deletions between snapshots.
#' @keywords internal

find_data_changes <- function(smallsetList,
                              fourCols,
                              altText
                              ) {
  tables <- list()
  
  if (isTRUE(altText)) {
    altTextInfo <- list()
  }
  
  # Loop through snapshots comparing two at a time
  for (p in 1:(length(smallsetList) - 1)) {
    c <- p + 1
    
    # Get two snapshots
    lprior <- smallsetList[[p]]
    lprior[] <- lapply(lprior, as.character)
    
    lcurrent <- smallsetList[[c]]
    lcurrent[] <- lapply(lcurrent, as.character)
    
    # Set starting colours
    if (p > 1) {
      tprior <- tables[[p]]
    } else {
      tprior <- flextable::flextable(lprior)
      tprior <- flextable::color(tprior, color = fourCols[1])
    }
    
    tcurrent <- flextable::flextable(lcurrent)
    tcurrent <- flextable::color(tcurrent, color = fourCols[1])
    
    # Compare rows
    rowsDrop <- setdiff(rownames(lprior), rownames(lcurrent))
    if (length(rowsDrop) > 0) {
      tprior <- flextable::color(tprior, color = fourCols[4], i = rowsDrop)
    }
    
    rowsAdd <- setdiff(rownames(lcurrent), rownames(lprior))
    if (length(rowsAdd) > 0) {
      tcurrent <-
        flextable::color(tcurrent, color = fourCols[3], i = rowsAdd)
    }
    
    # Compare columns
    colsDrop <- setdiff(colnames(lprior), colnames(lcurrent))
    if (length(colsDrop) > 0) {
      tprior <-
        flextable::color(tprior,
                         color = fourCols[4],
                         j = colsDrop,
                         part = "all")
    }
    
    colsAdd <- setdiff(colnames(lcurrent), colnames(lprior))
    if (length(colsAdd) > 0) {
      tcurrent <-
        flextable::color(tcurrent,
                         color = fourCols[3],
                         j = colsAdd,
                         part = "all")
    }
    
    # Compare data values
    if (length(rowsDrop) > 0) {
      lpriorAdj <- subset(lprior, !(row.names(lprior) %in% rowsDrop))
    } else {
      lpriorAdj <- lprior
    }
    
    if (length(colsAdd) > 0) {
      lcurrentAdj <-
        subset(lcurrent, select = colnames(lprior)[!(colnames(lprior) %in% colsDrop)])
    } else {
      lcurrentAdj <- lcurrent
    }
    
    row.names(lpriorAdj) <- as.numeric(row.names(lpriorAdj))
    row.names(lcurrentAdj) <- as.numeric(row.names(lcurrentAdj))
    
    original <- subset(lpriorAdj, select = colnames(lcurrentAdj))
    update <- lcurrentAdj
    
    if ((nrow(original) != 0) & (nrow(update) != 0)) {
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
      
      rr <-
        data.frame(nmbr = seq(1, length(row.names(
          tcurrent$body$dataset
        )), 1),
        r = row.names(tcurrent$body$dataset))
      adjData <- merge(adjData, rr, by = "r")
      
      
      if (nrow(adjData) > 0) {
        for (v in 1:nrow(adjData)) {
          tcurrent <- flextable::color(
            tcurrent,
            color = fourCols[2],
            i = adjData$nmbr[v],
            j = adjData$c[v]
          )
        }
      }
      
    }
    
    # Save latest snapshot tables
    tables[[p]] <- tprior
    tables[[c]] <- tcurrent
    
    # Update alt text info
    if (isTRUE(altText)) {
      newAltTextInfo <-
        list(
          rowsDrop = rowsDrop,
          rowsAdd = rowsAdd,
          colsAdd = colsAdd,
          colsDrop = colsDrop,
          adjData = adjData
        )
      altTextInfo <- append(altTextInfo, newAltTextInfo)
    }
  }
  
  if (isTRUE(altText)) {
    return(list(tables, altTextInfo))
  } else {
    return(list(tables))
  }
}
