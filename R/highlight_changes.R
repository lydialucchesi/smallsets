#' Highlight changes
#'
#' @param smallsetList A list from \code{prep_smallset}
#' @param constant A colour to represent data cells that have not changed since previous smallset
#' @param changed A colour to represent data cells that have changed since previous smallset
#' @param added A colour to represent data cells that have been added since previous smallset
#' @param deleted A colour to represent cells that will be deleted from next smallset
#' @param author The author's name for the caption .Rmd file
#' @param captionScript A file name for the caption .Rmd template
#' @param captionDir A file path for the caption .Rmd template
#' @export
#' @import "flextable"
#' @importFrom gdata cbindX


highlight_changes <-
  function(smallsetList,
           constant = "#927C5C",
           changed = "cornflowerblue",
           added = "#689F38",
           deleted = "#b2a38c",
           author = NULL,
           captionScript = "captions",
           captionDir = getwd()) {

    resumeLocs <- smallsetList[[2]]
    smallsetList <- smallsetList[[1]]
    
    if (!is.list(constant)) {
      constantAlpha = .4
    } else {
      constantAlpha = constant[[2]]
      constant = constant[[1]]
    }
    
    if (!is.list(changed)) {
      changedAlpha = .4
    } else {
      changedAlpha = changed[[2]]
      changed = changed[[1]]
    }
    
    if (!is.list(added)) {
      addedAlpha = .4
    } else {
      addedAlpha = added[[2]]
      added = added[[1]]
    }
    
    if (!is.list(deleted)) {
      deletedAlpha = .4
    } else {
      deletedAlpha = deleted[[2]]
      deleted = deleted[[1]]
    }
    
    tileAlphas <-
      data.frame(
        colValue = c(constant, changed, added, deleted),
        alpha = c(constantAlpha, changedAlpha, addedAlpha, deletedAlpha)
      )
    
    write_caption_template(
      authorName = author,
      col1 = changed,
      col2 = added,
      col3 = deleted,
      script = captionScript,
      pathway = captionDir
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
      
      adjData$r <- as.integer(as.character(adjData$r))
      # tcurrent <-
      #   flextable::color(tcurrent,
      #         color = changed,
      #         i = adjData$r,
      #         j = adjData$c)
      
      if (nrow(adjData) > 0) {
        tcurrent <-
          flextable::color(
            tcurrent,
            color = changed,
            i = (adjData$r == row.names(tcurrent$body$dataset)),
            j = adjData$c
          )
      }
      
      
      tables[[p]] <- tprior
      tables[[c]] <- tcurrent
      
    }
    
    return(list(
      tables,
      constant,
      changed,
      added,
      deleted,
      captionScript,
      captionDir,
      tileAlphas,
      resumeLocs
    ))
  }
