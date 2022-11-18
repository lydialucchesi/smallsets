#' Prepare colour sheet
#' @description Generates a colour sheet for automatic Smallset selection.
#' @keywords internal
#' @import "flextable" "tibble"

prepare_colour_sheet <-
  
  function(smallsetList = smallsetList) {
    constant = "#808080"
    changed = "#008000"
    added = "#0000FF"
    deleted = "#FF0000"
    
    first <- smallsetList[[1]]
    first[] <- lapply(first, as.character)
    
    last <- smallsetList[[length(smallsetList)]]
    last[] <- lapply(last, as.character)
    
    colsDrop <- setdiff(colnames(first), colnames(last))
    rowsDrop <- setdiff(rownames(first), rownames(last))
    
    for (c in colsDrop) {
      if (!c %in% colnames(last)) {
        origCols <- colnames(first)
        place <- which(origCols == c) - 1
        last <-
          add_column(.data = last,
                     addingCol = NA,
                     .after = place)
        names(last)[names(last) == 'addingCol'] <- c
      }
    }
    
    for (r in rowsDrop) {
      if (!r %in% rownames(last)) {
        origRows <- rownames(first)
        place <- which(origRows == r) - 1
        rownameslist <- append(rownames(last), r, after = place)
        last <-
          add_row(.data = last,
                  .after = place)
        rownames(last) <- rownameslist
      }
    }
    
    colours <- last
    colours[,] <- constant
    
    tables <- list()
    for (p in 1:(length(smallsetList) - 1)) {
      c <- p + 1
      
      lprior <- smallsetList[[p]]
      lprior[] <- lapply(lprior, as.character)
      
      lcurrent <- smallsetList[[c]]
      lcurrent[] <- lapply(lcurrent, as.character)
      
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
              color = changed,
              i = adjData$nmbr[v],
              j = adjData$c[v]
            )
          }
        }
        
      }
      
      tables[[p]] <- tprior
      tables[[c]] <- tcurrent
    }
    
    for (t in 1:length(tables)) {
      t_colours <- as.data.frame(tables[[t]]$body$styles$text$color$data)
      rownames(t_colours) <- rownames(tables[[t]]$body$dataset)
      
      for (i in rownames(t_colours)) {
        for (j in colnames(t_colours)) {
          c1 <- t_colours[i, j]
          if (c1 != "#808080") {
            colours[i, j] <- c1
          }
        }
      }
    }
    
    return(colours)
    
  }
