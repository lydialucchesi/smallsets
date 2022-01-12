#' Prepare colour sheet
#' @description Generates a colour sheet for automatic Smallset selection
#'   between snapshots.
#' @keywords internal
#' @import "flextable" "dplyr" "tibble"

prepare_colour_sheet <-
  
  function(smallsetList = smallsetList) {
    constant = "#808080"
    changed = "#008000"
    added = "#0000FF"
    deleted = "#FF0000"
    
    tables <- list()
    
    lprior <- smallsetList[[1]] %>% mutate_all(as.character)
    lcurrent <-
      smallsetList[[length(smallsetList)]] %>% mutate_all(as.character)
    
    tprior <- flextable::flextable(lprior)
    tprior <- flextable::color(tprior, color = constant)
    
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
      adjData <- left_join(adjData, rr, by = "r")
      
      
      if (nrow(adjData) > 0) {
        for (v in 1:nrow(adjData)) {
          tcurrent <- flextable::color(tcurrent,
                                       color = changed,
                                       i = adjData$nmbr[v],
                                       j = adjData$c[v])
        }
      }
      
    }
    
    tables[[1]] <- tprior
    tables[[2]] <- tcurrent
    
    tab1 <- as.data.frame(tables[[1]]$body$styles$text$color$data)
    rownames(tab1) <- rownames(tables[[1]]$body$dataset)
    
    tab2 <- as.data.frame(tables[[2]]$body$styles$text$color$data)
    rownames(tab2) <- rownames(tables[[2]]$body$dataset)
    
    tab1rows <- subset(tab1, !rownames(tab1) %in% rownames(tab2))
    
    colours <- bind_rows(tab1rows, tab2)
    
    colours <- colours[match(rownames(tab1), rownames(colours)), ]
    colours[is.na(colours)] <- deleted
    
    for (c in colsDrop) {
      if (!c %in% colnames(colours)) {
        origCols <- colnames(tab1)
        place <- which(origCols == c) - 1
        colours <-
          add_column(.data = colours,
                     addingCol = deleted,
                     .after = place)
        names(colours)[names(colours) == 'addingCol'] <- c
      }
    }
    
    return(colours)
    
  }
