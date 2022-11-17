#' Prepare score sheet
#' @description Generates a score sheet for automatic Smallset selection
#'   between snapshots.
#' @keywords internal
#' @import "flextable" "dplyr"

prepare_score_sheet <-
  
  function(smallsetList = smallsetList, data = data) {
    scores <- data.frame("s1" = rep(0, nrow(smallsetList[[1]])))
    for (s in 1:length(smallsetList)) {
      scores[, paste0("s", s)] <- rep(0, nrow(smallsetList[[1]]))
    }
    
    constant = "#808080"
    changed = "#008000"
    added = "#0000FF"
    deleted = "#FF0000"
    
    tables <- list()
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
        tprior <- flextable::color(tprior, color = constant)
      }
      
      tcurrent <- flextable::flextable(lcurrent)
      tcurrent <- flextable::color(tcurrent, color = constant)
      
      # Compare rows
      rowsDrop <- setdiff(rownames(lprior), rownames(lcurrent))
      if (length(rowsDrop) > 0) {
        tprior <- flextable::color(tprior, color = deleted, i = rowsDrop)
      }
      
      rowsAdd <- setdiff(rownames(lcurrent), rownames(lprior))
      if (length(rowsAdd) > 0) {
        tcurrent <-
          flextable::color(tcurrent, color = added, i = rowsAdd)
      }
      
      # Compare columns
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
      
      # Compare data values
      if (length(rowsDrop) > 0) {
        lpriorAdj <- subset(lprior, !(row.names(lprior) %in% rowsDrop))
      } else {
        lpriorAdj <- lprior
      }
      
      if (length(colsAdd) > 0) {
        lcurrentAdj <- subset(lcurrent, select = colnames(lprior)[!(colnames(lprior) %in% colsDrop)])
      } else {
        lcurrentAdj <- lcurrent
      }
      
      row.names(lpriorAdj) <- as.numeric(row.names(lpriorAdj))
      row.names(lcurrentAdj) <- as.numeric(row.names(lcurrentAdj))
      
      # original <-
      #   setdiff(subset(lpriorAdj, select = colnames(lcurrentAdj)), lcurrentAdj)
      # update <-
      #   setdiff(lcurrentAdj, subset(lpriorAdj, select = colnames(lcurrentAdj)))
      
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
        
        # tcurrent <-
        #   flextable::color(
        #     tcurrent,
        #     color = changed,
        #     i = (adjData$r == row.names(tcurrent$body$dataset)),
        #     j = adjData$c
        #   )
        
        # if (nrow(adjData) > 0) {
        #   tcurrent <-
        #     flextable::color(tcurrent,
        #                      color = changed,
        #                      i = adjData$r,
        #                      j = adjData$c)
        # }
        
        rr <-
          data.frame(nmbr = seq(1, length(row.names(
            tcurrent$body$dataset
          )), 1),
          r = row.names(tcurrent$body$dataset))
        adjData <- left_join(adjData, rr, by = "r")
        
        
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
      
      # Save latest snapshot tables
      tables[[p]] <- tprior
      tables[[c]] <- tcurrent
      
    }
    
    for (t in 1:length(tables)) {
      tab <- as.data.frame(tables[[t]]$body$styles$text$color$data)
      rownames(tab) <- rownames(tables[[t]]$body$dataset)
      tab[tab != "#808080"] <- 1
      tab[tab == "#808080"] <- 0
      tab <- mutate_all(tab, function(x) as.numeric(as.character(x)))
      tab$scoreSum <- rowSums(tab)
      score1 <- rownames(subset(tab, tab$scoreSum > 0))
      scores[ ,t] <- ifelse(rownames(scores) %in% score1, 1, 0)
    }
    
    return(scores)
    
  }
