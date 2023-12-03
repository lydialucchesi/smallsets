#' Prepare score sheet
#' @description Prepares the coverage indicator matrix for automated Smallset
#'   selection.
#' @noRd

prepare_score_sheet <-
  function(smallsetList,
           fourCols) {
    # Build structure for coverage indicator matrix
    scores <- data.frame("s1" = rep(0, nrow(smallsetList[[1]])))
    for (s in 1:length(smallsetList)) {
      scores[, paste0("s", s)] <- rep(0, nrow(smallsetList[[1]]))
    }
    
    # Find data changes between snapshots
    tables <- find_data_changes(smallsetList, fourCols, FALSE)
    
    # For each step (snapshot), update matrix cell to 1 if row changes
    for (t in 1:length(tables[[1]])) {
      tab <- as.data.frame(tables[[1]][[t]]$body$styles$text$color$data)
      rownames(tab) <- rownames(tables[[1]][[t]]$body$dataset)
      tab[tab != fourCols[4]] <- 1
      tab[tab == fourCols[4]] <- 0
      tab[] <- lapply(tab, as.character)
      tab[] <- lapply(tab, as.numeric)
      tab$scoreSum <- rowSums(tab)
      score1 <- rownames(subset(tab, tab$scoreSum > 0))
      scores[, t] <- ifelse(rownames(scores) %in% score1, 1, 0)
    }
    
    return(scores)
    
  }
