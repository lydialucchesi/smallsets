#' Prepare score sheet
#' @description Generates a coverage indicator matrix for automated Smallset selection (autoSelect = 1 or 2).
#' @keywords internal
#' @import "flextable"

prepare_score_sheet <-
  function(smallsetList,
           fourCols) {
    scores <- data.frame("s1" = rep(0, nrow(smallsetList[[1]])))
    for (s in 1:length(smallsetList)) {
      scores[, paste0("s", s)] <- rep(0, nrow(smallsetList[[1]]))
    }
    
    tables <- find_data_changes(smallsetList, fourCols, FALSE)
    
    for (t in 1:length(tables[[1]])) {
      tab <- as.data.frame(tables[[1]][[t]]$body$styles$text$color$data)
      rownames(tab) <- rownames(tables[[1]][[t]]$body$dataset)
      tab[tab != fourCols[1]] <- 1
      tab[tab == fourCols[1]] <- 0
      tab[] <- lapply(tab, as.character)
      tab[] <- lapply(tab, as.numeric)
      tab$scoreSum <- rowSums(tab)
      score1 <- rownames(subset(tab, tab$scoreSum > 0))
      scores[, t] <- ifelse(rownames(scores) %in% score1, 1, 0)
    }
    
    return(scores)
    
  }
