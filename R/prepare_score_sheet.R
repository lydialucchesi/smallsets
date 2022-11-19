#' Prepare score sheet
#' @description Generates a score sheet for automatic Smallset selection
#'   between snapshots.
#' @keywords internal
#' @import "flextable"

prepare_score_sheet <-
  
  function(smallsetList = smallsetList, data = data) {
    scores <- data.frame("s1" = rep(0, nrow(smallsetList[[1]])))
    for (s in 1:length(smallsetList)) {
      scores[, paste0("s", s)] <- rep(0, nrow(smallsetList[[1]]))
    }
    
    tables <- list()
    tables <- find_data_changes(smallsetList = smallsetList,
                                tables = tables,
                                altText = FALSE)
    
    for (t in 1:length(tables)) {
      tab <- as.data.frame(tables[[t]]$body$styles$text$color$data)
      rownames(tab) <- rownames(tables[[t]]$body$dataset)
      tab[tab != "#808080"] <- 1
      tab[tab == "#808080"] <- 0
      tab[] <- lapply(tab, as.character)
      tab[] <- lapply(tab, as.numeric)
      tab$scoreSum <- rowSums(tab)
      score1 <- rownames(subset(tab, tab$scoreSum > 0))
      scores[ ,t] <- ifelse(rownames(scores) %in% score1, 1, 0)
    }
    
    return(scores)
    
  }
