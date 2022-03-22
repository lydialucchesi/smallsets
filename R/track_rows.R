


for (i in 1:length(smallsetList)) {
  if (i == 1) {
    index <- data.frame("key1" = rownames(smallsetList[[1]]))
  } else {
    m1 <-
      match_df(smallsetList[[i - 1]], smallsetList[[i]][, colnames(smallsetList[[i -
                                                                                   1]])])
    m2 <-
      match_df(smallsetList[[i]][, colnames(smallsetList[[i - 1]])], smallsetList[[i -
                                                                                     1]])
    
    m1[, paste0("key", i - 1)] <- rownames(m1)
    m2$newRN <- rownames(m2)
    
    m <- merge(m1, m2)
    m <- m[, c(paste0("key", i - 1), "newRN")]
    index <- full_join(index, m, by = paste0("key", i - 1))
    
    match_df(df[, colnames(df1)], df1)
    
    newRows <-
      data.frame("newRN" = rownames(df[!rownames(df) %in% rownames(m2),]))
    index <- full_join(index, newRows)
    names(index)[names(index) == "newRN"] <- paste0("key", i)
  }
  
}