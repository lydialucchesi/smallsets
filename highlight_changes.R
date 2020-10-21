library(flextable)

# highlight what you will lose (rows and columns) (prior to it happening because once it is gone you can't see it)
# default colour is tan
# highlight what is now changed (cells in data frame)
# default is cornflower blue
# highlight what has been added (rows or columns)
# default colour is olive green

highlight_changes <- function(list) {
  tables <- list()
  for (p in 1:(length(list) - 1)) {
    c <- p + 1
    
    lprior <- list[[p]]
    lcurrent <- list[[c]]
    
    if (p > 1) {
      tprior <- tables[[p]]
    } else {
      tprior <- flextable(lprior)
    }
    
    tcurrent <- flextable(lcurrent)
    
    rowsDrop <- setdiff(rownames(lprior), rownames(lcurrent))
    if (length(rowsDrop) > 0) {
      tprior <- color(tprior, color = "seashell4", i = rowsDrop)
    }
    
    rowsAdd <- setdiff(rownames(lcurrent), rownames(lprior))
    if (length(rowsAdd) > 0) {
      tcurrent <-
        color(tcurrent, color = "olivedrab4", i = rowsAdd)
    }
    
    colsDrop <- setdiff(colnames(lprior), colnames(lcurrent))
    if (length(colsDrop) > 0) {
      tprior <-
        color(tprior,
              color = "seashell4",
              j = colsDrop,
              part = "all")
    }
    
    colsAdd <- setdiff(colnames(lcurrent), colnames(lprior))
    if (length(colsAdd) > 0) {
      tcurrent <-
        color(tcurrent,
              color = "olivedrab4",
              j = colsAdd,
              part = "all")
    }
    
    if (length(rowsDrop) > 0) {
      lpriorAdj <- subset(lprior,!(row.names(lprior) %in% rowsDrop))
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
    tcurrent <-
      color(tcurrent,
            color = "cornflowerblue",
            i = adjData$r,
            j = adjData$c)
    
    tables[[p]] <- tprior
    tables[[c]] <- tcurrent
    
  }
  
  
  return(tables)
}

source("gen_data.R")
mylist <-
  prep_smallset(
    data = df,
    prepCode = "prep_data.R",
    size = 6,
    rowNums = c(1, 2, 5)
  )
fts <- highlight_changes(list = mylist)
