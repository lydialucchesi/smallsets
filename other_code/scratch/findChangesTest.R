
testingList <- smallsetList

list = testingList
captionScript = "mycaptions"
constant = "cornsilk4"
changed = "cornflowerblue"
added = "blueviolet"
deleted = "darkgoldenrod1"
author = "Lydia"

p = 1
p = 2
p = 3

write_caption_template(
  authorName = author,
  col1 = changed,
  col2 = added,
  col3 = deleted,
  script = captionScript,
  pathway = captionDir
)

tables <- list()

c <- p + 1

lprior <- list[[p]]
lcurrent <- list[[c]]

if (p > 1) {
  tprior <- tables[[p]]
} else {
  tprior <- flextable::flextable(lprior)
  tprior <- flextable::color(tprior, color = constant)
}

tcurrent <- flextable::flextable(lcurrent)
tcurrent <- flextable::color(tcurrent, color = constant)

adjData <- data.frame(r = numeric(), c = numeric())
for (i in 1:nrow(lcurrent)) {
  for (j in 1:ncol(lcurrent)) {
    if (identical(lcurrent[i, j], lprior[i, j]) == FALSE) {
      adjDatum <- data.frame(r = c(i), c = c(j))
      adjData <- rbind(adjData, adjDatum)
    }
  }
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
# check if colsAdd is greater than zero
# check if column appears in dataAdj 
if (length(colsAdd) > 0) {
  tcurrent <-
    flextable::color(tcurrent,
                     color = added,
                     j = colsAdd,
                     part = "all")
}

  
  
  
  
  
  # if (rownames(tcurrent) == as.character(seq(1, nrow(tcurrent), 1)))
  
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
  # tcurrent <-
  #   flextable::color(tcurrent,
  #         color = changed,
  #         i = adjData$r,
  #         j = adjData$c)
  
  if (nrow(adjData) > 0) {
    tcurrent <-
      flextable::color(tcurrent,
                       color = changed,
                       i = (adjData$r == row.names(tcurrent$body$dataset)),
                       j = adjData$c)
  }
  
  
  tables[[p]] <- tprior
  tables[[c]] <- tcurrent
  
