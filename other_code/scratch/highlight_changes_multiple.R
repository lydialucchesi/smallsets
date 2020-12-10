list <- smallsetList

listInfo <-
  data.frame(n = seq(1, length(list)), index = as.character(names(list)))
listInfo$index <- as.numeric(as.character(listInfo$index))
listInfo$snap <-
  ifelse(floor(listInfo$index) == listInfo$index,
         listInfo$index,
         ceiling(listInfo$index))
listInfo$first <-
  ifelse(listInfo$n %in% as.numeric(rownames(unique(
    data.frame(listInfo)["snap"]
  ))), TRUE, FALSE)
listInfo$ext <- ifelse(grepl("\\.", listInfo$index), FALSE, TRUE)


tables <- list()
for (p in 1:(length(list) - 1)) {
  c <- p + 1
  
  lprior <- list[[p]]
  lcurrent <- list[[c]]
  
  # ROWNAME ADJUSTMENTS
  if (isTRUE(flag) & (nrow(lprior) == nrow(lcurrent))) {
    rownames(lprior) <- rownames(lcurrent)
  }
  
  if ((nrow(lprior) == nrow(lcurrent)) & (FALSE %in% (rownames(lprior) == rownames(lcurrent)))) {
    rownames(lprior) <- rownames(lcurrent)
  }
  
  # SET TABLE COLOURS
  iprior <- listInfo[listInfo$n == p, "first"]
  icurrent <- listInfo[listInfo$n == c, "first"]
  
  if (p > 1) {
    tprior <- tables[[p]]
  } else {
    tprior <- flextable::flextable(lprior)
    tprior <- flextable::color(tprior, color = constant)
  }
  
  tcurrent <- flextable::flextable(lcurrent)
  tcurrent <- flextable::color(tcurrent, color = constant)
  
  if (isFALSE(icurrent)) {
    
    ivals <- row.names(tprior$body$dataset)[row.names(tprior$body$dataset) %in% row.names(lcurrent)]
    jvals <- colnames(tprior$body$dataset)[colnames(tprior$body$dataset) %in% colnames(lcurrent)]
    priorCols <- tprior$body$styles$text$color$data[as.numeric(ivals), jvals]
      
    tcurrent <- flextable::color(
      tcurrent,
      color = priorCols,
      i = ivals,
      j = jvals
    )
  }
  
  ## ROW DROPS
  rowsDrop <- setdiff(rownames(lprior), rownames(lcurrent))
  if (length(rowsDrop) > 0) {
    tprior <- flextable::color(tprior, color = deleted, i = rowsDrop)
  }
  
  flag <- FALSE
  if (length(rowsDrop) > 0) {
    flag <- TRUE
  }
  
  if (isTRUE(listInfo$first[c])) {
    oldDropped <- dropped
    dropped <- data.frame()
  } else {
    dropped <- rbind(dropped, lprior[as.numeric(rowsDrop),])
    print(dropped)
  }
  
  ## ROW ADDITIONS
  rowsAdd <- setdiff(rownames(lcurrent), rownames(lprior))
  if (length(rowsAdd) > 0) {
    tcurrent <-
      flextable::color(tcurrent, color = added, i = rowsAdd)
  }
  
  ## COLUMN DROPS
  colsDrop <- setdiff(colnames(lprior), colnames(lcurrent))
  if (length(colsDrop) > 0) {
    tprior <-
      flextable::color(tprior,
                       color = deleted,
                       j = colsDrop,
                       part = "all")
  }
  
  if (isTRUE(listInfo$first[c])) {
    oldColDropped <- colDropped
    colDropped <- data.frame()
  } else {
    colDropped <- rbind(colDropped, lprior[as.numeric(colsDrop), ])
    print(dropped)
  }
  
  ## COLUMN ADDITIONS
  colsAdd <- setdiff(colnames(lcurrent), colnames(lprior))
  if (length(colsAdd) > 0) {
    tcurrent <-
      flextable::color(tcurrent,
                       color = added,
                       j = colsAdd,
                       part = "body")
  }
  
  ## DATA EDITS
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
  
  if (nrow(adjData) > 0) {
    tcurrent <-
      flextable::color(
        tcurrent,
        color = changed,
        i = adjData$r,
        j = adjData$c
      )
  }
  
  if (isTRUE(listInfo$ext[p]) & nrow(oldDropped) != 0) {
    savedNames <- row.names(lcurrent)
    
    lcurrentD <- lcurrent
    for (d in 1:nrow(oldDropped)) {
      lcurrentD <- rbind.fill(lcurrentD[1:as.numeric(row.names(oldDropped))[d] - 1,], oldDropped[d,], lcurrentD[as.numeric(row.names(oldDropped))[d]:nrow(lcurrentD),])
      print(rownames(lcurrentD))
    }
    
    rows <- c()
    for (r in 1:nrow(lcurrentD)) {
      if (row.names(lcurrentD)[r] %in% row.names(oldDropped)) {
        rows <- c(rows, paste0("INTPKGRNAME", r))
      } else {
        rows <- c(rows, savedNames[1])
        savedNames <- savedNames[-1]
      }
    }
    row.names(lcurrentD) <- rows
    
    tcurrentD <- flextable::flextable(lcurrentD)
    
    tcurrentD <- flextable::color(tcurrentD,
                                  color = tcurrent$body$styles$text$color$data,
                                  i = row.names(tcurrent$body$dataset),
                                  j = colnames(tcurrent$body$dataset))
    
    tcurrentD <- flextable::color(tcurrentD,
                                  color = deleted,
                                  i = grep("INTPKGRNAME", row.names(lcurrentD), value=TRUE),
                                  j = colnames(oldDropped))
    
    tcurrentD <- flextable::color(tcurrentD,
                                  color = deleted,
                                  i = grep("INTPKGRNAME", row.names(lcurrentD), value=TRUE),
                                  j = colnames(oldDropped))
    
    tcurrentD <- flextable::color(tcurrentD,
                                  color = "white",
                                  i = grep("INTPKGRNAME", row.names(lcurrentD), value=TRUE),
                                  j = setdiff(colnames(lcurrent), colnames(oldDropped)))
  }
  
  ## SAVING
  if (isTRUE(listInfo$ext[p]) & nrow(oldDropped) != 0) {
    tables[[p]] <- tcurrentD
  } else {
    tables[[p]] <- tprior
  }
  tables[[c]] <- tcurrent
  
}

tables <- tables[listInfo[listInfo$ext == TRUE, "n"]]












# figure out how to get deleted rows added back in
# figure out what is wrong with time column 

dropped <- data.frame(year = c(-1999, 2000), 
                      count = c(5, 5),
                      time = c(11, 11),
                      defect = c(0, 0),
                      id = c(2, 2),
                      latitude = c(35.2809, 25))

rownames(dropped) <- c("2", "4")

savedNames <- row.names(lcurrent)

lcurrentD <- lcurrent
for (d in 1:nrow(dropped)) {
  lcurrentD <- rbind.fill(lcurrentD[1:as.numeric(row.names(dropped))[d] - 1,], dropped[d,], lcurrentD[as.numeric(row.names(dropped))[d]:nrow(lcurrentD),])
  print(rownames(lcurrentD))
}

rows <- c()
for (r in 1:nrow(lcurrentD)) {
  if (row.names(lcurrentD)[r] %in% row.names(dropped)) {
    rows <- c(rows, paste0("INTPKGRNAME", r))
  } else {
    rows <- c(rows, savedNames[1])
    savedNames <- savedNames[-1]
  }
}
row.names(lcurrentD) <- rows

tcurrentD <- flextable::flextable(lcurrentD)

tcurrentD <- flextable::color(tcurrentD,
                              color = tcurrent$body$styles$text$color$data,
                              i = row.names(tcurrent$body$dataset),
                              j = colnames(tcurrent$body$dataset))

tcurrentD <- flextable::color(tcurrentD,
                              color = deleted,
                              i = grep("INTPKGRNAME", row.names(lcurrentD), value=TRUE),
                              j = colnames(dropped))

tcurrentD <- flextable::color(tcurrentD,
                              color = deleted,
                              i = grep("INTPKGRNAME", row.names(lcurrentD), value=TRUE),
                              j = colnames(dropped))

tcurrentD <- flextable::color(tcurrentD,
                              color = "white",
                              i = grep("INTPKGRNAME", row.names(lcurrentD), value=TRUE),
                              j = setdiff(colnames(lcurrent), colnames(dropped)))





fts <- highlight_changes(
  list = smallsetList,
  captionScript = "mycaptions",
  constant = "cornsilk4",
  changed = "cornflowerblue",
  added = "blueviolet",
  deleted = "darkgoldenrod1",
  author = "Lydia"
)








list <- smallsetList

listInfo <-
  data.frame(n = seq(1, length(list)), index = as.character(names(list)))
listInfo$index <- as.numeric(as.character(listInfo$index))
listInfo$snap <-
  ifelse(floor(listInfo$index) == listInfo$index,
         listInfo$index,
         ceiling(listInfo$index))
listInfo$first <-
  ifelse(listInfo$n %in% as.numeric(rownames(unique(
    data.frame(listInfo)["snap"]
  ))), TRUE, FALSE)
listInfo$ext <- ifelse(grepl("\\.", listInfo$index), FALSE, TRUE)

tables <- list()
rowDropped <- data.frame()
for (p in 1:(length(list) - 1)) {
  c <- p + 1
  
  lprior <- list[[p]]
  lcurrent <- list[[c]]
  
  # ROWNAME ADJUSTMENTS
  if (isTRUE(flag) & (nrow(lprior) == nrow(lcurrent))) {
    rownames(lprior) <- rownames(lcurrent)
  }
  
  if ((nrow(lprior) == nrow(lcurrent)) &
      (FALSE %in% (rownames(lprior) == rownames(lcurrent)))) {
    rownames(lprior) <- rownames(lcurrent)
  }
  
  # SET TABLE COLOURS
  iprior <- listInfo[listInfo$n == p, "first"]
  icurrent <- listInfo[listInfo$n == c, "first"]
  
  if (p > 1) {
    tprior <- tables[[p]]
  } else {
    tprior <- flextable::flextable(lprior)
    tprior <- flextable::color(tprior, color = constant)
  }
  
  tcurrent <- flextable::flextable(lcurrent)
  tcurrent <- flextable::color(tcurrent, color = constant)
  
  if (isFALSE(icurrent)) {
    ivals <-
      row.names(tprior$body$dataset)[row.names(tprior$body$dataset) %in% row.names(lcurrent)]
    jvals <-
      colnames(tprior$body$dataset)[colnames(tprior$body$dataset) %in% colnames(lcurrent)]
    priorCols <-
      tprior$body$styles$text$color$data[as.numeric(ivals), jvals]
    
    tcurrent <- flextable::color(tcurrent,
                                 color = priorCols,
                                 i = ivals,
                                 j = jvals)
  }
  
  ## ROW DROPS
  rowsDrop <- setdiff(rownames(lprior), rownames(lcurrent))
  if (length(rowsDrop) > 0) {
    tprior <- flextable::color(tprior, color = deleted, i = rowsDrop)
  }
  
  flag <- FALSE
  if (length(rowsDrop) > 0) {
    flag <- TRUE
  }
  
  if (isTRUE(listInfo$first[c])) {
    oldRowDropped <- rowDropped
    rowDropped <- data.frame()
  } else {
    rowDropped <- rbind(rowDropped, lprior[as.numeric(rowsDrop), ])
  }
  
  ## ROW ADDITIONS
  rowsAdd <- setdiff(rownames(lcurrent), rownames(lprior))
  if (length(rowsAdd) > 0) {
    tcurrent <-
      flextable::color(tcurrent, color = added, i = rowsAdd)
  }
  
  ## COLUMN DROPS
  colsDrop <- setdiff(colnames(lprior), colnames(lcurrent))
  if (length(colsDrop) > 0) {
    tprior <-
      flextable::color(tprior,
                       color = deleted,
                       j = colsDrop,
                       part = "all")
  }
  
  # if (isTRUE(listInfo$first[c])) {
  #   oldColDropped <- colDropped
  #   colDropped <- data.frame()
  # } else {
  #   colDropped <- rbind(colDropped, lprior[as.numeric(colsDrop), ])
  #   print(dropped)
  # }
  
  ## COLUMN ADDITIONS
  colsAdd <- setdiff(colnames(lcurrent), colnames(lprior))
  if (length(colsAdd) > 0) {
    tcurrent <-
      flextable::color(tcurrent,
                       color = added,
                       j = colsAdd,
                       part = "body")
  }
  
  ## DATA EDITS
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
  
  if (nrow(adjData) > 0) {
    tcurrent <-
      flextable::color(tcurrent,
                       color = changed,
                       i = adjData$r,
                       j = adjData$c)
  }
  
  ## ADD IN DROPPED ROWS AND COLUMNS
  if (isTRUE(listInfo$ext[p]) & (nrow(oldRowDropped) != 0)) {
    
    savedNames <- row.names(lcurrent)
    
    lcurrentD <- lcurrent
    for (d in 1:nrow(oldRowDropped)) {
      lcurrentD <-
        rbind.fill(lcurrentD[1:as.numeric(row.names(oldRowDropped))[d] - 1, ], oldRowDropped[d, ], lcurrentD[as.numeric(row.names(oldRowDropped))[d]:nrow(lcurrentD), ])
      print(rownames(lcurrentD))
    }
    
    rows <- c()
    for (r in 1:nrow(lcurrentD)) {
      if (row.names(lcurrentD)[r] %in% row.names(oldRowDropped)) {
        rows <- c(rows, paste0("INTPKGRNAME", r))
      } else {
        rows <- c(rows, savedNames[1])
        savedNames <- savedNames[-1]
      }
    }
    row.names(lcurrentD) <- rows
    
    tcurrentD <- flextable::flextable(lcurrentD)
    
    tcurrentD <- flextable::color(
      tcurrentD,
      color = tcurrent$body$styles$text$color$data,
      i = row.names(tcurrent$body$dataset),
      j = colnames(tcurrent$body$dataset)
    )
    
    tcurrentD <- flextable::color(
      tcurrentD,
      color = deleted,
      i = grep("INTPKGRNAME", row.names(lcurrentD), value =
                 TRUE),
      j = colnames(oldRowDropped)
    )
    
    tcurrentD <- flextable::color(
      tcurrentD,
      color = deleted,
      i = grep("INTPKGRNAME", row.names(lcurrentD), value =
                 TRUE),
      j = colnames(oldRowDropped)
    )
    
    tcurrentD <- flextable::color(
      tcurrentD,
      color = "white",
      i = grep("INTPKGRNAME", row.names(lcurrentD), value =
                 TRUE),
      j = setdiff(colnames(lcurrent), colnames(oldRowDropped))
    )
  }
  
  # if (isTRUE(listInfo$ext[p]) & (nrow(oldColDropped) != 0)) {
  #   
  #   if ((nrow(oldRowDropped) == 0)) {
  #     lcurrentD <- lcurrent
  #   }
  #   
  #   savedNames <- colnames(lcurrent)
  #   
  #   for (d in 1:nrow(oldColDropped)) {
  #     lcurrentD <-
  #       rbind.fill(lcurrentD[1:as.numeric(row.names(oldColDropped))[d] - 1, ], oldRowDropped[d, ], lcurrentD[as.numeric(row.names(oldRowDropped))[d]:nrow(lcurrentD), ])
  #     print(rownames(lcurrentD))
  #   }
  #   
  #   rows <- c()
  #   for (r in 1:nrow(lcurrentD)) {
  #     if (row.names(lcurrentD)[r] %in% row.names(oldRowDropped)) {
  #       rows <- c(rows, paste0("INTPKGRNAME", r))
  #     } else {
  #       rows <- c(rows, savedNames[1])
  #       savedNames <- savedNames[-1]
  #     }
  #   }
  #   row.names(lcurrentD) <- rows
  #   
  #   tcurrentD <- flextable::flextable(lcurrentD)
  #   
  #   tcurrentD <- flextable::color(
  #     tcurrentD,
  #     color = tcurrent$body$styles$text$color$data,
  #     i = row.names(tcurrent$body$dataset),
  #     j = colnames(tcurrent$body$dataset)
  #   )
  #   
  #   tcurrentD <- flextable::color(
  #     tcurrentD,
  #     color = deleted,
  #     i = grep("INTPKGRNAME", row.names(lcurrentD), value =
  #                TRUE),
  #     j = colnames(oldRowDropped)
  #   )
  #   
  #   tcurrentD <- flextable::color(
  #     tcurrentD,
  #     color = deleted,
  #     i = grep("INTPKGRNAME", row.names(lcurrentD), value =
  #                TRUE),
  #     j = colnames(oldRowDropped)
  #   )
  #   
  #   tcurrentD <- flextable::color(
  #     tcurrentD,
  #     color = "white",
  #     i = grep("INTPKGRNAME", row.names(lcurrentD), value =
  #                TRUE),
  #     j = setdiff(colnames(lcurrent), colnames(oldRowDropped))
  #   )
  # }
  
  
  ## SAVING
  if (isTRUE(listInfo$ext[p]) & (nrow(oldRowDropped) != 0 | nrow(oldColDropped) != 0)) {
    tables[[p]] <- tcurrentD
  } else {
    tables[[p]] <- tprior
  }
  tables[[c]] <- tcurrent
  
}

tables <- tables[listInfo[listInfo$ext == TRUE, "n"]]




