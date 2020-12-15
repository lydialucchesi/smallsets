#' Highlight changes
#'
#' @param list A list from \code{prep_smallset}
#' @param constant A colour to represent data cells that have not changed since previous smallset
#' @param changed A colour to represent data cells that have changed since previous smallset
#' @param added A colour to represent data cells that have been added since previous smallset
#' @param deleted A colour to represent cells that will be deleted from next smallset
#' @param author The author's name for the caption .Rmd file
#' @param captionScript A file name for the caption .Rmd template
#' @param captionDir A file path for the caption .Rmd template
#' @export
#' @import "flextable"
#' @importFrom gdata cbindX


highlight_changes <- function(list,
                              constant = "#927C5C",
                              changed = "cornflowerblue",
                              added = "#689F38",
                              deleted = "#b2a38c",
                              author = NULL,
                              captionScript = "captions",
                              captionDir = getwd()) {
  write_caption_template(
    authorName = author,
    col1 = changed,
    col2 = added,
    col3 = deleted,
    script = captionScript,
    pathway = captionDir
  )
  
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
  colDropped <- data.frame()
  colInfo <- data.frame()
  flag <- FALSE
  for (p in 1:(length(list) - 1)) {
    print(p)
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
      # row.names(tprior$body$dataset) <- rownames(lcurrent)
    }
    
    # SET TABLE COLOURS
    if (p > 1) {
      tprior <- tables[[p]]
    } else {
      tprior <- flextable::flextable(lprior)
      tprior <- flextable::color(tprior, color = constant)
    }
    
    tcurrent <- flextable::flextable(lcurrent)
    tcurrent <- flextable::color(tcurrent, color = constant)
    
    # what to do if INTPKGRNAME is in tprior
    if (!(FALSE %in% ((row.names(
      tprior$body$dataset
    )[!grepl("INTPKGRNAME", row.names(tprior$body$dataset))]) != rownames(lprior)))) {
      row.names(tprior$body$dataset) <- rownames(lprior)
    }
    row.names(tcurrent$body$dataset) <- rownames(lcurrent)
    
    if (isFALSE(listInfo[listInfo$n == c, "first"]))  {
      ivals <-
        row.names(tprior$body$dataset)[row.names(tprior$body$dataset) %in% row.names(lcurrent)]
      jvals <-
        colnames(tprior$body$dataset)[colnames(tprior$body$dataset) %in% colnames(lcurrent)]
      
      row.names(tprior$body$styles$text$color$data) <-
        row.names(tprior$body$dataset)
      
      if (length(row.names(tprior$body$dataset)) != length(row.names(tcurrent$body$dataset))) {
        row.names(tcurrent$body$dataset) <-
          row.names(tprior$body$dataset)[row.names(tprior$body$dataset) %in% row.names(tcurrent$body$dataset)]
      } else {
        row.names(tcurrent$body$dataset) <- row.names(tprior$body$dataset)
      }
      
      priorCols <-
        tprior$body$styles$text$color$data[as.character(ivals), jvals]
      
      tcurrent <- flextable::color(
        tcurrent,
        color = priorCols,
        i = (
          as.character(ivals) == row.names(tcurrent$body$dataset)
        ),
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
      oldRowDropped <- rowDropped
      rowDropped <- data.frame()
    } else {
      rowDropped <- rbind(rowDropped, lprior[as.numeric(rowsDrop),])
      if (p == (length(list) - 1)) {
        oldRowDropped <- rowDropped
      }
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
    
    if ((isTRUE(listInfo$first[c])) | (c == nrow(listInfo))) {
      oldColDropped <- colDropped
      colDropped <- data.frame()
      
      oldColInfo <- colInfo
      colInfo <- data.frame()
    } else {
      new <- select(lprior, colsDrop)
      if (length(colnames(colDropped)) > 0) {
        colDropped <- gdata::cbindX(colDropped, new)
      } else {
        colDropped <- new
      }
      if (p == (length(list) - 1)) {
        oldColDropped <- colDropped
      }
      new2 <-
        data.frame(name = colnames(lprior), place = seq(1, length(colnames(lprior))))
      
      if (nrow(colInfo) > 0) {
        if (length(colsDrop) > 0) {
          newColInfo <- subset(new2, name %in% colnames(colDropped))
          for (s in 1:nrow(newColInfo)) {
            if (newColInfo$place[s] %in% colInfo$place) {
              newColInfo$place[s] <- newColInfo$place[s] + 1
            }
          }
          colInfo <-
            rbind(colInfo, newColInfo)
        }
      } else {
        colInfo <- subset(new2, name %in% colnames(colDropped))
      }
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
      selectionCols <- colnames(lprior)[colnames(lprior) %in% colnames(lcurrent)]
      lcurrentAdj <- subset(lcurrent, select = selectionCols)
    } else {
      lcurrentAdj <- lcurrent
    }
    
    # original <- setdiff(subset(lpriorAdj, select = colnames(lcurrentAdj)), lcurrentAdj)
    # update <- setdiff(lcurrentAdj, subset(lpriorAdj, select = colnames(lcurrentAdj)))
    
    original <- subset(lpriorAdj, select = colnames(lcurrentAdj))
    update <- subset(lcurrentAdj, select = colnames(lcurrentAdj))
    
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
      for (h in 1:nrow(adjData)) {
        tcurrent <-
          flextable::color(
            tcurrent,
            color = changed,
            i = as.character(adjData$r[h]),
            j = adjData$c[h]
          )
      }
    }
    
    ## ADD IN DROPPED ROWS AND COLUMNS
    if (isTRUE(listInfo$ext[p]) & nrow(oldRowDropped) != 0) {
      savedNames <- row.names(lprior)
      
      lcurrentD <- lprior
      for (d in 1:nrow(oldRowDropped)) {
        if (row.names(oldRowDropped)[d] > nrow(lcurrentD)) {
          lcurrentD <-
            bind_rows(lcurrentD[1:as.numeric(row.names(oldRowDropped))[d] - 1,],
                      oldRowDropped[d,])
        } else {
          lcurrentD <-
            bind_rows(lcurrentD[1:as.numeric(row.names(oldRowDropped))[d] - 1,],
                      oldRowDropped[d,],
                      lcurrentD[as.numeric(row.names(oldRowDropped))[d]:nrow(lcurrentD),])
        }
      }
      
      rows1 <- c()
      for (r in 1:nrow(lcurrentD)) {
        if (row.names(lcurrentD)[r] %in% row.names(oldRowDropped)) {
          rows1 <- c(rows1, paste0("INTPKGRNAME", r))
        } else {
          rows1 <- c(rows1, savedNames[1])
          savedNames <- savedNames[-1]
        }
      }
      row.names(lcurrentD) <- rows1
      
      rows2 <- c()
      priorRows <- row.names(tprior$body$dataset)
      for (r in 1:nrow(lcurrentD)) {
        if (grepl("INTPKGRNAME", row.names(lcurrentD)[r])) {
          rows2 <- c(rows2, row.names(lcurrentD)[r])
        } else {
          rows2 <- c(rows2, priorRows[1])
          priorRows <- priorRows[-1]
        }
      }
      row.names(lcurrentD) <- rows2
      
      tcurrentD <- flextable::flextable(lcurrentD)
      
      
      tcurrentD <- flextable::color(
        tcurrentD,
        color = tprior$body$styles$text$color$data,
        i = row.names(tprior$body$dataset),
        j = colnames(tprior$body$dataset)
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
        j = setdiff(colnames(lprior), colnames(oldRowDropped))
      )
      
      # do i need to then save rows as rows1?
      row.names(tcurrentD$body$dataset) <- rows1
      
    }
    
    if ((isTRUE(listInfo$ext[p]) &
         (ncol(oldColDropped) != 0)) |
        ((p == (length(list) - 1)) & (ncol(oldColDropped) != 0))) {
      if ((nrow(oldRowDropped) == 0) & (p == (length(list) - 1))) {
        lcurrentD <- lcurrent
      }
      if ((nrow(oldRowDropped) == 0) & (p != (length(list) - 1))) {
        lcurrentD <- lprior
      }
      
      for (col in 1:nrow(oldColInfo)) {
        if (col == 1) {
          lcurrentD <-
            cbind(lcurrent[, 1:oldColInfo$place[col] - 1],
                  select(oldColDropped, oldColInfo$name[col]),
                  lcurrent[, (oldColInfo$place[col]):ncol(lcurrent)])
        } else {
          lcurrentD <-
            cbind(lcurrentD[, 1:oldColInfo$place[col] - 1],
                  select(oldColDropped, oldColInfo$name[col]),
                  lcurrentD[, (oldColInfo$place[col]):ncol(lcurrentD)])
        }
        
      }
      
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
        i = row.names(oldColDropped),
        j = as.character(oldColInfo$name)
      )
      
      tcurrentD <- flextable::color(
        tcurrentD,
        color = "white",
        i = setdiff(row.names(lcurrent), row.names(oldColDropped)),
        j = as.character(oldColInfo$name)
      )
    }
    
    
    ## SAVING
    if ((((isTRUE(listInfo$ext[p]) & nrow(oldRowDropped) != 0)) | 
         (((isTRUE(listInfo$ext[p]) & 
            (ncol(oldColDropped) != 0)) | 
           ((p == (length(list) - 1)) & (ncol(oldColDropped) != 0))))) & (p == (length(list) - 1))) {
      tables[[c]] <- tcurrentD
    } else {
      tables[[c]] <- tcurrent
    }
    
    if ((isTRUE(listInfo$ext[p]) &
         (nrow(oldRowDropped) != 0)) |
        ((isTRUE(listInfo$ext[p]) &
          (ncol(oldColDropped) != 0)) |
         ((p == (length(
           list
         ) - 1)) & (ncol(oldColDropped) != 0)))) {
      tables[[p]] <- tcurrentD
    } else {
      tables[[p]] <- tprior
    }
    
  }
  
  tables <- tables[listInfo[listInfo$ext == TRUE, "n"]]
  
  return(list(
    tables,
    constant,
    changed,
    added,
    deleted,
    captionScript,
    captionDir
  ))
}
