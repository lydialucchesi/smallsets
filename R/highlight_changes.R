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

highlight_changes <-
  function(list,
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
    # for (p in 1:(length(list) - 1)) {
    for (p in 1:8) {
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
      
      row.names(tprior$body$dataset) <- rownames(lprior)
      row.names(tcurrent$body$dataset) <- rownames(lcurrent)
      
      if (isFALSE(listInfo[listInfo$n == c, "first"])) {
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
          i = (as.character(ivals) == row.names(tcurrent$body$dataset)),
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
      
      if (isTRUE(listInfo$first[c])) {
        oldColDropped <- colDropped
        colDropped <- data.frame()
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
        colInfo <- subset(new2, name %in% colnames(oldColDropped))
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
          flextable::color(tcurrent,
                           color = changed,
                           i = as.character(adjData$r),
                           j = adjData$c)
      }
      
      ## ADD IN DROPPED ROWS AND COLUMNS
      if (isTRUE(listInfo$ext[p]) & nrow(oldRowDropped) != 0) {
        savedNames <- row.names(lprior)
        
        lcurrentD <- lprior
        for (d in 1:nrow(oldRowDropped)) {
          lcurrentD <-
            bind_rows(lcurrentD[1:as.numeric(row.names(oldRowDropped))[d] - 1,], 
                       oldRowDropped[d,], 
                       lcurrentD[as.numeric(row.names(oldRowDropped))[d]:nrow(lcurrentD),])
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
      }
      
      if ((isTRUE(listInfo$ext[p]) &
           (ncol(oldColDropped) != 0)) |
          ((p == (length(list) - 1)) & (ncol(oldColDropped) != 0))) {
        if (nrow(oldRowDropped) == 0) {
          lcurrentD <- lprior
        }
        
        for (col in 1:nrow(colInfo)) {
          lcurrentD <-
            cbind(lcurrent[, 1:colInfo$place[col] - 1],
                  select(oldColDropped, colInfo$name[col]),
                  lcurrent[, (colInfo$place[col]):ncol(lcurrent)])
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
          j = as.character(colInfo$name)
        )
        
        tcurrentD <- flextable::color(
          tcurrentD,
          color = "white",
          i = setdiff(row.names(lcurrent), row.names(oldColDropped)),
          j = as.character(colInfo$name)
        )
      }
      
      
      ## SAVING
      if (((isTRUE(listInfo$ext[p]) &
            (nrow(oldRowDropped) != 0)) |
           ((isTRUE(listInfo$ext[p]) &
             (ncol(oldColDropped) != 0)) |
            ((p == (length(
              list
            ) - 1)) & (ncol(oldColDropped) != 0)))) & (p == length(list) - 1)) {
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
