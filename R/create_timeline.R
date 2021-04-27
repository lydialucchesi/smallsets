#' Create the timeline
#'
#' @param snapshotList A list from \code{highlight_changes}
#' @param constant A colour to represent data that have not changed.
#' @param changed A colour to represent data that have changed.
#' @param added A colour to represent data that have been added.
#' @param deleted A colour to represent data that have been deleted.
#' @param abstract TRUE or FALSE for hiding data values in timeline.
#' @param ghostData TRUE or FALSE for including blank tiles where data have been removed.
#' @param highlightNA TRUE or FALSE for using a lighter colour to signal data value is missing.
#' @param sizing A list of size specifications for the column names, the tiles, the captions, the circles, and the symbols
#' @param truncateData FALSE if data do not need to be truncated to fit in tiles. Otherwise, an integer specifying width of data value (width includes "...").
#' @param accentCols Either "darker" or "lighter" for stamp colour. Can enter a list corresponding to specific actions.
#' @param accentColsDif Degree to which stamp colour is darker or lighter. Can enter a list corresponding to specific actions.
#' @param otherTextCol Value between 0 and 1. Default is 1, meaning column names will be black. 0 means columns will be the same colour as the constant colour.
#' @param stampLoc Location of stamp. 1 = top left. 2 = top right. 3 = bottom left. 4 = bottom right. 5 = center.
#' @param timelineRows Number of rows to divide the smallset timeline into.
#' @param timelineFont Font family.
#' @param captionSpace Increase room for captions. Any value greater than or equal to .5. Default is one.
#' @param captionTemplateName Name of caption template if renamed so not written over when running highlight_changes function.
#' @param captionTemplateDir Name of caption template directory if moved so not written over when running highlight_changes function.
#' @export
#' @import "patchwork" "gplots" "colorspace" "magrittr" "dplyr"
#' @importFrom plyr mapvalues

create_timeline <-
  function(snapshotList,
           constant = list("#cecfd6", .7),
           changed = list("#0f3d1c", .7),
           added = list("#a35222", .7),
           deleted = list("#3e4d63", .7),
           abstract = TRUE,
           ghostData = FALSE,
           highlightNA = FALSE,
           sizing = list(
             "columns" = 2,
             "tiles" = 1,
             "captions" = 8,
             "circles" = .15,
             "symbols" = 2.5,
             "data" = 2.5,
             "legend" = 7
           ),
           truncateData = FALSE,
           accentCols = "darker",
           accentColsDif = .5,
           otherTextCol = 1,
           stampLoc = 1,
           timelineRows = NULL,
           timelineFont = "sans",
           captionSpace = 1,
           captionTemplateName = NULL,
           captionTemplateDir = NULL) {
    items <- seq(1, length(snapshotList[[1]]), 1)
    
    if (!is.list(constant)) {
      constantAlpha = .4
    } else {
      constantAlpha = constant[[2]]
      constant = constant[[1]]
    }
    
    if (!is.list(changed)) {
      changedAlpha = .4
    } else {
      changedAlpha = changed[[2]]
      changed = changed[[1]]
    }
    
    if (!is.list(added)) {
      addedAlpha = .4
    } else {
      addedAlpha = added[[2]]
      added = added[[1]]
    }
    
    if (!is.list(deleted)) {
      deletedAlpha = .4
    } else {
      deletedAlpha = deleted[[2]]
      deleted = deleted[[1]]
    }
    
    tileAlphas <-
      data.frame(
        colValue = c(constant, changed, added, deleted),
        alpha = c(constantAlpha, changedAlpha, addedAlpha, deletedAlpha)
      )
    
    for (i in 1:length(snapshotList)) {
      temp <- snapshotList[[1]][[i]]$body$styles$text$color$data
      for (c in colnames(temp)) {
        temp[, c] <- mapvalues(
          temp[, c],
          from = c("#808080", "#FFFF00", "#0000FF", "#FF0000"),
          to = c(constant, changed, added, deleted),
          warn_missing = FALSE
        )
      }
      snapshotList[[1]][[i]]$body$styles$text$color$data <- temp
    }
    
    if (is.null(sizing[["columns"]])) {
      sizing[["columns"]] = 2
    }
    
    if (is.null(sizing[["tiles"]])) {
      sizing[["tiles"]] = 1
    }
    
    if (is.null(sizing[["captions"]])) {
      sizing[["captions"]] = 8
    }
    
    if (is.null(sizing[["circles"]])) {
      sizing[["circles"]] = .15
    }
    
    if (is.null(sizing[["symbols"]])) {
      sizing[["symbols"]] = 2.5
    }
    
    if (is.null(sizing[["data"]])) {
      sizing[["data"]] = 2.5
    }
    
    if (is.null(sizing[["legend"]])) {
      sizing[["legend"]] = 2.5
    }
    
    if (is.list(accentCols)) {
      if (is.null(accentCols[["constant"]])) {
        accentCols[["constant"]] = "darker"
      }
      
      if (is.null(accentCols[["changed"]])) {
        accentCols[["changed"]] = "darker"
      }
      
      if (is.null(accentCols[["added"]])) {
        accentCols[["added"]] = "darker"
      }
      
      if (is.null(accentCols[["deleted"]])) {
        accentCols[["deleted"]] = "darker"
      }
    } else {
      if (accentCols == "darker") {
        accentCols <-
          list(
            constant = "darker",
            changed = "darker",
            added = "darker",
            deleted = "darker"
          )
      } else {
        accentCols <-
          list(
            constant = "lighter",
            changed = "lighter",
            added = "lighter",
            deleted = "lighter"
          )
      }
    }
    
    if (is.list(accentColsDif)) {
      if (is.null(accentColsDif[["constant"]])) {
        accentColsDif[["constant"]] = .5
      }
      
      if (is.null(accentColsDif[["changed"]])) {
        accentColsDif[["changed"]] = .5
      }
      
      if (is.null(accentColsDif[["added"]])) {
        accentColsDif[["added"]] = .5
      }
      
      if (is.null(accentColsDif[["deleted"]])) {
        accentColsDif[["deleted"]] = .5
      }
    } else {
      accentColsDif <-
        list(
          constant = accentColsDif,
          changed = accentColsDif,
          added = accentColsDif,
          deleted = accentColsDif
        )
    }
    
    maxDims <- get_timeline_dimensions(snapshotList)
    
    accents <-
      data.frame(
        colValue = c(constant, changed, added, deleted),
        accent = unlist(accentCols),
        degree = unlist(accentColsDif)
      )
    rownames(accents) <- NULL
    accents$hex <-
      ifelse(
        grepl("#", accents$colValue) == TRUE,
        as.character(accents$colValue),
        col2hex(accents$colValue)
      )
    accents$colValue2 <-
      ifelse(
        accents$accent == "darker",
        darken(accents$hex, accents$degree),
        lighten(accents$hex, accents$degree)
      )
    accents$hex <- NULL
    
    colsPresent <- c()
    for (u in 1:length(snapshotList[[1]])) {
      uniqueCols <- snapshotList[[1]][[u]]$body$styles$text$color$data
      uniqueCols <- as.vector(as.matrix(uniqueCols))
      uniqueCols <- unique(uniqueCols)
      colsPresent <- c(colsPresent, uniqueCols)
    }
    
    colsPresent <- unique(colsPresent)
    
    if (isTRUE(highlightNA)) {
      descriptions <-
        c(
          "Data have not changed since previous snapshot.\nLighter shade signals a missing data value.",
          "Data have changed since previous snapshot.\nLighter shade signals a missing data value.",
          "Data have been added since previous snapshot.\nLighter shade signals a missing data value.",
          "Data will be removed prior to the next snapshot.\nLighter shade signals a missing data value."
        )
    } else {
      descriptions <-
        c(
          "Data have not changed since previous snapshot.",
          "Data have changed since previous snapshot.",
          "Data have been added since previous snapshot.",
          "Data will be removed prior to the next snapshot."
        )
    }
    
    legendDF <- data.frame(colValue = c(), description = c())
    colItems <- c(constant, changed, added, deleted)
    for (colItemNum in 1:length(colItems)) {
      if (colItems[colItemNum] %in% colsPresent) {
        legendAddition <-
          data.frame(colValue = c(colItems[colItemNum]),
                     description = descriptions[colItemNum])
        legendDF <- rbind(legendDF, legendAddition)
      }
    }
    
    otherTextColour <- darken(legendDF$colValue[1], otherTextCol)
    
    if (isTRUE(ghostData)) {
      ghostDF1 <-
        as.data.frame(snapshotList[[1]][[1]]$body$styles$text$color$data) %>%
        mutate_all(as.character)
      
      for (i in 1:nrow(ghostDF1)) {
        for (j in 1:length(colnames(ghostDF1))) {
          ghostDF1[i, j] <- "#FFFFFF"
        }
      }
      
      ghostDF2 <-
        as.data.frame(snapshotList[[1]][[1]]$body$dataset) %>%
        mutate_all(as.character)
      
      for (i in 1:nrow(ghostDF2)) {
        for (j in 1:length(colnames(ghostDF2))) {
          ghostDF2[i, j] <- ""
        }
      }
      
      row.names(ghostDF1) <- row.names(ghostDF2)
      
      snapshotList[[5]] <- constant
      snapshotList[[6]] <- changed
      snapshotList[[7]] <- added
      snapshotList[[8]] <- deleted
      snapshotList[[9]] <- tileAlphas
      
      l <-
        lapply(
          items,
          snapshotList,
          abstract,
          sizing,
          truncateData,
          accentCols,
          accentColsDif,
          otherTextColour,
          stampLoc,
          maxDims,
          timelineFont,
          captionSpace,
          accents,
          legendDF,
          ghostDF1,
          ghostDF2,
          highlightNA,
          captionTemplateName,
          captionTemplateDir,
          FUN = make_timeline_plot2
        )
    } else {
      l <-
        lapply(
          items,
          snapshotList,
          abstract,
          sizing,
          truncateData,
          accentCols,
          accentColsDif,
          otherTextColour,
          stampLoc,
          maxDims,
          timelineFont,
          captionSpace,
          accents,
          legendDF,
          highlightNA,
          captionTemplateName,
          captionTemplateDir,
          FUN = make_timeline_plot1
        )
    }
    
    patchedPlots <- ""
    for (s in 1:length(l)) {
      addPlot <- paste0("l[[", as.character(s), "]] + ")
      patchedPlots <- paste0(patchedPlots, addPlot)
    }
    
    if (is.null(timelineRows)) {
      patchedPlots <- paste0(patchedPlots, "plot_layout()")
    } else {
      patchedPlots <-
        paste0(
          patchedPlots,
          "plot_layout(nrow = ",
          as.character(timelineRows),
          ", guides = 'collect')"
        )
    }
    
    annotateInfo <-
      as.data.frame(readLines(paste0(
        snapshotList[[3]], "/", snapshotList[[2]], ".Rmd"
      )))
    colnames(annotateInfo) <- c("lines")
    
    title <-
      subset(annotateInfo, grepl("Timeline title: ", annotateInfo$lines))
    title <- sub("Timeline title*: ", "", title$lines)[1]
    
    subtitle <-
      subset(annotateInfo,
             grepl("Timeline subtitle: ", annotateInfo$lines))
    subtitle <- sub("Timeline subtitle*: ", "", subtitle$lines)[1]
    
    footnote <-
      subset(annotateInfo,
             grepl("Timeline footnote: ", annotateInfo$lines))
    footnote <- sub("Timeline footnote*: ", "", footnote$lines)[1]
    
    quote <- "'"
    timelineHeader <- paste0(
      " + plot_annotation(title = ",
      quote,
      title,
      quote,
      ", subtitle = ",
      quote,
      as.character(subtitle),
      quote,
      ", caption = ",
      quote,
      as.character(footnote),
      quote,
      ")"
    )
    
    fontChoice <-
      paste0(
        " & theme(text = element_text(family = '",
        timelineFont,
        "', colour = otherTextColour), legend.position = 'bottom', legend.title = element_blank(), legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'))"
      )
    
    patchedPlots <- paste0(patchedPlots, timelineHeader, fontChoice)
    
    return(eval(parse(text = patchedPlots)))
    
  }
