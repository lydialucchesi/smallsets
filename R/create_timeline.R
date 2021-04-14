#' Create the timeline
#'
#' @param snapshotList A list from \code{highlight_changes}
#' @param abstract TRUE or FALSE for hiding data values in timeline.
#' @param sizing A list of size specifications for the column names, the tiles, the captions, the cirlces, and the symbols
#' @param accentCols Either "darker" or "lighter" for stamp colour. Can enter a list corresponding to specific actions.
#' @param accentColsDif Degree to which stamp colour is darker or lighter. Can enter a list corresponding to specific actions.
#' @param otherTextCol Value between 0 and 1. Default is 1, meaning column names will be black. 0 means columns will be the same colour as the constant colour.
#' @param stampLoc Location of stamp. 1 = top left. 2 = top right. 3 = bottom left. 4 = bottom right. 5 = center.
#' @param timelineRows Number of rows to divide the smallset timeline into.
#' @param timelineFont Font family.
#' @param captionSpace Increase room for captions. Any value greater than or equal to .5. Default is one.
#' @export
#' @import "patchwork" "gplots" "colorspace"

create_timeline <-
  function(snapshotList,
           abstract = TRUE,
           sizing = list(
             "columns" = 2,
             "tiles" = 1,
             "captions" = 8,
             "circles" = .15,
             "symbols" = 2.5,
             "data" = 2.5,
             "legend" = 7
           ),
           accentCols = "darker",
           accentColsDif = .5,
           otherTextCol = 1,
           stampLoc = 1,
           timelineRows = NULL,
           timelineFont = "sans",
           captionSpace = 1) {
    
    items <- seq(1, length(snapshotList[[1]]), 1)
    
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
        colValue = c(snapshotList[[2]], snapshotList[[3]], snapshotList[[4]], snapshotList[[5]]),
        accent = unlist(accentCols),
        degree = unlist(accentColsDif)
      )
    rownames(accents) <- NULL
    accents$hex <-
      ifelse(grepl("#", accents$colValue) == TRUE,
             as.character(accents$colValue),
             col2hex(accents$colValue))
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
    descriptions <-  c("Data have not changed since previous snapshot.",
                       "Data have changed since previous snapshot.",
                       "Data have been added since previous snapshot.",
                       "Data will be removed prior to the next snapshot.")
    legendDF <- data.frame(colValue = c(), description = c())
    for (colItemNum in 2:5) {
      if (snapshotList[[colItemNum]] %in% colsPresent) {
        legendAddition <- data.frame(colValue = c(snapshotList[[colItemNum]]),
                                     description = descriptions[colItemNum - 1])
        legendDF <- rbind(legendDF, legendAddition)
      }
    }
    
    otherTextColour <- darken(legendDF$colValue[1], otherTextCol)
    
    l <-
      lapply(
        items,
        snapshotList,
        abstract,
        sizing,
        accentCols,
        accentColsDif,
        otherTextColour,
        stampLoc,
        maxDims,
        timelineFont,
        captionSpace,
        accents,
        legendDF,
        FUN = make_timeline_plot
      )
    
    patchedPlots <- ""
    for (s in 1:length(l)) {
      addPlot <- paste0("l[[", as.character(s), "]] + ")
      patchedPlots <- paste0(patchedPlots, addPlot)
    }
    
    if (is.null(timelineRows)) {
      patchedPlots <- paste0(patchedPlots, "plot_layout()")
    } else {
      patchedPlots <-
        paste0(patchedPlots,
               "plot_layout(nrow = ",
               as.character(timelineRows),
               ", guides = 'collect')")
    }
    
    annotateInfo <-
      as.data.frame(readLines(paste0(snapshotList[[7]], "/", snapshotList[[6]], ".Rmd")))
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
      paste0(" & theme(text = element_text(family = '", timelineFont, 
             "', colour = otherTextColour), legend.position = 'bottom', legend.title=element_blank(), legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'))")

    patchedPlots <- paste0(patchedPlots, timelineHeader, fontChoice)
    
    return(eval(parse(text = patchedPlots)))
    
  }

