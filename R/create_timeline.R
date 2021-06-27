#' Create the timeline
#'
#' @description  The function creates a smallset timeline using output from
#'   \code{prepare_smallset}. Timelines have many customisation options. They
#'   are detailed below.
#'
#' @param snapshotList List output from \code{prepare_smallset}.
#' @param constant Hex colour code. Colour represents data that have not changed
#'   since previous snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param changed Hex colour code. Colour represents data that have changed
#'   since previous snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param added Hex colour code. Colour represents data that have been added
#'   since previous snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param deleted Hex colour code. Colour represents data that will be deleted
#'   prior to next snapshot. Can pass in a list with a colour and transparency
#'   value (0 to 1) for that colour.
#' @param colScheme NULL, colour scheme name, or vector. If NULL, uses four
#'   colour arguments above. If colour scheme name, uses built-in scheme with
#'   colours pre-assigned to the four preprocessing states (constant, changed,
#'   added, deleted). If vector, it must be a vector of length five, with the
#'   first element being the colour scheme name followed by the four
#'   preprocessing states in the order that they should be assigned to scheme
#'   colours (e.g.,, c("colScheme1", "changed", "constant", "deleted",
#'   "added")).
#' @param abstract TRUE or FALSE. FALSE prints data values in tables.
#' @param ghostData TRUE or FALSE. TRUE includes blank spaces where data have
#'   been removed.
#' @param highlightNA TRUE or FALSE. TRUE plots a lighter colour value to signal
#'   data value is missing.
#' @param sizing List of size specifications. Can specify sizes for column
#'   names, table tiles, caption text, stamp symbols, stamp circles, printed
#'   data, legend text, legend icons, timeline title, timeline subtitle,
#'   timeline footnote, and resume marker.
#' @param truncateData TRUE or FALSE. FALSE if data do not need to be truncated
#'   to fit within table tiles. Otherwise, an integer specifying width of data
#'   value (width includes "...").
#' @param accentCols Either "darker" or "lighter" for stamp colour. Can enter a
#'   list corresponding to specific actions.
#' @param accentColsDif Value between 0 and 1. Corresponds to how much lighter
#'   or darker accent colour will be. Can pass a list with different accent
#'   values for different colours.
#' @param otherTextCol Value between 0 and 1. Default is 1, which is when column
#'   names are black. 0 means columns will be the constant colour.
#' @param timelineRows Integer greater than or equal to one. Number of rows to
#'   divide the smallset timeline into.
#' @param timelineFont One of "sans", "serif", or "mono".
#' @param captionSpace Value greater than or equal to .5. Higher values create
#'   more caption space. Default is 1.
#' @param captionTemplateName Name of caption template. Can be included so
#'   template is not overwritten when running \code{prepare_smallset}.
#' @param captionTemplateDir Name of caption template directory. Can be included
#'   so template is not overwritten when running \code{prepare_smallset}.
#' @import "patchwork" "gplots" "colorspace" "magrittr" "dplyr"
#' @importFrom plyr mapvalues
#' @export

create_timeline <-
  function(snapshotList,
           constant = NULL,
           changed = NULL,
           added = NULL,
           deleted = NULL,
           colScheme = "colScheme1",
           abstract = TRUE,
           ghostData = TRUE,
           highlightNA = FALSE,
           sizing = list(
             "columns" = 2,
             "tiles" = 1,
             "captions" = 8,
             "data" = 2.5,
             "legendText" = 7,
             "legendIcons" = 1,
             "title" = 10,
             "subtitle" = 8,
             "footnote" = 7,
             "resume" = .25
           ),
           truncateData = FALSE,
           accentCols = "darker",
           accentColsDif = .8,
           otherTextCol = 1,
           timelineRows = 1,
           timelineFont = "sans",
           captionSpace = 1,
           captionTemplateName = NULL,
           captionTemplateDir = NULL) {
    if (missing(snapshotList)) {
      stop(
        "Must include object from prepare_smallset. See snapshotList argument in ?create_timeline."
      )
    }
    
    if ((class(snapshotList)[1] != "smallsetSnapshots"))
      stop(
        "Object snapshotList is not of class smallsetSnapshots (output from prepare_smallset).'"
      )
    
    items <- seq(1, length(snapshotList[[1]]), 1)
    
    # Get four colours ready
    if (is.null(constant) & is.null(changed) & is.null(added) & is.null(deleted)) {
      chosenScheme <- return_scheme(colScheme = colScheme)
    } else {
      chosenScheme <- NULL
      colScheme <- NULL
    }
    
    if (!is.null(colScheme) & length(colScheme) > 1) {
      constantPlace <- match("constant", colScheme) - 1
      constant <- unlist(chosenScheme[constantPlace][1])[1]
      constantAlpha <-
        as.numeric(unlist(chosenScheme[constantPlace][1])[2])
      
      changedPlace <- match("changed", colScheme) - 1
      changed <- unlist(chosenScheme[changedPlace][1])[1]
      changedAlpha <-
        as.numeric(unlist(chosenScheme[changedPlace][1])[2])
      
      addedPlace <- match("added", colScheme) - 1
      added <- unlist(chosenScheme[addedPlace][1])[1]
      addedAlpha <-
        as.numeric(unlist(chosenScheme[addedPlace][1])[2])
      
      deletedPlace <- match("constant", colScheme) - 1
      deleted <- unlist(chosenScheme[deletedPlace][1])[1]
      deletedAlpha <-
        as.numeric(unlist(chosenScheme[deletedPlace][1])[2])
      
    } else if ((!is.null(colScheme) & length(colScheme) == 1)) {
      constant <- unlist(chosenScheme[1][1])[1]
      constantAlpha <- as.numeric(unlist(chosenScheme[1][1])[2])
      
      changed <- unlist(chosenScheme[2][1])[1]
      changedAlpha <- as.numeric(unlist(chosenScheme[2][1])[2])
      
      added <- unlist(chosenScheme[3][1])[1]
      addedAlpha <- as.numeric(unlist(chosenScheme[3][1])[2])
      
      deleted <- unlist(chosenScheme[4][1])[1]
      deletedAlpha <- as.numeric(unlist(chosenScheme[4][1])[2])
      
    } else {
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
      
    }
    
    tileAlphas <-
      data.frame(
        colValue = c(constant, changed, added, deleted),
        alpha = c(constantAlpha, changedAlpha, addedAlpha, deletedAlpha)
      )
    
    for (i in 1:length(snapshotList[[1]])) {
      temp <- snapshotList[[1]][[i]]$body$styles$text$color$data
      for (c in colnames(temp)) {
        temp[, c] <- plyr::mapvalues(
          temp[, c],
          from = c("#808080", "#008000", "#0000FF", "#FF0000"),
          to = c(constant, changed, added, deleted),
          warn_missing = FALSE
        )
      }
      snapshotList[[1]][[i]]$body$styles$text$color$data <- temp
    }
    
    # Set argument if it is not specified
    if (is.null(sizing[["columns"]])) {
      sizing[["columns"]] = 2
    }
    
    if (is.null(sizing[["tiles"]])) {
      sizing[["tiles"]] = 1
    }
    
    if (is.null(sizing[["captions"]])) {
      sizing[["captions"]] = 8
    }
    
    if (is.null(sizing[["data"]])) {
      sizing[["data"]] = 2.5
    }
    
    if (is.null(sizing[["legendText"]])) {
      sizing[["legendText"]] = 7
    }
    
    if (is.null(sizing[["legendIcons"]])) {
      sizing[["legendIcons"]] = 1
    }
    
    if (is.null(sizing[["title"]])) {
      sizing[["title"]] = 10
    }
    
    if (is.null(sizing[["subtitle"]])) {
      sizing[["subtitle"]] = 8
    }
    
    if (is.null(sizing[["footnote"]])) {
      sizing[["footnote"]] = 7
    }
    
    if (is.null(sizing[["resume"]])) {
      sizing[["resume"]] = .25
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
    
    # Prepare timeline accent colours
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
    
    # Identify which colours are present in the timeline
    colsPresent <- c()
    for (u in 1:length(snapshotList[[1]])) {
      uniqueCols <- snapshotList[[1]][[u]]$body$styles$text$color$data
      uniqueCols <- as.vector(as.matrix(uniqueCols))
      uniqueCols <- unique(uniqueCols)
      colsPresent <- c(colsPresent, uniqueCols)
    }
    
    colsPresent <- unique(colsPresent)
    
    # Prepare colour legend
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
    
    # Insert ghost data
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
      
      extTables <-
        lapply(items, ghostDF1, ghostDF2, snapshotList, FUN = add_ghost_data)
    } else {
      extTables <- lapply(items, snapshotList, FUN = extract_tables)
    }
    
    maxDims <- get_timeline_dimensions(extTables)
    
    snapshotList[[5]] <- constant
    snapshotList[[6]] <- changed
    snapshotList[[7]] <- added
    snapshotList[[8]] <- deleted
    
    tileAlphas <- subset(tileAlphas, tileAlphas$colValue %in% colsPresent)
    snapshotList[[9]] <- tileAlphas
    
    # Make the timeline plot for each snapshot
    l <-
      lapply(
        items,
        extTables,
        snapshotList,
        abstract,
        ghostData,
        sizing,
        truncateData,
        accentCols,
        accentColsDif,
        otherTextColour,
        maxDims,
        timelineFont,
        captionSpace,
        accents,
        legendDF,
        highlightNA,
        captionTemplateName,
        captionTemplateDir,
        timelineRows,
        FUN = make_timeline_plot
      )
    
    # Set a limits for x-axis if necessary
    if (timelineRows > 1) {
      m <- maxDims[[1]] + .5
      if (!is.null(snapshotList[[4]])) {
        m <- maxDims[[1]] + 2.51
      }
      for (p in 1:length(l)) {
        l[[p]] <- l[[p]] + xlim(c(0, m))
      }
    }
    
    # Connect snapshots into timeline
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
    
    # Add timeline title, subtitle, and footnote
    if (is.null(captionTemplateName) &
        is.null(captionTemplateDir)) {
      annotateInfo <-
        as.data.frame(readLines(paste0(
          snapshotList[[3]], "/", snapshotList[[2]], ".Rmd"
        )))
    } else if (!is.null(captionTemplateName) &
               is.null(captionTemplateDir)) {
      annotateInfo <-
        as.data.frame(readLines(paste0(
          snapshotList[[3]], "/", captionTemplateName, ".Rmd"
        )))
    } else if (is.null(captionTemplateName) &
               !is.null(captionTemplateDir)) {
      annotateInfo <-
        as.data.frame(readLines(paste0(
          captionTemplateDir, "/", snapshotList[[2]], ".Rmd"
        )))
    } else {
      annotateInfo <-
        as.data.frame(readLines(
          paste0(captionTemplateDir, "/", captionTemplateName, ".Rmd")
        ))
    }
    
    colnames(annotateInfo) <- c("lines")
    
    title <-
      subset(annotateInfo,
             grepl("Timeline title: ", annotateInfo$lines))
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
    
    # Set timeline design choices
    fontChoice <-
      paste0(
        " & theme(text = element_text(family = '",
        timelineFont,
        "', colour = otherTextColour),
        plot.title = element_text(size = ",
        sizing[["title"]],
        "), ",
        "plot.subtitle = element_text(size = ",
        sizing[["subtitle"]],
        "), ",
        "plot.caption = element_text(size = ",
        sizing[["footnote"]],
        "), ",
        "legend.key.size = unit(",
        sizing[["legendIcons"]],
        ", 'line'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'))"
      )
    
    patchedPlots <-
      paste0(patchedPlots, timelineHeader, fontChoice)
    o <- return(eval(parse(text = patchedPlots)))
    
    oldClass(o) <- c("smallsetTimeline", class(o))
    
    o
    
  }
