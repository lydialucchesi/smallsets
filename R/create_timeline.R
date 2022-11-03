#' Create the Timeline
#' @description  Creates the Timeline.
#' @keywords internal
#' @import "patchwork" "colorspace" "magrittr" "dplyr"
#' @importFrom plyr mapvalues
#' @importFrom gplots col2hex

create_timeline <-
  function(snapshotList,
           constant,
           changed,
           added,
           deleted,
           colScheme,
           abstract,
           ghostData,
           highlightNA,
           sizing,
           truncateData,
           rotateHeader,
           headerSpace,
           accentCols,
           accentColsDif,
           otherTextCol,
           timelineRows,
           timelineFont,
           captionSpace) {
    if (missing(snapshotList)) {
      stop(
        "Must include object from prepare_smallset. See snapshotList argument in ?create_timeline."
      )
    }
    
    if ((class(snapshotList)[1] != "smallsetSnapshots"))
      stop(
        "Object snapshotList is not of class smallsetSnapshots (output from prepare_smallset).'"
      )
    
    if (length(headerSpace) != 2) {
      headerSpace <- c(1, .5)
      print(
        "headerSpace must be a vector of length two. See headerSpace argument in ?create_timeline. Resorting to default c(1, .5)."
      )
    }
    
    altTextInfo <- snapshotList[[2]]
    items <- seq(1, length(snapshotList[[1]]), 1)
    
    # Get four colours ready
    if (is.null(constant) &
        is.null(changed) & is.null(added) & is.null(deleted)) {
      if (length(colScheme) > 1) {
        chosenScheme <- return_scheme(colScheme = colScheme[1])
      }
      else {
        chosenScheme <- return_scheme(colScheme = colScheme)
      }
    } else {
      chosenScheme <- NULL
      colScheme <- NULL
    }
    
    if (!is.null(colScheme) & length(colScheme) > 1) {
      constantPlace <- match("constant", colScheme) - 1
      constant <- unlist(chosenScheme[constantPlace])[1]
      names(constant) <- "constant1"
      constantAlpha <-
        as.numeric(unlist(chosenScheme[constantPlace])[[2]])
      
      changedPlace <- match("changed", colScheme) - 1
      changed <- unlist(chosenScheme[changedPlace])[1]
      names(changed) <- "changed1"
      changedAlpha <-
        as.numeric(unlist(chosenScheme[changedPlace])[[2]])
      
      addedPlace <- match("added", colScheme) - 1
      added <- unlist(chosenScheme[addedPlace])[1]
      names(added) <- "added1"
      addedAlpha <-
        as.numeric(unlist(chosenScheme[addedPlace])[[2]])
      
      deletedPlace <- match("deleted", colScheme) - 1
      deleted <- unlist(chosenScheme[deletedPlace])[[1]]
      names(deleted) <- "deleted1"
      deletedAlpha <-
        as.numeric(unlist(chosenScheme[deletedPlace])[[2]])
      
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
          "Data has not changed.\nTint is missing data.",
          "Data has been edited.\nTint is missing data.",
          "Data has been added.\nTint is missing data.",
          "Data will be deleted.\nTint is missing data."
        )
    } else {
      descriptions <-
        c(
          "Data has not changed.",
          "Data has been edited.",
          "Data has been added.",
          "Data will be deleted."
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
    
    tileAlphas <-
      subset(tileAlphas, tileAlphas$colValue %in% colsPresent)
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
        rotateHeader,
        headerSpace,
        accentCols,
        accentColsDif,
        otherTextCol,
        maxDims,
        timelineFont,
        captionSpace,
        accents,
        legendDF,
        highlightNA,
        timelineRows,
        FUN = make_timeline_plot
      )
    
    # Set a limits for x-axis if necessary
    if (timelineRows > 1) {
      m <- maxDims[[1]] + headerSpace[2]
      if (!is.null(snapshotList[[4]])) {
        m <- maxDims[[1]] + 2.51
      }
      for (p in 1:length(l)) {
        l[[p]] <- l[[p]] + xlim(c(0, m))
      }
    }
    
    if ((timelineRows == 1) & (headerSpace[2] != .5)) {
      m <- maxDims[[1]] + headerSpace[2]
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
    
    # Set timeline design choices
    fontChoice <-
      paste0(
        " & theme(text = element_text(family = '",
        timelineFont,
        "', colour = otherTextColour),",
        "legend.key.size = unit(",
        sizing[["legendIcons"]],
        ", 'line'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'))"
      )
    
    patchedPlots <-
      paste0(patchedPlots, fontChoice)
    
    generate_alt_text(snapshotList = snapshotList,
                      altTextInfo = altTextInfo,
                      l = l,
                      abstract = abstract,
                      ghostData = ghostData)

    o <- return(eval(parse(text = patchedPlots)))
    
    oldClass(o) <- c("smallsetTimeline", class(o))
    
    o
    
  }
