#' Create the Timeline
#' @description  Creates the Timeline.
#' @keywords internal
#' @import "patchwork" "colorspace"

create_timeline <-
  function(snapshotList,
           colours,
           printedData,
           ghostData,
           missingDataTints,
           sizing,
           truncateData,
           rotateHeader,
           headerSpace,
           accentCol,
           accentColDif,
           otherTextCol,
           timelineRows,
           timelineFont,
           captionSpace) {
    items <- seq(1, length(snapshotList[[1]]), 1)
    
    if (length(headerSpace) != 2) {
      headerSpace <- c(1, .5)
      print(
        "headerSpace must be a vector of length two. See headerSpace argument in ?Smallset_Timeline. Resorting to default c(1, .5)."
      )
    }
    
    sizing <- set_sizes(sizing = sizing)
    
    # Get four colours ready
    colClass <- class(colours)
    
    if (colClass == "character") {
      chosenScheme <- return_scheme(colScheme = colours)
      same <- chosenScheme$same
      edit <- chosenScheme$edit
      add <- chosenScheme$add
      delete <- chosenScheme$delete
    } else {
      same <- colours$same
      edit <- colours$edit
      add <- colours$add
      delete <- colours$delete
    }
    
    tileColours <- data.frame(colValue = c(same, edit, add, delete))
    
    for (i in 1:length(snapshotList[[1]])) {
      temp <- snapshotList[[1]][[i]]$body$styles$text$color$data
      for (c in colnames(temp)) {
        temp[, c] <- replace(temp[, c], temp[, c] == "#808080", same)
        temp[, c] <- replace(temp[, c], temp[, c] == "#008000", edit)
        temp[, c] <- replace(temp[, c], temp[, c] == "#0000FF", add)
        temp[, c] <-
          replace(temp[, c], temp[, c] == "#FF0000", delete)
      }
      snapshotList[[1]][[i]]$body$styles$text$color$data <- temp
    }
    
    # Prepare timeline accent colours
    accents <-
      data.frame(
        colValue = c(same, edit, add, delete),
        accent = accentCol,
        degree = accentColDif
      )
    accents$colValue2 <-
      ifelse(
        accents$accent == "darker",
        darken(accents$colValue, accents$degree),
        lighten(accents$colValue, accents$degree)
      )
    
    # Identify which colours are present in the timeline
    colsPresent <- c()
    for (u in 1:length(snapshotList[[1]])) {
      uniqueCols <- snapshotList[[1]][[u]]$body$styles$text$color$data
      uniqueCols <- as.vector(as.matrix(uniqueCols))
      uniqueCols <- unique(uniqueCols)
      colsPresent <- c(colsPresent, uniqueCols)
    }
    
    colsPresent <- unique(colsPresent)
    tileColours <-
      subset(tileColours, tileColours$colValue %in% colsPresent)
    snapshotList[[5]] <- tileColours
    
    # Prepare colour legend
    if (isTRUE(missingDataTints)) {
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
    colItems <- c(same, edit, add, delete)
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
        as.data.frame(snapshotList[[1]][[1]]$body$styles$text$color$data)
      ghostDF1[] <- lapply(ghostDF1, as.character)
      
      for (i in 1:nrow(ghostDF1)) {
        for (j in 1:length(colnames(ghostDF1))) {
          ghostDF1[i, j] <- "#FFFFFF"
        }
      }
      
      ghostDF2 <- as.data.frame(snapshotList[[1]][[1]]$body$dataset)
      ghostDF2[] <- lapply(ghostDF2, as.character)
      
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
    
    # Make the timeline plot for each snapshot
    l <-
      lapply(
        items,
        extTables,
        snapshotList,
        printedData,
        ghostData,
        sizing,
        truncateData,
        rotateHeader,
        headerSpace,
        accentCol,
        accentColDif,
        otherTextCol,
        maxDims,
        timelineFont,
        captionSpace,
        accents,
        legendDF,
        missingDataTints,
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
    
    altTextInfo <- snapshotList[[2]]
    generate_alt_text(
      snapshotList = snapshotList,
      legendDF = legendDF,
      altTextInfo = altTextInfo,
      l = l,
      printedData = printedData,
      ghostData = ghostData
    )
    
    o <- return(eval(parse(text = patchedPlots)))
    
    oldClass(o) <- c("smallsetTimeline", class(o))
    
    o
    
  }
