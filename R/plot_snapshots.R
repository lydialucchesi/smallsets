#' Plot the snapshots
#' @description Transforms a table snapshot into a plot snapshot.
#' @keywords internal
#' @import "colorspace" "ggfittext" "ggforce" "ggplot2" "ggtext"

plot_snapshots <-
  function(itemNum,
           extTables,
           smallsetTables,
           output,
           fourCols,
           printedData,
           ghostData,
           sizing,
           spacing,
           truncateData,
           maxDims,
           timelineFont,
           accents,
           legendDF,
           missingDataTints) {
    # Retrieve colour and data information for a snapshot
    tab1 <- extTables[[itemNum]][[1]]
    tab1[] <- lapply(tab1, as.character)
    tab2 <- extTables[[itemNum]][[2]]
    
    # Set plot coordinates
    xs <-
      data.frame(ind = colnames(tab1), x = seq(1, length(colnames(tab1)), 1))
    
    # Assign coordinates to tile colours
    tab1$y <- seq(nrow(tab1), 1,-1)
    tab1Long <-
      suppressWarnings(cbind(tab1[ncol(tab1)], utils::stack(tab1[-ncol(tab1)])))
    tab1Long <- merge(tab1Long, xs)
    colnames(tab1Long) <- c("variable", "y", "colValue", "x")
    
    # Assign coordinates to tile data
    tab2$y <- seq(nrow(tab2), 1,-1)
    tab2Long <-
      suppressWarnings(cbind(tab2[ncol(tab2)], utils::stack(tab2[-ncol(tab2)])))
    tab2Long <- merge(tab2Long, xs)
    colnames(tab2Long) <- c("variable", "y", "datValue", "x")
    
    tabs <- merge(tab1Long, tab2Long)
    tabs <- suppressMessages(merge(tabs, accents, all.x = TRUE))
    
    xs$y <- rep(max(tabs$y) + 1, nrow(xs))
    
    # Create empty (invisible) tiles and adjust y coordinates
    if (isFALSE(ghostData)) {
      
      empty <- data.frame(x = as.numeric(), y = as.numeric())
      
      if (max(tabs$x) != maxDims[1]) {
        empty1 <-
          data.frame(expand.grid(x = seq(max(tabs$x) + 1, maxDims[1]),
                                 y = seq(1, maxDims[2])))
      }
      
      if (max(tabs$y) != maxDims[2]) {
        d <- maxDims[2] - max(tabs$y)
        tabs$y <- tabs$y + d
        xs$y <- xs$y + d
        empty2 <- data.frame(expand.grid(x = seq(1, maxDims[1]),
                                         y = seq(1, min(tabs$y) - 1)))
      }
      
      if (exists("empty1")) {
        empty <- rbind(empty, empty1)
      }
      if (exists("empty2")) {
        empty <- rbind(empty, empty2)
      }
    }
    
    # Prepare lighter colour values for tiles with missing data
    if (isTRUE(missingDataTints)) {
      tabs$colValue <-
        ifelse(is.na(tabs$datValue),
               lighten(tabs$colValue, .4),
               tabs$colValue)
      
      missingCols <- lighten(fourCols, .4)
      legendDF <-
        rbind(legendDF,
              data.frame(colValue = missingCols, description = ""))
      tabs$colValue <-
        factor(tabs$colValue, levels = legendDF$colValue)
      legendDF <- subset(legendDF, description != "")
    } else {
      tabs$colValue <- factor(tabs$colValue, levels = legendDF$colValue)
    }
    
    # Rotate column names
    if (spacing$columnsDeg != 0) {
      angleVal <- spacing$columnsDeg
      hjustVal <- 0
      vjustVal <- 1
    } else {
      angleVal <- 0
      hjustVal <- .5
      vjustVal <- .5
    }
    
    # Assign column name colours to match column addition and deletion colours
    colNameCols <-
      data.frame(ind = unique(tabs$variable), col = accents$colValue[1])
    for (v in as.character(unique(tabs$variable))) {
      uniCols <-
        as.character(unique(subset(tabs, variable == v)$colValue))
      uniCols <-
        uniCols[!is.na(uniCols) & (uniCols %in% unique(legendDF$colValue))]
      if (length(uniCols) > 0) {
        if (sum(length(uniCols) == 1 &
                uniCols == accents$colValue[3]) == 1) {
          colNameCols[colNameCols$ind == v, c("col")] <- accents$colValue2[3]
        } else if (sum(length(uniCols) == 1 &
                       uniCols == accents$colValue[4]) == 1) {
          colNameCols[colNameCols$ind == v, c("col")] <- accents$colValue2[4]
        } else {
          colNameCols[colNameCols$ind == v, c("col")] <- accents$colValue2[1]
        }
      } else {
        colNameCols[colNameCols$ind == v, c("col")] <- "#FFFFFF"
      }
    }
    xs <- merge(xs, colNameCols)
    
    # Create snapshot plot
    snapshot <- ggplot() +
      geom_tile(
        data = tabs,
        aes(x = x, y = y, fill = colValue),
        colour = "white",
        size = sizing$tiles
      ) +
      scale_fill_identity(
        "",
        labels = legendDF$description,
        breaks = legendDF$colValue,
        guide = "legend",
        drop = FALSE
      ) +
      geom_text(
        data = xs,
        aes(
          x = x,
          y = y,
          label = ind,
          colour = col
        ),
        family = timelineFont,
        size = sizing$columns,
        angle = angleVal,
        hjust = hjustVal,
        vjust = vjustVal
      ) +
      scale_colour_identity() + 
      coord_equal() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.title.align = 0.5,
        legend.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0,
          unit = 'cm'
        ),
        text = element_text(
          family = timelineFont,
          size = sizing$legendText,
          colour = "black"
        )
      )
    
    # Print data in Smallset snapshots
    if (isTRUE(printedData)) {
      tabs$datValue <- ifelse(is.na(tabs$datValue), "", tabs$datValue)
      # First truncate data if necessary
      if (!is.null(truncateData)) {
        tabs$datValue <-
          ifelse(nchar(tabs$datValue) > truncateData,
                 paste0(substr(tabs$datValue, 1, truncateData), "..."),
                 tabs$datValue)
      }
      snapshot <- snapshot +
        geom_text(
          data = tabs,
          aes(
            x = x,
            y = y,
            label = datValue,
            colour = colValue2
          ),
          family = timelineFont,
          size = sizing$data
        )
    }
    
    # Add empty tiles to maintain equal tile size across snapshots
    if (isFALSE(ghostData)) {
      snapshot <- snapshot +
        geom_tile(
          data = empty,
          aes(x = x, y = y),
          fill = NA,
          colour = NA,
          size = sizing$tiles
        )
    }
    
    # Retrieve snapshot caption
    snapshotCaption <- output[[1]]$text[itemNum]
    snapshotCaption[is.na(snapshotCaption)] <- ""
    
    # Add caption to the snapshot plot
    snapshot <- snapshot +
      geom_textbox(
        aes(x = .5,
            y = -.25,
            label = snapshotCaption),
        width = grid::unit(.9, "npc"),
        box.padding = unit(0, "cm"),
        family = timelineFont,
        hjust = c(0),
        vjust = c(1),
        size = sizing$captions,
        box.colour = NA,
        colour = "black"
      ) +
      ylim(c(spacing$captionB * (-1), maxDims[2] + spacing$columnsT))
    
    # Add a resume marker (a vertical line between two snapshots)
    if (itemNum %in% output[[2]]) {
      snapshot <- snapshot +
        geom_segment(
          aes(
            x = (maxDims[1] + 2),
            xend = (maxDims[1] + 2),
            y = .5,
            yend = maxDims[2] + .5
          ),
          colour = as.character(fourCols[1]),
          size = sizing$resume
        )
    }
    
    return(snapshot)
    
  }
