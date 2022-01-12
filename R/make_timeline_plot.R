#' Make timeline plot
#' @description The function transforms a table snapshot into a plot snapshot.
#' @keywords internal
#' @import "reshape2" "ggplot2" "ggforce" "ggfittext" "colorspace" "stringr"
#'   "ggtext"
#' @importFrom gplots col2hex

make_timeline_plot <-
  function(itemNum,
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
           captionTemplateName,
           captionTemplateDir,
           timelineRows) {
    
    # Retrieve colour and data information for a snapshot
    tab1 <- extTables[[itemNum]][[1]]
    tab2 <- extTables[[itemNum]][[2]]
    
    # Set plot coordinates
    xs <-
      data.frame(variable = colnames(tab1), x = seq(1, length(colnames(tab1)), 1))
    
    for (i in 1:ncol(tab1)) {
      tab1[, i] <- as.character(tab1[, i])
    }
    tab1$y <- seq(nrow(tab1), 1, -1)
    tab1Long <- suppressWarnings(reshape2::melt(tab1, id = c("y")))
    tab1Long <- merge(tab1Long, xs)
    colnames(tab1Long) <- c("variable", "y", "colValue", "x")
    
    tab2$y <- seq(nrow(tab2), 1, -1)
    tab2Long <- suppressWarnings(reshape2::melt(tab2, id = c("y")))
    tab2Long <- merge(tab2Long, xs)
    colnames(tab2Long) <- c("variable", "y", "datValue", "x")
    
    tabs <- merge(tab1Long, tab2Long)
    tabs <- suppressMessages(left_join(tabs, accents))
    
    xs$y <- rep(max(tabs$y) + 1, nrow(xs))
    xs$variable <- str_to_title(xs$variable)
    
    tabs <-
      suppressMessages(left_join(tabs, snapshotList[[9]], by = "colValue"))
    
    # Prepare lighter colour values for tiles with missing data
    # if (isTRUE(highlightNA)) {
    #   missingCols <- c(
    #     lighten(col2hex(snapshotList[[5]]), .4),
    #     lighten(col2hex(snapshotList[[6]]), .4),
    #     lighten(col2hex(snapshotList[[7]]), .4),
    #     lighten(col2hex(snapshotList[[8]]), .4)
    #   )
    #   tabs$colValue <-
    #     ifelse(is.na(tabs$datValue),
    #            lighten(col2hex(tabs$colValue), .4),
    #            tabs$colValue)
    # }
    
    if (isTRUE(highlightNA)) {
      missingCols <- c()
      if (snapshotList[[5]] %in% legendDF$colValue) {
        missingCols <- c(missingCols, lighten(col2hex(snapshotList[[5]]), .4))
      }
      
      if (snapshotList[[6]] %in% legendDF$colValue) {
        missingCols <- c(missingCols, lighten(col2hex(snapshotList[[6]]), .4))
      }
      
      if (snapshotList[[7]] %in% legendDF$colValue) {
        missingCols <- c(missingCols, lighten(col2hex(snapshotList[[7]]), .4))
      }
      
      if (snapshotList[[8]] %in% legendDF$colValue) {
        missingCols <- c(missingCols, lighten(col2hex(snapshotList[[8]]), .4))
      }
      
      tabs$colValue <-
        ifelse(is.na(tabs$datValue),
               lighten(col2hex(tabs$colValue), .4),
               tabs$colValue)
    }
    
    tileColGuide <-
      data.frame(
        breaks = accents$colValue,
        labels = c("constant", "changed", "added", "deleted")
      )
    
    legendDF$legend <- TRUE
    
    if (isTRUE(highlightNA)) {
      addNewRows <-
        data.frame(
          colValue = missingCols,
          description = c(""),
          legend = c(FALSE)
        )
      legendDF <- rbind(legendDF, addNewRows)
    }
    
    legendDF$fillVar <-
      factor(legendDF$colValue, levels = legendDF$colValue)
    
    if (isTRUE(highlightNA)) {
      legendDF$alpha <-
        c(snapshotList[[9]]$alpha[1:nrow(subset(legendDF, legendDF == TRUE))], snapshotList[[9]]$alpha)
      # legendDF$alpha <-
      #   c(snapshotList[[9]]$alpha[1:nrow(subset(legendDF, legendDF == TRUE))])
      } else {
      legendDF$alpha <- snapshotList[[9]]$alpha
    }
    
    legendDF$colAlp <-
      as.factor(alpha(legendDF$fillVar, legendDF$alpha))
    tabs <- suppressMessages(left_join(tabs, legendDF[, c("colValue", "colAlp")]))
    legendDF <- subset(legendDF, legend == TRUE)
    
    if (isFALSE(ghostData)) {
      if (max(tabs$x) != maxDims[1]) {
        empty1 <-
          data.frame(expand.grid(
            x = seq(max(tabs$x) + 1, maxDims[1]),
            y = seq(1, maxDims[2])
          ))
      }
      
      if (max(tabs$y) != maxDims[2]) {
        d <- maxDims[2] - max(tabs$y)
        tabs$y <- tabs$y + d
        xs$y <- xs$y + d
        empty2 <- data.frame(expand.grid(x = seq(1, maxDims[1]),
                                         y = seq(1, min(tabs$y) - 1)))
      }
      
      empty <- data.frame()
      if (exists("empty1")) {
        empty <- rbind(empty, empty1)
      }
      if (exists("empty2")) {
        empty <- rbind(empty, empty2)
      }
    }
    
    if (isTRUE(rotateHeader)) {
      angleVal <- 45
      hjustVal <- 0
      vjustVal <- 1
    } else {
      angleVal <- 0
      hjustVal <- .5
      vjustVal <- .5
    }
    
    # Create plot
    abstractSmallset <- ggplot() +
      geom_tile(
        data = tabs,
        aes(x = x, y = y, fill = colAlp),
        colour = "white",
        size = sizing[["tiles"]]
      ) +
      scale_fill_identity(
        "",
        labels = legendDF$description,
        breaks = legendDF$colAlp,
        guide = "legend",
        drop = FALSE
      ) +
      geom_text(
        data = xs,
        aes(x = x, y = y, label = variable),
        family = timelineFont,
        size = sizing[["columns"]],
        colour = otherTextCol,
        angle = angleVal,
        hjust = hjustVal,
        vjust = vjustVal
      ) +
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
          size = sizing[["legendText"]],
          colour = otherTextCol
        )
      ) +
      scale_colour_identity()

    # Print data in tables
    tabs$datValue <- ifelse(is.na(tabs$datValue), "", tabs$datValue)
    
    if (isFALSE(abstract) & !isFALSE(truncateData)) {
      tabs$datValue <- str_trunc(tabs$datValue, truncateData, "right")
    }
    
    if (abstract != TRUE) {
      abstractSmallset <- abstractSmallset +
        geom_text(
          data = tabs,
          aes(
            x = x,
            y = y,
            label = datValue,
            colour = colValue2
          ),
          family = timelineFont,
          size = sizing[["data"]]
        )
    }
    
    # Add invisible tiles to keep visible tiles equal in size (if ghostData = F)
    if (isFALSE(ghostData)) {
      if (nrow(empty) > 0) {
        abstractSmallset <- abstractSmallset +
          geom_tile(
            data = empty,
            aes(x = x, y = y),
            fill = NA,
            colour = NA,
            size = sizing[["tiles"]]
          )
      }
    }
    
    # Add captions to the plot
    if (is.null(captionTemplateName) &
        is.null(captionTemplateDir)) {
      plotInfo <-
        read_captions_rmd(snapshotList[[2]], snapshotList[[3]])
    } else if (is.null(captionTemplateName) &
               !is.null(captionTemplateDir)) {
      plotInfo <-
        read_captions_rmd(snapshotList[[2]], captionTemplateDir)
    } else if (!is.null(captionTemplateName) &
               is.null(captionTemplateDir)) {
      plotInfo <-
        read_captions_rmd(captionTemplateName, snapshotList[[3]])
    } else {
      plotInfo <-
        read_captions_rmd(captionTemplateName, captionTemplateDir)
    }
    smallsetCaption <- plotInfo[itemNum, "caption"]
    
    if ((timelineRows > 1) | (isFALSE(ghostData))) {
      captionInfo <-
        data.frame(
          x = c((maxDims[1] + headerSpace[2]) / 2),
          y = c(-.25),
          smallsetCaption = c(smallsetCaption)
        )
    } else if ((timelineRows == 1) & (headerSpace[2] != .5)) {
      captionInfo <-
        data.frame(
          x = c((ncol(tab2) + headerSpace[2]) / 2),
          y = c(-.25),
          smallsetCaption = c(smallsetCaption)
        )
    } else {
      captionInfo <-
        data.frame(
          x = c((ncol(tab2)) / 2),
          y = c(-.25),
          smallsetCaption = c(smallsetCaption)
        )
    }
    
    if (itemNum %in% snapshotList[[4]]) {
      captionInfo$x <- captionInfo$x + 1.25
    }

    if (is.na(captionInfo$smallsetCaption)) {
      captionInfo$smallsetCaption <-
        as.character(captionInfo$smallsetCaption)
      captionInfo$smallsetCaption[1] <- ""
    }
    
    abstractWithCaption <- abstractSmallset +
      geom_textbox(
        data = captionInfo,
        aes(
          x = x,
          y = y,
          label = smallsetCaption
        ),
        width = grid::unit(.95, "npc"),
        family = timelineFont,
        vjust = c(1),
        hjust = c(.5),
        valign = c(.5),
        halign = c(0),
        size = sizing[["captions"]],
        box.colour = NA,
        colour = otherTextCol
      ) +
      ylim(c(captionSpace * (-1), maxDims[2] + headerSpace[1]))
    
    # Add resume markers (a vertical line between two snapshots)
    if (itemNum %in% snapshotList[[4]]) {
      abstractWithCaption <- abstractWithCaption +
        geom_segment(
          aes(
            x = (maxDims[1] + 2),
            xend = (maxDims[1] + 2),
            y = .5,
            yend = maxDims[2] + .5
          ),
          colour = as.character(snapshotList[[9]]$colValue[1]),
          alpha = snapshotList[[9]]$alpha[1],
          size = sizing[["resume"]]
        )
    }
    
    return(abstractWithCaption)
    
  }
