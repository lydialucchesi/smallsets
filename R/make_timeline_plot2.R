#' Make a timeline plot
#' @description A function to transform the flextable into a colour plot
#' @keywords internal
#' @export
#' @import "reshape2" "ggplot2" "ggforce" "ggfittext" "colorspace" "stringr" "ggtext"
#' @importFrom gplots col2hex

make_timeline_plot2 <-
  function(itemNum,
           snapshotList,
           abstract,
           sizing,
           truncateData,
           accentCols,
           accentColsDif,
           otherTextCol,
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
           captionTemplateDir) {
    tab1 <-
      as.data.frame(snapshotList[[1]][[itemNum]]$body$styles$text$color$data)
    tab2 <-
      as.data.frame(snapshotList[[1]][[itemNum]]$body$dataset)
    
    # add in empty colours
    row.names(tab1) <- row.names(tab2)
    difRows <- setdiff(row.names(ghostDF1), row.names(tab1))
    if (length(difRows) > 0) {
      newColCheck <- setdiff(colnames(tab1), colnames(ghostDF1))
      if (length(newColCheck > 0)) {
        for (c in 1:length(newColCheck)) {
          ghostDF1[, paste0(newColCheck[c])] <- "#FFFFFF"
        }
        # ghostDF1 <- ghostDF1[names(tab1)]
      }
      
      tab1 <- rbind(tab1, ghostDF1[difRows, colnames(tab1)])
      tab1 <- tab1[match(rownames(ghostDF1), rownames(tab1)), ]
    }
    
    difCols <- setdiff(colnames(ghostDF1), colnames(tab1))
    if (length(difCols) > 0) {
      tab1 <- cbind(tab1, ghostDF1[row.names(tab1), difCols])
      # tab1 <- tab1[order(row.names(tab1)), ]
      tab1 <- tab1[names(ghostDF1)]
      
    }
    
    # add in empty data values
    
    difRows <- setdiff(row.names(ghostDF2), row.names(tab2))
    if (length(difRows) > 0) {
      newColCheck <- setdiff(colnames(tab2), colnames(ghostDF2))
      if (length(newColCheck > 0)) {
        for (c in 1:length(newColCheck)) {
          ghostDF2[, paste0(newColCheck[c])] <- ""
        }
        # ghostDF2 <- ghostDF2[names(tab2)]
      }
      
      tab2 <- rbind(tab2, ghostDF2[difRows, colnames(tab2)])
      tab2 <- tab2[match(rownames(ghostDF2), rownames(tab2)), ]
      
    }
    
    # difRows <- setdiff(row.names(ghostDF2), row.names(tab2))
    # if (length(difRows) > 0) {
    #   tab2 <- rbind(tab2, ghostDF2[difRows, colnames(tab2)])
    #   tab2 <- tab2[order(row.names(tab2)),]
    #
    # }
    
    difCols <- setdiff(colnames(ghostDF2), colnames(tab2))
    if (length(difCols) > 0) {
      tab2 <- cbind(tab2, ghostDF2[row.names(tab2), difCols])
      # tab2 <- tab2[order(row.names(tab2)), ]
      tab2 <- tab2[names(ghostDF2)]
      
    }
    
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
    
    circles <- subset(tabs, colValue != snapshotList[[9]])
    
    if (stampLoc == 1) {
      circles$xCir <- circles$x - .25
      circles$yCir <- circles$y + .25
    } else if (stampLoc == 2) {
      circles$xCir <- circles$x + .25
      circles$yCir <- circles$y + .25
    } else if (stampLoc == 3) {
      circles$xCir <- circles$x - .25
      circles$yCir <- circles$y - .25
    } else if (stampLoc == 4) {
      circles$xCir <- circles$x + .25
      circles$yCir <- circles$y - .25
    } else {
      circles$xCir <- circles$x
      circles$yCir <- circles$y
    }
    
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
    
    circleSymbols <- plotInfo[itemNum, -c(8)]
    circleSymbols <-
      data.frame(
        action = c(
          circleSymbols$changed,
          circleSymbols$added,
          circleSymbols$deleted
        ),
        colValue = c(snapshotList[[6]], snapshotList[[7]], snapshotList[[8]])
      )
    circleSymbols$colValue <- as.character(circleSymbols$colValue)
    circleSymbols <- merge(circleSymbols, accents)
    
    circleSymbols[, c("hex", "colDir", "colDirDif")] <- NULL
    
    circles <- merge(circles, circleSymbols)
    circles$action <- as.character(circles$action)
    circles$test <- circles$action == ""
    circles <- subset(circles, circles$test == FALSE)
    circles$test <- NULL
    
    smallsetCaption <- plotInfo[itemNum, "caption"]
    
    tabs <-
      suppressMessages(left_join(tabs, snapshotList[[9]], by = "colValue"))
    
    if (isTRUE(highlightNA)) {
      missingCols <- c(
        lighten(col2hex(snapshotList[[5]]), .4),
        lighten(col2hex(snapshotList[[6]]), .4),
        lighten(col2hex(snapshotList[[7]]), .4),
        lighten(col2hex(snapshotList[[8]]), .4)
      )
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
    } else {
      legendDF$alpha <- snapshotList[[9]]$alpha
    }
    
    legendDF$colAlp <-
      as.factor(alpha(legendDF$fillVar, legendDF$alpha))
    tabs <- merge(tabs, legendDF[, c("colValue", "colAlp")])
    legendDF <- subset(legendDF, legend == TRUE)
    
    if (nrow(circles) == 0) {
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
          colour = otherTextCol
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
          legend.margin = margin(
            t = 0,
            r = 0,
            b = 0,
            l = 0,
            unit = 'cm'
          ),
          text = element_text(
            family = timelineFont,
            size = sizing[["legend"]],
            colour = otherTextCol
          )
        ) +
        # xlim(c(.5, (maxDims[1] + .5))) +
        scale_colour_identity()
    } else {
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
          colour = otherTextCol
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
          legend.position = 'bottom',
          legend.title.align = 0.5,
          legend.title = element_blank(),
          legend.margin = margin(
            t = 0,
            r = 0,
            b = 0,
            l = 0,
            unit = 'cm'
          ),
          text = element_text(
            family = timelineFont,
            size = sizing[["legend"]],
            colour = otherTextCol
          )
        ) +
        xlim(c(.5, (maxDims[1] + .5))) +
        geom_circle(
          data = circles,
          aes(
            x0 = xCir,
            y0 = yCir,
            r = sizing[["circles"]],
            colour = colValue2,
          ),
          fill = "white",
          size = .3
        ) +
        geom_text(
          data = circles,
          aes(
            x = xCir,
            y = yCir,
            label = action,
            colour = colValue2
          ),
          family = timelineFont,
          size = sizing[["symbols"]]
        ) +
        scale_colour_identity()
    }
    
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
    
    captionInfo <-
      data.frame(
        x = c((maxDims[1] + 1) / 2),
        y = c(-.25),
        smallsetCaption = c(smallsetCaption)
      )
    
    if (is.na(captionInfo$smallsetCaption)) {
      captionInfo$smallsetCaption <- as.character(captionInfo$smallsetCaption)
      captionInfo$smallsetCaption[1] <- ""
    }
    
    abstractWithCaption <- abstractSmallset +
      geom_textbox(
        data = captionInfo,
        aes(
          x = (maxDims[1] + 1) / 2,
          y = -.25,
          label = smallsetCaption
        ),
        width = grid::unit(.95, "npc"),
        family = timelineFont,
        vjust = c(1),
        hjust = c(.5),
        valign = c(.5),
        halign = c(.5),
        size = sizing[["captions"]],
        box.colour = NA,
        colour = otherTextCol
      ) +
      ylim(c(captionSpace * (-1), maxDims[2] + 1))
    
    if (itemNum %in% snapshotList[[4]]) {
      abstractWithCaption <- abstractWithCaption +
        geom_point(
          aes(x = (maxDims[1] + .5),
              y = ((maxDims[2] + 1) - (
                captionSpace * (-1)
              )) / 2,),
          fill = as.character(snapshotList[[9]]$colValue[1]),
          colour = as.character(snapshotList[[9]]$colValue[1]),
          alpha = snapshotList[[9]]$alpha[1],
          size = 2
        )
    }
    
    abstractWithCaption
    return(abstractWithCaption)
    
  }
