#' Make a timeline plot
#' @description A function to transform the flextable into a colour plot
#' @keywords internal
#' @export
#' @import "reshape2" "ggplot2" "ggforce" "ggfittext" "gplots" "colorspace" "stringr" "ggtext"

make_timeline_plot_w_legend <-
  function(ftsItemNum,
           ftsList,
           abstract,
           sizing,
           accentCols,
           accentColsDif,
           stampLoc,
           maxDims,
           timelineFont,
           captionSpace,
           accents,
           legendDF) {
    tab1 <-
      as.data.frame(ftsList[[1]][[ftsItemNum]]$body$styles$text$color$data)
    tab2 <-
      as.data.frame(ftsList[[1]][[ftsItemNum]]$body$dataset)
    
    xs <-
      data.frame(variable = colnames(tab1), x = seq(1, length(colnames(tab1)), 1))
    
    for (i in 1:ncol(tab1)) {
      tab1[, i] <- as.character(tab1[, i])
    }
    tab1$y <- seq(nrow(tab1), 1, -1)
    tab1Long <- reshape2::melt(tab1, id = c("y"))
    tab1Long <- merge(tab1Long, xs)
    colnames(tab1Long) <- c("variable", "y", "colValue", "x")
    
    tab2$y <- seq(nrow(tab2), 1, -1)
    tab2Long <- reshape2::melt(tab2, id = c("y"))
    tab2Long <- merge(tab2Long, xs)
    colnames(tab2Long) <- c("variable", "y", "datValue", "x")
    
    tabs <- merge(tab1Long, tab2Long)
    tabs <- merge(tabs, accents)
    
    xs$y <- rep(max(tabs$y) + 1, nrow(xs))
    xs$variable <- str_to_title(xs$variable)
    
    circles <- subset(tabs, colValue != ftsList[[2]])
    
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
    
    plotInfo <- read_captions_rmd(ftsList[[6]], ftsList[[7]])
    circleSymbols <- plotInfo[ftsItemNum, -c(8)]
    circleSymbols <-
      data.frame(
        action = c(
          circleSymbols$changed,
          circleSymbols$added,
          circleSymbols$deleted
        ),
        colValue = c(circleSymbols$col1, circleSymbols$col2, circleSymbols$col3)
      )
    circleSymbols$colValue <- as.character(circleSymbols$colValue)
    circleSymbols <- merge(circleSymbols, accents)
    
    circleSymbols[, c("hex", "colDir", "colDirDif")] <- NULL
    
    circles <- merge(circles, circleSymbols)
    circles$action <- as.character(circles$action)
    circles$test <- circles$action == ""
    circles <- subset(circles, circles$test == FALSE)
    circles$test <- NULL
    
    smallsetCaption <- plotInfo[ftsItemNum, "caption"]
    
    if (max(tabs$x) != maxDims[1]) {
      empty1 <-
        data.frame(expand.grid(x = seq(max(tabs$x) + 1, maxDims[1]),
                               y = seq(1, maxDims[2])))
    }
    
    if (max(tabs$y) != maxDims[2]) {
      d <- maxDims[2] - max(tabs$y)
      tabs$y <- tabs$y + d
      xs$y <- xs$y + d
      circles$yCir <- circles$yCir + d
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
    
    missingCol <- lighten(col2hex(ftsList[[2]]), .4)
    tabs$colValue <-
      ifelse(is.na(tabs$datValue), lighten(col2hex(tabs$colValue), .4), tabs$colValue)
    
    tileColGuide <-
      data.frame(
        breaks = accents$colValue,
        labels = c("constant", "changed", "added", "deleted")
      )
    
    legendDF$legend <- TRUE
    addNewRow <- data.frame(colValue = c(missingCol), description = c(""), legend = c(FALSE))
    legendDF <- rbind(legendDF, addNewRow)
    legendDF$fillVar <-
      factor(legendDF$colValue, levels = legendDF$colValue)
    tabs <- merge(tabs, legendDF[,c("colValue", "fillVar")])
    
    legendDF <- subset(legendDF, legend == TRUE)
    
    if (nrow(circles) == 0) {
      abstractSmallset <- ggplot() +
        geom_tile(
          data = tabs,
          aes(x = x, y = y, fill = fillVar),
          alpha = .4,
          colour = "white",
          size = sizing[["tiles"]]
        ) +
        scale_fill_identity(
          "",
          labels = legendDF$description,
          breaks = legendDF$fillVar,
          guide = "legend",
          drop = FALSE
        ) +
        geom_text(
          data = xs,
          aes(x = x, y = y, label = variable),
          family = timelineFont,
          size = sizing[["columns"]]
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
          legend.title=element_blank(), 
          legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'),
          text = element_text(family = timelineFont,
                              size = sizing[["legend"]])
        ) +
        xlim(c(.5, (maxDims[1] + .5))) +
        scale_colour_identity()
    } else {
      abstractSmallset <- ggplot() +
        geom_tile(
          data = tabs,
          aes(x = x, y = y, fill = fillVar),
          alpha = .4,
          colour = "white",
          size = sizing[["tiles"]]
        ) +
        scale_fill_identity(
          "",
          labels = legendDF$description,
          breaks = legendDF$fillVar,
          guide = "legend",
          drop = FALSE
        ) +
        geom_text(
          data = xs,
          aes(x = x, y = y, label = variable),
          family = timelineFont,
          size = sizing[["columns"]]
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
          legend.title=element_blank(), 
          legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'),
          text = element_text(family = timelineFont,
                              size = sizing[["legend"]])
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
    
    if (nrow(empty) > 0) {
      abstractSmallset <- abstractSmallset +
        geom_tile(
          data = empty,
          aes(x = x, y = y),
          fill = NA,
          colour = NA,
          size = sizing[["tiles"]]
        )
    } else {
      abstractSmallset <- abstractSmallset
    }
    
    captionInfo <-
      data.frame(
        x = c((maxDims[1] + 1) / 2),
        y = c(-.25),
        smallsetCaption = c(smallsetCaption)
      )
    
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
        box.colour = NA
      ) +
      ylim(c(captionSpace * (-1), maxDims[2] + 1))
    
    return(abstractWithCaption)
    
  }
