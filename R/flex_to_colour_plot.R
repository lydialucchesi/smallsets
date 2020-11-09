#' Flex to colour plot
#' @description A function to transform the flextable into a colour plot
#' @keywords internal
#' @export
#' @import "reshape2" "ggplot2" "ggforce" "ggfittext" "gplots" "colorspace" "stringr"

flex_to_colour_plot <-
  function(ftsItemNum,
           ftsList,
           sizing,
           stampCols,
           stampColsDif,
           stampLoc,
           maxDims) {
    tab <-
      as.data.frame(ftsList[[1]][[ftsItemNum]]$body$styles$text$color$data)
    
    xs <-
      data.frame(variable = colnames(tab), x = seq(1, length(colnames(tab)), 1))
    
    for (i in 1:ncol(tab)) {
      tab[, i] <- as.character(tab[, i])
    }
    tab$y <- seq(nrow(tab), 1, -1)
    tabLong <- reshape2::melt(tab, id = c("y"))
    tabLong <- merge(tabLong, xs)
    
    xs$y <- rep(max(tabLong$y) + 1, nrow(xs))
    xs$variable <- str_to_title(xs$variable)
    
    circles <- subset(tabLong, value != ftsList[[2]])
    
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
        value = c(circleSymbols$col1, circleSymbols$col2, circleSymbols$col3)
      )
    circleSymbols$value <- as.character(circleSymbols$value)
    circleSymbols$hex <-
      ifelse(
        grepl("#", circleSymbols$value) == TRUE,
        circleSymbols$value,
        col2hex(circleSymbols$value)
      )
    circleSymbols$colDir <- unlist(stampCols)
    circleSymbols$colDirDif <- unlist(stampColsDif)
    
    circleSymbols$value2 <- ifelse(
      circleSymbols$colDir == "darker",
      darken(circleSymbols$hex, circleSymbols$colDirDif),
      lighten(circleSymbols$hex, circleSymbols$colDirDif)
    )
    
    circleSymbols[, c("hex", "colDir", "colDirDif")] <- NULL
    
    circles <- merge(circles, circleSymbols)
    circles$action <- as.character(circles$action)
    circles$test <- circles$action == ""
    circles <- subset(circles, circles$test == FALSE)
    circles$test <- NULL
    
    smallsetCaption <- plotInfo[ftsItemNum, "caption"]
    
    if (max(tabLong$x) != maxDims[1]) {
      empty1 <-
        data.frame(expand.grid(x = seq(max(tabLong$x) + 1, maxDims[1]),
                               y = seq(1, maxDims[2])))
    }
    
    if (max(tabLong$y) != maxDims[2]) {
      d <- maxDims[2] - max(tabLong$y)
      tabLong$y <- tabLong$y + d
      xs$y <- xs$y + d
      circles$yCir <- circles$yCir + d
      empty2 <- data.frame(expand.grid(x = seq(1, maxDims[1]),
                                       y = seq(1, min(tabLong$y) - 1)))
    }
    
    empty <- data.frame()
    if (exists("empty1")) {
      empty <- rbind(empty, empty1)
    }
    
    if (exists("empty2")) {
      empty <- rbind(empty, empty2)
    }
    
    if (nrow(circles) == 0) {
      abstractSmallset <- ggplot() +
        geom_tile(
          data = tabLong,
          aes(x = x, y = y, fill = value),
          alpha = .4,
          colour = "white",
          size = sizing[["tiles"]]
        ) +
        scale_fill_identity() +
        geom_text(data = xs,
                  aes(x = x, y = y, label = variable),
                  size = sizing[["columns"]]) +
        coord_equal() +
        theme_void() +
        xlim(c(.5, (maxDims[1] + .5)))
    } else {
      abstractSmallset <- ggplot() +
        geom_tile(
          data = tabLong,
          aes(x = x, y = y, fill = value),
          alpha = .4,
          colour = "white",
          size = sizing[["tiles"]]
        ) +
        scale_fill_identity() +
        geom_text(data = xs,
                  aes(x = x, y = y, label = variable),
                  size = sizing[["columns"]]) +
        coord_equal() +
        theme_void() +
        xlim(c(.5, (maxDims[1] + .5))) +
        geom_circle(
          data = circles,
          aes(
            x0 = xCir,
            y0 = yCir,
            r = sizing[["circles"]],
            colour = value2,
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
            colour = value2
          ),
          size = sizing[["symbols"]]
        ) +
        scale_colour_identity()
    }
    
    if (nrow(empty) > 0) {
      abstractSmallset <- abstractSmallset +
        geom_tile(data = empty,
                  aes(x = x, y = y),
                  fill = NA,
                  colour = NA)
      } else {
      abstractSmallset <- abstractSmallset
    }
    
    captionInfo <-
      data.frame(
        x = c((maxDims[1] + 1) / 2),
        y = c(-.25),
        smallsetCaption = c(smallsetCaption)
      )
    
    if (nrow(captionInfo) > 0) {
      abstractWithCaption <- abstractSmallset +
        geom_fit_text(
          data = captionInfo,
          aes(xmin = .5, xmax = maxDims[1] + .5, ymin = -1, ymax = 0, label = smallsetCaption),
          size = sizing[["captions"]],
          place = 'centre',
          reflow = TRUE
        )
    } else {
      abstractWithCaption <- abstractSmallset
    }
    
    return(abstractWithCaption)
    
  }

