library(reshape2)
library(ggplot2)
library(ggforce)
library(ggfittext)

source("read_captions_rmd.R")

flex_2_colourPlot <- function(ftsItemNum, ftsList) {
  tab <-
    as.data.frame(ftsList[[1]][[ftsItemNum]]$body$styles$text$color$data)
  xs <-
    data.frame(variable = colnames(tab), x = seq(1, length(colnames(tab)), 1))
  tab$y <- seq(nrow(tab), 1, -1)
  tabLong <- melt(tab, id = c("y"))
  tabLong <- merge(tabLong, xs)
  
  xs$y <- rep(max(tabLong$y) + 1, nrow(xs))
  xs$variable <- toupper(xs$variable)
  
  circles <- subset(tabLong, value != ftsList[[2]])
  circles$xCir <- circles$x + .25
  circles$yCir <- circles$y - .25
  
  plotInfo <- read_captions_rmd(ftsList[[6]])
  circleSymbols <- plotInfo[ftsItemNum, -c(8)]
  circleSymbols <- data.frame(action = c(circleSymbols$changed, circleSymbols$added, circleSymbols$deleted), 
                              value = c(circleSymbols$col1, circleSymbols$col2, circleSymbols$col3))
  circles <- merge(circles, circleSymbols)
  circles$action <- as.character(circles$action)
  circles$test <- circles$action == ""
  circles <- subset(circles, circles$test == FALSE)
  circles$test <- NULL
  
  smallsetCaption <- plotInfo[ftsItemNum, "caption"]
  
  if (nrow(circles) == 0) {
    abstractSmallset <- ggplot() +
      geom_tile(
        data = tabLong,
        aes(x = x, y = y, fill = value),
        alpha = .4,
        colour = "white",
        size = 1
      ) +
      scale_fill_identity() +
      geom_text(data = xs,
                aes(x = x, y = y, label = variable),
                size = 2) +
      coord_equal() +
      theme_void() +
      xlim(c(min(tabLong$x) - .5, max(tabLong$x) + .5))
  } else {
    abstractSmallset <- ggplot() +
      geom_tile(
        data = tabLong,
        aes(x = x, y = y, fill = value),
        alpha = .4,
        colour = "white",
        size = 1
      ) +
      scale_fill_identity() +
      geom_text(data = xs,
                aes(x = x, y = y, label = variable),
                size = 2) +
      coord_equal() +
      theme_void() +
      xlim(c(min(tabLong$x) - .5, max(tabLong$x) + .5)) +
      geom_circle(
        data = circles,
        aes(
          x0 = xCir,
          y0 = yCir,
          r = .15,
          fill = NA
        ),
        colour = "black",
        size = .1
      ) +
      geom_text(data = circles,
                aes(x = xCir, y = yCir, label = action),
                size = 2.5)
  }
  
  captionInfo <-
    data.frame(
      x = c(((
        min(tabLong$x) - .5
      ) + (
        max(tabLong$x) + .5
      )) / 2),
      y = c(-.25),
      smallsetCaption = c(smallsetCaption)
    )
  
  if (nrow(captionInfo) > 0) {
    smallsetWithCaption <- abstractSmallset +
      geom_tile(data = captionInfo,
                aes(x = x, y = y),
                fill = NA,
                colour = NA) +
      geom_fit_text(
        data = captionInfo,
        aes(x = x, y = y, label = smallsetCaption),
        size = 8,
        place = 'centre',
        reflow = TRUE,
        width = ((max(tabLong$x) + .5) - (min(tabLong$x) - .5))
      )
  } else {
    smallsetWithCaption <- abstractSmallset
  }

  
  return(smallsetWithCaption)
  
}

abstract_it <- function(ftsList) {
  items <- seq(1, length(ftsList[[1]]), 1)
  l <- lapply(items, ftsList, FUN = flex_2_colourPlot)
  
}
