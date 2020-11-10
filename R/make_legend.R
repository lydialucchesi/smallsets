
make_legend <- function(legendDF, f, s) {
  
  constantText <- "Data have not changed since previous snapshot."
  changedText <- "Data have been changed since previous snapshot."
  addedText <- "Data have been added since previous snapshot."
  deletedText <- "Data will be removed prior to the next snapshot."
  
  legendDF$x <- seq(1, 4)
  legendDF$y <- rep(1, 4)
  legendDF$description <- c(constantText, changedText, addedText, deletedText)
  
  fillLegend <- ggplot() + 
    geom_tile(data = legendDF, aes(x = x, y = y, fill = colValue), colour = "white", size = s, alpha = .4) + 
    scale_fill_identity() + 
    geom_fit_text(data = legendDF, aes(x = x, y = y, fill = colValue, label = description, colour = colValue2), 
                  padding.x = grid::unit(2, "mm"),
                  padding.y = grid::unit(2, "mm"),
                  family = f) +
    scale_colour_identity() +
    coord_fixed(.5) +
    theme_void()
  
  return(fillLegend)
}

