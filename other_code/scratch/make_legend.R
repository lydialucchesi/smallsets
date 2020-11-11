
make_legend <- function(legendDF, f, s) {
  
  constantText <- "Data have not changed 
  since previous snapshot."
  changedText <- "Data have changed 
  since previous snapshot."
  addedText <- "Data have been added 
  since previous snapshot."
  deletedText <- "Data will be removed 
  prior to the next snapshot."
  
  legendDF$x <- c(0, 6, 12, 18)
  legendDF$y <- c(1, 1, 1, 1)
  legendDF$x2 <- c(2, 8, 14, 20)
  legendDF$description <- c(constantText, changedText, addedText, deletedText)
  
  fillLegend <- ggplot() + 
    geom_tile(data = legendDF, aes(x = x, y = y, fill = colValue), colour = "white", alpha = .4, width = 1, height = 1) + 
    scale_fill_identity() + 
    coord_equal() +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_void()
  fillLegend
  return(fillLegend)
}

