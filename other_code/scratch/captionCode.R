g <- ggplot() +
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
            aes(x = xCir, y = yCir, label = circleSymbol),
            size = 2.5) + 
  theme_void()
g

captionData$y <- -.25
colnames(captionData) <- c('x', 'y', 'capt')
g + 
  geom_tile(data = captionData, aes(x = x, y = y, width = 5), fill = NA, colour = NA) + 
  geom_fit_text(data = captionData, aes(x = x, y = y, label = capt), size = 8, reflow = T, width = 5)

