tab <-
  as.data.frame(ftsList[[ftsItemNum]]$body$styles$text$color$data)
xs <-
  data.frame(variable = colnames(tab), x = seq(1, length(colnames(tab)), 1))
tab$y <- seq(nrow(tab), 1,-1)
tabLong <- melt(tab, id = c("y"))
tabLong <- merge(tabLong, xs)

xs$y <- rep(max(tabLong$y) + 1, nrow(xs))
xs$variable <- toupper(xs$variable)

circles <- subset(tabLong, value != "#927C5C")
circles$xCir <- circles$x + .25
circles$yCir <- circles$y - .25

# it doesn't work to add an empty data frame to a ggplot
# so I will need to do an if else setup instead
# checking if the cirlces data frame is empty or not
circles <- data.frame(xCir = numeric(), yCir = numeric())
  
  
  
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
  ylim(c(min(tabLong$y) - .5, max(tabLong$y) + 1.5)) + 
  geom_circle(data = circles, aes(x0 = xCir, y0 = yCir, r = .15), size = .1, fill = "green")
  
abstractSmallset
