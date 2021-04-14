
create_timeline <- function(ftsList, abstract){
  
}
  
ftsList <- check

p1 <- ggplot_build(check[[1]])$data[[1]]
p1$id <- 1
p2 <- ggplot_build(check[[2]])$data[[1]]
p2$id <- 2
p3 <- ggplot_build(check[[3]])$data[[1]]
p3$id <- 3
p4 <- ggplot_build(check[[4]])$data[[1]]
p4$id <- 4
p5 <- ggplot_build(check[[5]])$data[[1]]
p5$id <- 5

p <- rbind(p1, p2, p3, p4, p5)

ggplot() +
  geom_tile(
    data = p,
    aes(x = x, y = y, fill = fill, colour = colour), size = 2
  ) +
  scale_fill_identity() + 
  scale_colour_identity() + 
  coord_equal() + 
  facet_wrap(~id, nrow = 1)


library(patchwork)

g1 <- ggplot() +
  geom_tile(
    data = p1,
    aes(x = x, y = y, fill = fill, colour = colour), size = 2
  ) +
  scale_fill_identity() + 
  scale_colour_identity() + 
  coord_equal() + theme_void()

g2 <- ggplot() +
  geom_tile(
    data = p2,
    aes(x = x, y = y, fill = fill, colour = colour), size = 2
  ) +
  scale_fill_identity() + 
  scale_colour_identity() + 
  coord_equal() + theme_void()

g3 <- ggplot() +
  geom_tile(
    data = p3,
    aes(x = x, y = y, fill = fill, colour = colour), size = 2
  ) +
  scale_fill_identity() + 
  scale_colour_identity() + 
  coord_equal() + theme_void()

g4 <- ggplot() +
  geom_tile(
    data = p4,
    aes(x = x, y = y, fill = fill, colour = colour), size = 2
  ) +
  scale_fill_identity() + 
  scale_colour_identity() + 
  coord_equal() + theme_void()

empty <- data.frame(x = seq(1, 4), y = rep(1, 4))
g5 <- ggplot() +
  geom_tile(
    data = p5,
    aes(x = x, y = y + 1, fill = fill, colour = colour), size = 2
  ) +
  scale_fill_identity() + 
  scale_colour_identity() + 
  coord_equal() + 
  geom_tile(data = empty, aes(x = x, y = y), fill = NA, colour = NA, size = 2) + 
  theme_void()
g5

g1 + g2 + g3 + g4 + g5 + plot_layout(nrow = 1)


# https://github.com/tidyverse/ggplot2/issues/187
# look at patchwork package
# add a row of empty tiles?