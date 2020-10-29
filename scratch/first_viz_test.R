library(ggplot2)
library(ggpubr)

subdat <- data.frame(expand.grid(x = seq(1, 5), y = seq(1, 6)))
subdat <- subdat[order(subdat$x), ]
rownames(subdat) <- seq(1, nrow(subdat))

subdat$colour <- "ivory3"
subdat[c(8, 30), "colour"] <- "white"

labels <-
  data.frame(
    x = seq(1, 5),
    y = rep(7, 5),
    label = c("C1", "C2", "C3", "C4", "C5")
  )

g <- ggplot() +
  geom_point(
    data = subdat,
    aes(x = x, y = y, colour = colour),
    size = 7,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  geom_text(
    data = labels,
    aes(x = x, y = y, label = label),
    colour = "seashell4",
    size = 5
  ) +
  geom_segment(aes(
    x = .5,
    xend = .7,
    y = 5,
    yend = 5
  ), colour = "ivory3") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "seashell4",
      fill = NA,
      size = 1,
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(caption = "A raw data subset of the NASA metrics data program (MDP) data set.") +
  xlim(c(-.75, 7.5)) +
  coord_equal()
g

subdat[subdat$x == 4, "colour"] <-
  c(
    "darkolivegreen3",
    "darkolivegreen1",
    "darkolivegreen2",
    "darkolivegreen3",
    "darkolivegreen1",
    "darkolivegreen2"
  )
binned <- subdat[subdat$x == 4,]
subdat[subdat$x == 4, "x"] <- subdat[subdat$x == 4, "x"] - .2
binned$x <- binned$x + .2
binned$colour <-
  c(
    "lightsteelblue4",
    "lightsteelblue1",
    "lightsteelblue1",
    "lightsteelblue4",
    "lightsteelblue1",
    "lightsteelblue1"
  )
p1 <- ggplot() +
  geom_point(
    data = subdat,
    aes(x = x, y = y, colour = colour),
    size = 7,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  geom_text(
    data = labels,
    aes(x = x, y = y, label = label),
    colour = "seashell4",
    size = 5
  ) +
  geom_segment(aes(
    x = .5,
    xend = .7,
    y = 5,
    yend = 5
  ), colour = "ivory3") +
  geom_point(
    data = binned,
    aes(x = x, y = y, colour = colour),
    size = 7,
    show.legend = FALSE
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "seashell4",
      fill = NA,
      size = 1
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(caption = "Class variable is binned, turning a continuous
       variable into a binary variable.") +
  xlim(c(-.75, 7.5)) +
  coord_equal()
p1

subdat[subdat$x == 3.8, "x"] <- subdat[subdat$x == 3.8, "x"] + .2
subdat[subdat$x == 4, "colour"] <-
  c(
    "lightsteelblue4",
    "lightsteelblue1",
    "lightsteelblue1",
    "lightsteelblue4",
    "lightsteelblue1",
    "lightsteelblue1"
  )

dropShape <-
  data.frame(
    x = c(6, 6, 8, 8, 6),
    y = c(1, 6, 6, 1, 1),
    group = rep(1, 5)
  )
dropShapeLabel <-
  data.frame(x = c(7.68),
             y = c(1.25),
             label = "- 5")
p2 <- ggplot() +
  geom_point(
    data = subdat,
    aes(x = x, y = y, colour = colour),
    size = 7,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  geom_text(
    data = labels,
    aes(x = x, y = y, label = label),
    colour = "ivory3",
    size = 5
  ) +
  geom_segment(aes(
    x = .5,
    xend = .7,
    y = 5,
    yend = 5
  ), colour = "ivory3") +
  geom_polygon(
    data = dropShape,
    aes(x = x, y = y, group = group),
    fill = "firebrick1",
    alpha = .2,
    colour = "firebrick1"
  ) +
  geom_text(
    data = dropShapeLabel,
    aes(x = x, y = y, label = label),
    colour = "firebrick1",
    size = 5
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "seashell4",
      fill = NA,
      size = 1
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  (
    labs(
      caption = 'Dropped five unneeded columns. "A numeric
      attribute which has a constant/fixed value throughout
      all instances is easily identifiable as it will have
      a variance of zero. Such attributes contain no information
      with which to discern modules apart, and are at best a
      waste of classifier resources... This stage removes data
      that may be genuine, but in the context of machine learning
      it is of no use and is therefore discarded." (from page 551)'
    )
  ) +
  xlim(c(.25, 8.5)) +
  coord_equal()
p2

subdat$alpha <- 1
subdat[subdat$x == 3, "alpha"] <- .1
labels$alpha <- 1
labels[labels$x == 3, "alpha"] <- .1
connect <-
  data.frame(cbind(subdat[subdat$x == 2, c("x", "y")], subdat[subdat$x == 3, c("x", "y")]))
connect$group <- seq(1, 6)
p3 <- ggplot() +
  geom_point(
    data = subdat,
    aes(
      x = x,
      y = y,
      colour = colour,
      alpha = alpha
    ),
    size = 7,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  geom_text(
    data = labels,
    aes(
      x = x,
      y = y,
      label = label,
      alpha = alpha
    ),
    colour = "seashell4",
    size = 5,
    show.legend = FALSE
  ) +
  geom_segment(aes(
    x = .5,
    xend = .7,
    y = 5,
    yend = 5
  ), colour = "ivory3") +
  geom_polygon(
    data = dropShape,
    aes(x = x, y = y, group = group),
    fill = "firebrick1",
    alpha = .2,
    colour = "firebrick1"
  ) +
  geom_text(
    data = dropShapeLabel,
    aes(x = x, y = y, label = label),
    colour = "firebrick1",
    size = 5
  ) +
  geom_segment(
    data = connect,
    aes(
      x = x,
      xend = x.1,
      y = y,
      yend = y.1,
      group = group
    ),
    colour = "ivory3",
    alpha = .5
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "seashell4",
      fill = NA,
      size = 1
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  (
    labs(
      caption = 'Remove correlated column. "In addition to constant attributes,
      repeated attributes occur where two or more attributes have
      identical values for each instance. Such attributes are therefore
      fully correlated, which may effectively result in a single attribute
      being over-represented... This stage again removes data that may
      be genuine, because it can be problematic when data mining." (from page 552)'
    )
  ) +
  xlim(c(.25, 8.5)) +
  coord_equal()
p3

zeros <- subdat[c(8, 30),]
zeros$label <- "0"
p4 <- ggplot() +
  geom_point(
    data = subdat,
    aes(
      x = x,
      y = y,
      colour = colour,
      alpha = alpha
    ),
    size = 7,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  geom_text(
    data = labels,
    aes(
      x = x,
      y = y,
      label = label,
      alpha = alpha
    ),
    colour = "seashell4",
    size = 5,
    show.legend = FALSE
  ) +
  geom_segment(aes(
    x = .5,
    xend = .7,
    y = 5,
    yend = 5
  ), colour = "ivory3") +
  geom_polygon(
    data = dropShape,
    aes(x = x, y = y, group = group),
    fill = "firebrick1",
    alpha = .2,
    colour = "firebrick1"
  ) +
  geom_text(
    data = dropShapeLabel,
    aes(x = x, y = y, label = label),
    colour = "firebrick1",
    size = 5
  ) +
  geom_text(
    data = zeros,
    aes(x = x, y = y, label = label),
    colour = "seashell4",
    size = 6
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "seashell4",
      fill = NA,
      size = 1
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  (
    labs(
      caption = '"missing values have occurred because of a division
      by zero error... because of this we replace all missing values
      with zero, ensuring consistency between data sets." (from page 552)'
    )
  ) +
  xlim(c(.25, 8.5)) +
  coord_equal()
p4

subdat[subdat$y == 5, "alpha"] <- .1
p5 <- ggplot() +
  geom_point(
    data = subdat,
    aes(
      x = x,
      y = y,
      colour = colour,
      alpha = alpha
    ),
    size = 7,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  geom_text(
    data = labels,
    aes(
      x = x,
      y = y,
      label = label,
      alpha = alpha
    ),
    colour = "seashell4",
    size = 5,
    show.legend = FALSE
  ) +
  geom_segment(aes(
    x = .5,
    xend = .7,
    y = 5,
    yend = 5
  ),
  colour = "ivory3",
  alpha = .1) +
  geom_polygon(
    data = dropShape,
    aes(x = x, y = y, group = group),
    fill = "firebrick1",
    alpha = .2,
    colour = "firebrick1"
  ) +
  geom_text(
    data = dropShapeLabel,
    aes(x = x, y = y, label = label),
    colour = "firebrick1",
    size = 5
  ) +
  geom_text(
    data = zeros,
    aes(x = x, y = y, label = label),
    colour = "seashell4",
    size = 6
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "seashell4",
      fill = NA,
      size = 1
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  (
    labs(caption = 'Removal of "theoretically impossible occurrences,"
         such as a negative year value.')
  ) +
  xlim(c(.25, 8.5)) +
  coord_equal()
p5

plot(ggarrange(g, p1, p2, p3, p4, p5, nrow = 1))

# ggsave(
#   "test.pdf",
#   ggarrange(g, p1, p2, p3, p4, p5, nrow = 1),
#   width = 30,
#   height = 8
# )

