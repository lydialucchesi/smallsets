library(ggplot2)

t <- seq(0, 200, .001)
y <- cos(t)

df <- as.data.frame(cbind(t, y))

ggplot() + geom_path(data = df, aes(x = t, y = y)) + coord_polar()

# https://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2

caption <-
  expression(paste("No. of ", bold("bacteria X"), " isolates with corresponding types"))

library(ggplot2)
ggplot() + labs(y = my_y_title)

mylist <- prep_smallset(
  data = df,
  prepCode = "other_code/prep_data.R",
  rowCount = 6,
  rowNums = c(1, 2, 5)
)

fts <- highlight_changes(
  list = mylist,
  captionScript = "mycaptions",
  constant = "seashell4",
  changed = "darkorchid1",
  added = "darkorange",
  deleted = "goldenrod1"
)
ftsList <- fts
ftsItemNum <- 2

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
xs$variable <- toupper(xs$variable)

circles <- subset(tabLong, value != ftsList[[2]])
circles$xCir <- circles$x - .25
circles$yCir <- circles$y + .25
circles <- circles[1, ]
circles$label <- "B"

# library(ggforce)
# c <- ggplot() +
#   geom_circle(data = circles,
#               aes(x0 = xCir,
#                   y0 = yCir,
#                   r = .2), fill = "lavender") +
#   geom_text(data = circles, aes(x = xCir, y = yCir, label = label), colour = "black", size = 40) +
#   coord_equal() +
#   theme(
#     panel.background = element_rect(fill = "transparent"),
#     # bg of the panel
#     plot.background = element_rect(fill = "transparent", color = NA),
#     # bg of the plot
#     panel.grid.major = element_blank(),
#     # get rid of major grid
#     panel.grid.minor = element_blank(),
#     # get rid of minor grid
#     legend.background = element_rect(fill = "transparent"),
#     # get rid of legend bg
#     legend.box.background = element_rect(fill = "transparent"),
#     # get rid of legend panel bg
#     axis.line = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank()
#   )
# c
# 
# ggsave(c, filename = "stampTest.png",  bg = "transparent")


a <- ggplot() +
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


a + geom_circle(data = circles,
              aes(x0 = xCir,
                  y0 = yCir,
                  r = .1), fill = "white", size = .3, alpha = 1, colour = "#9831cc") +
  geom_text(data = circles, aes(x = xCir, y = yCir, label = "B"), colour = "#9831cc", size = 2.5) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA),
    # bg of the plot
    panel.grid.major = element_blank(),
    # get rid of major grid
    panel.grid.minor = element_blank(),
    # get rid of minor grid
    legend.background = element_rect(fill = "transparent"),
    # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"),
    # get rid of legend panel bg
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


