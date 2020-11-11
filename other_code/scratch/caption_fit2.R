abstractSmallset  +
  geom_textbox(
    data = captionInfo,
    aes(x = x, y = -1, label = smallsetCaption),
    colour = "black",
    fill = NA,
    box.colour = NA,
    width = grid::unit(.99, "npc"),
    size = 3,
    vjust = c(1),
    valign = c(.5),
    hjust = c(.5),
    halign = c(.5)
  )



df <- data.frame(x = c(3), y = c(-1), caption = c("caption this! does it **bold**?"))
abstractSmallset + geom_richtext(data = df, aes(x = x, y = y, label = caption)) + geom_fit_text()


# rt <- richtext_grob(text = df$caption, x = 3, y = -.5)
as.ggplot(rt)


geom_fit_text(
  data = captionInfo,
  aes(
    xmin = .5,
    xmax = maxDims[1] + .5,
    ymin = -1,
    ymax = 0,
    label = smallsetCaption
  ),
  size = sizing[["captions"]],
  place = 'centre',
  reflow = TRUE
)


# look at rich text grob
# https://www.rdocumentation.org/packages/gridtext/versions/0.1.1/topics/richtext_grob


abstractWithCaption <- abstractSmallset  +
  geom_textbox(
    data = captionInfo,
    aes(x = x, y = y, label = smallsetCaption),
    colour = "black",
    fill = NA,
    box.colour = NA,
    width = grid::unit(.99, "npc"),
    size = sizing[["captions"]],
    vjust = c(1),
    valign = c(.5),
    hjust = c(.5),
    halign = c(.5)
  ) + coord_munch()

abstractWithCaption







# https://www.rdocumentation.org/packages/ggtext/versions/0.1.0

df <- tibble(
  label = c(
    "Some text **in bold.**",
    "Linebreaks<br>Linebreaks<br>Linebreaks",
    "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
    "Some <span style='color:blue'>blue text **in bold.** And *italics text.* 
    And some <span style='font-size:18pt; color:black'>large</span> text."
  ),
  x = c(.2, .1, .5, .9),
  y = c(.8, .4, .1, .5),
  hjust = c(0.5, 0, 0, 1),
  vjust = c(0.5, 1, 0, 0.5),
  angle = c(0, 0, 45, -45),
  color = c("black", "blue", "black", "red"),
  fill = c("cornsilk", "white", "lightblue1", "white")
)


ggplot(df) +
  aes(
    x, y, label = label, angle = angle, color = color, fill = fill,
    hjust = hjust, vjust = vjust
  ) +
  geom_richtext() +
  geom_point(color = "black", size = 2) +
  scale_color_identity() +
  scale_fill_identity() +
  xlim(0, 1) + ylim(0, 1)

library(dplyr)
library(ggplot2)
library(ggtext)

df <- tibble(
  label = c("testing text testing text testing text testing text testing text testing text testing text testing text 
            testing text testing text testing text testing text testing text testing text testing text testing text
             testing text testing text testing text testing text testing text testing text"),
  x = c(0),
  y = c(1),
  hjust = c(.5),
  vjust = c(1),
  orientation = c("upright"),
  color = c("black"),
  fill = c("cornsilk"))


ggplot(df) +
  aes(
    x, y, label = label, color = color, fill = fill,
    hjust = hjust, vjust = vjust,
    orientation = orientation
  ) +
  geom_textbox(width = unit(0.95, "npc"), size = 10) +
  geom_point(color = "black", size = 2) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) + ylim(c(0.5, 1))

p <- ggplot(df) +
  aes(
    x, y, label = label, color = color, fill = fill,
    hjust = hjust, vjust = vjust,
    orientation = orientation
  ) +
  geom_textbox(width = unit(0.95, "npc"), size = 5) +
  geom_point(color = "black", size = 2) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) + ylim(c(-20, 1))

b <- ggplot_build(p)
# ggbuild and get min y value then add ylim to plot


# options for printing with markdown formatting
# geom_textbox
  # ylims are off / hard to set
# geom_richtext
  # Doesn't wrap
# richtext_grob
  # Don't know how to use it

# current using geom_fit_text
  # but doesn't offer rich text formatting


