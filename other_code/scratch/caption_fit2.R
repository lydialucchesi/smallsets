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