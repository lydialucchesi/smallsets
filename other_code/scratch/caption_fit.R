abstractWithCaption <- abstractSmallset +
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

abstractWithCaption <- abstractSmallset  +
  geom_textbox(
    data = captionInfo,
    aes(x = x, y = y, label = smallsetCaption),
    colour = "black",
    fill = NA,
    box.colour = NA,
    width = grid::unit(1.0, "npc"),
    size = sizing[["captions"]],
    halign = c(.5)
  )

scale_fill_identity(
  breaks = tileColGuide$breaks,
  labels = tileColGuide$labels,
  guide = "legend",
  family = timelineFont
)
    
    
ftsList = fts
sizing = list("columns" = 2, "tiles" = 1, "captions" = 8, "circles" = .15, "symbols" = 2.5)
stampCols = "darker"
stampColsDif = .5
stampLoc = 1
timelineRows = NULL
maxDims = c(6, 5)
