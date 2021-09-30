# This code is to make a Smallset timeline.

library(smallset)

source("examples/gender/data.R")

# Run first smallset function
# (Original run: four selected (83, 192, 494, 929) and two sampled)
# (Now all six specified to recreate the same figure each time)
step1 <- prepare_smallset(
  data = dat,
  code = "examples/gender/preprocess.R",
  rowCount = 6,
  rowNums = c(83, 192, 276, 494, 518, 929),
  runBig = FALSE,
  ignoreCols = NULL,
  captionTemplateName = "mycaptions",
  captionTemplateDir = "examples/gender/"
)

# Run second smallset function
timeline <- create_timeline(
  snapshotList = step1,
  colScheme = "colScheme1",
  abstract = FALSE,
  ghostData = TRUE,
  highlightNA = TRUE,
  sizing =
    list(
      "columns" = 2.8,
      "tiles" = .3,
      "captions" = 2.8,
      "data" = 2.1,
      "legendText" = 9,
      "legendIcons" = 1,
      "title" = 12,
      "subtitle" = 10,
      "footnote" = 7,
      "resume" = .25
    ),
  truncateData = 6,
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  timelineFont = "serif",
  captionSpace = 5.2,
  captionTemplateName = "mycaptionsCompleted",
  captionTemplateDir = "examples/gender/"
)

# View figure
timeline
