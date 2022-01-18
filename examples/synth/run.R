library(smallset)

source("examples/synth/inputs/data.R")

step1 <- prepare_smallset(
  data = df,
  code = "examples/synth/inputs/preprocess.R",
  rowCount = 5,
  # these rowNums were obtained with initial run of auto = 2, added to rowNums to reduce future run times
  rowNums = c(3, 32, 80, 97, 99),
  # auto = 2,
  runBig = TRUE,
  captionTemplateName = "captions",
  captionTemplateDir = paste0(getwd(), "/examples/synth/inputs")
)

timeline <- create_timeline(
  snapshotList = step1,
  colScheme = "colScheme1",
  abstract = FALSE,
  ghostData = TRUE,
  highlightNA = TRUE,
  sizing =
    list(
      "columns" = 3,
      "tiles" = .5,
      "captions" = 3.5,
      "data" = 3,
      "legendText" = 11,
      "legendIcons" = 1,
      "title" = 12,
      "subtitle" = 8,
      "footnote" = 7,
      "resume" = .25
    ),
  rotateHeader = FALSE,
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  captionSpace = 2,
  captionTemplateName = "captionsCompleted",
  timelineFont = "sans",
  captionTemplateDir = paste0(getwd(), "/examples/synth/inputs")
)

timeline



