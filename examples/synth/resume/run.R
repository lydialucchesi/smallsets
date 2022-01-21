library(smallset)

source("examples/synth/inputs/data.R")

step1 <- prepare_smallset(
  data = df,
  code = paste0(getwd(), "/examples/synth/resume/inputs/preprocess.R"),
  rowCount = 5,
  rowNums = c(3, 32, 80, 97, 99),
  # auto = 1,
  runBig = TRUE,
  captionTemplateName = "captions",
  captionTemplateDir = paste0(getwd(), "/examples/synth/resume/inputs")
)

timeline <- create_timeline(
  snapshotList = step1,
  colScheme = "colScheme1",
  abstract = FALSE,
  ghostData = TRUE,
  highlightNA = TRUE,
  sizing =
    list(
      "columns" = 2,
      "tiles" = .5,
      "captions" = 2,
      "data" = 1.8,
      "legendText" = 8,
      "legendIcons" = 1,
      "title" = 12,
      "subtitle" = 8,
      "footnote" = 7,
      "resume" = .65
    ),
  rotateHeader = FALSE,
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  captionSpace = 3,
  captionTemplateName = "captionsCompleted",
  timelineFont = "sans",
  captionTemplateDir = paste0(getwd(), "/examples/synth/resume/inputs")
)

timeline



