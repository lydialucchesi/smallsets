library(smallset)

source("examples/synth/data.R")

step1 <- prepare_smallset(
  data = df,
  code = "examples/synth/resume/preprocessMore.R",
  rowCount = 5,
  rowNums = c(3, 32, 80, 97, 99),
  # auto = 1,
  runBig = TRUE,
  captionTemplateName = "captions",
  captionTemplateDir = paste0(getwd(), "/examples/synth/resume")
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
      "captions" = 2.7,
      "data" = 2,
      "legendText" = 9,
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
  captionSpace = 2.5,
  captionTemplateName = "captionsMoreCompleted",
  timelineFont = "sans",
  captionTemplateDir = paste0(getwd(), "/examples/synth/resume")
)

timeline



