library(smallset)

# Smallset timeline for alternative preprocessing approach
caData <- readRDS("examples/census/inputs/caData.Rds")

step1 <- prepare_smallset(
  data = caData,
  code = "examples/census/inputs/preprocess.R",
  rowCount = 6,
  # obtained through random sampling first time running prepare_smallset then added to rowNums
  rowNums = c(160189, 190367, 191891, 304020, 332285, 371085),
  runBig = FALSE,
  captionTemplateName = "preprocessCaptions",
  captionTemplateDir = paste0(getwd(), "/examples/census/inputs")
)

timeline <- create_timeline(
  snapshotList = step1,
  colScheme = "colScheme2",
  abstract = TRUE,
  ghostData = TRUE,
  highlightNA = FALSE,
  sizing =
    list(
      "columns" = 4.2,
      "tiles" = .3,
      "captions" = 4,
      "data" = 3,
      "legendText" = 14,
      "legendIcons" = 1.5,
      "title" = 14,
      "subtitle" = 12,
      "footnote" = 8,
      "resume" = .25
    ),
  rotateHeader = TRUE,
  accentCols = "lighter",
  headerSpace = c(2, .5),
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  timelineFont = "sans",
  captionSpace = 3.5,
  captionTemplateName = "preprocessCaptionsCompleted",
  captionTemplateDir = paste0(getwd(), "/examples/census/inputs")
)

timeline
