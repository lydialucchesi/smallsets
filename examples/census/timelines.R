library(smallset)

# original preprocessing approach
caData <- readRDS("examples/census/caData.Rds")

step1 <- prepare_smallset(
  data = caData,
  code = "examples/census/preprocessOrig.R",
  rowCount = 8,
  runBig = FALSE,
  captionTemplateName = "preprocessOrigCaptions",
  captionTemplateDir = paste0(getwd(), "/examples/census")
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
      "tiles" = .3,
      "captions" = 2,
      "data" = 2,
      "legendText" = 8,
      "legendIcons" = 1,
      "title" = 8,
      "subtitle" = 8,
      "footnote" = 7,
      "resume" = .25
    ),
  rotateHeader = TRUE,
  headerSpace = c(1.5, 1),
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  timelineFont = "serif",
  captionSpace = 6,
  captionTemplateName = "preprocessOrigCaptions",
  captionTemplateDir = paste0(getwd(), "/examples/census")
)

timeline


# alternative preprocessing approach
caData <- readRDS("examples/census/caData.Rds")

step1 <- prepare_smallset(
  data = caData,
  code = "examples/census/preprocessDif.R",
  rowCount = 8,
  runBig = FALSE,
  captionTemplateName = "preprocessDifCaptions",
  captionTemplateDir = paste0(getwd(), "/examples/census")
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
      "tiles" = .3,
      "captions" = 2,
      "data" = 2,
      "legendText" = 8,
      "legendIcons" = 1,
      "title" = 8,
      "subtitle" = 8,
      "footnote" = 7,
      "resume" = .25
    ),
  rotateHeader = TRUE,
  headerSpace = c(1.5, 1),
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  timelineFont = "serif",
  captionSpace = 6,
  captionTemplateName = "preprocessDifCaptions",
  captionTemplateDir = paste0(getwd(), "/examples/census")
)

timeline
