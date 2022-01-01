# Make a Smallset timeline for preprocessing in petric2016.R

library(smallset)

mdpData <- readRDS(file = "examples/mdp/CM1.rds")

ignore <- colnames(mdpData)
ignore <- ignore[!ignore %in% c("Defective", 
                                "DECISION_DENSITY", 
                                "PATHOLOGICAL_COMPLEXITY", 
                                "GLOBAL_DATA_COMPLEXITY", 
                                "GLOBAL_DATA_DENSITY", 
                                "NUM_OPERATORS",
                                "NUM_OPERANDS", 
                                "LOC_TOTAL", 
                                "NUMBER_OF_LINES", 
                                "LOC_COMMENTS",
                                "LOC_CODE_AND_COMMENT", 
                                "LOC_EXECUTABLE", 
                                "LOC_BLANK")]

snapshots <- prepare_smallset(
  data = mdpData,
  code = paste0(getwd(), "/examples/mdp/shepperd2013.R"),
  rowCount = 8,
  rowNums = c(1, 2, 11, 156, 212, 221, 236, 265),
  runBig = TRUE,
  ignoreCols = ignore,
  captionTemplateName = "shepperd2013Captions",
  captionTemplateDir = paste0(getwd(), "/examples/mdp"),
)

timeline <- create_timeline(
  snapshotList = snapshots,
  constant = list("#C4B8B1", .5),
  changed = list("#4E7BD4", .7),
  added = list("#55C447", .6),
  deleted = list("#E89C2A", .7),
  abstract = FALSE,
  ghostData = TRUE,
  highlightNA = TRUE,
  sizing =
    list(
      "columns" = 1.4,
      "tiles" = .1,
      "captions" = 1.8,
      "data" = 1.1,
      "legendText" = 5,
      "legendIcons" = .7,
      "title" = 10,
      "subtitle" = 8,
      "footnote" = 7,
      "resume" = .25
    ),
  truncateData = 4,
  rotateHeader = TRUE,
  headerSpace = c(6, 2.5),
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  timelineFont = "sans",
  captionSpace = 4,
  captionTemplateName = "shepperd2013Captions",
  captionTemplateDir = paste0(getwd(), "/examples/mdp"),
)

timeline 
