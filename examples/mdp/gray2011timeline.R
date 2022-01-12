# Make a Smallset timeline for preprocessing in gray2011_1.R and gray2011_2.R

library(smallset)

# short

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
  code = paste0(getwd(), "/examples/mdp/gray2011_1.R"),
  rowCount = 6,
  # from petric auto = 1 output (enabling comparisons between three)
  # rowNums = c(34, 129, 163, 239, 336, 388),
  auto = 1,
  runBig = TRUE,
  ignoreCols = ignore,
  captionTemplateName = "gray2011_1Captions",
  captionTemplateDir = paste0(getwd(), "/examples/mdp"),
)

timeline <- create_timeline(
  snapshotList = snapshots,
  colScheme = "colScheme1",
  abstract = FALSE,
  ghostData = TRUE,
  highlightNA = TRUE,
  sizing =
    list(
      "columns" = 2.5,
      "tiles" = .1,
      "captions" = 3,
      "data" = 1.8,
      "legendText" = 10,
      "legendIcons" = 1,
      "title" = 12,
      "subtitle" = 10,
      "footnote" = 9,
      "resume" = .25
    ),
  truncateData = 4,
  rotateHeader = TRUE,
  headerSpace = c(6, 2.75),
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  timelineFont = "sans",
  captionSpace = 7,
  captionTemplateName = "gray2011_1CaptionsCompleted",
  captionTemplateDir = paste0(getwd(), "/examples/mdp"),
)

timeline 


# long

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
                                "LOC_BLANK",
                                "CONDITION_COUNT",
                                "DECISION_COUNT",
                                "HALSTEAD_LENGTH",
                                "CYCLOMATIC_COMPLEXITY",
                                "CALL_PAIRS")]

snapshots <- prepare_smallset(
  data = mdpData,
  code = paste0(getwd(), "/examples/mdp/gray2011_2.R"),
  rowCount = 10,
  # ran auto = 1 first time to get automated row selection, then added result to rowNums for faster run times
  rowNums = c(10, 141, 233, 235, 271, 290, 300, 388, 412, 443),
  # auto = 1,
  runBig = TRUE,
  ignoreCols = ignore,
  captionTemplateName = "gray2011_2Captions",
  captionTemplateDir = paste0(getwd(), "/examples/mdp"),
)

timeline <- create_timeline(
  snapshotList = snapshots,
  colScheme = "colScheme1",
  abstract = FALSE,
  ghostData = TRUE,
  highlightNA = TRUE,
  sizing =
    list(
      "columns" = 2.5,
      "tiles" = .1,
      "captions" = 2.5,
      "data" = 1.5,
      "legendText" = 8,
      "legendIcons" = 1,
      "title" = 12,
      "subtitle" = 8,
      "footnote" = 7,
      "resume" = .25
    ),
  truncateData = 4,
  rotateHeader = TRUE,
  headerSpace = c(6, 3),
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 2,
  timelineFont = "sans",
  captionSpace = 8,
  captionTemplateName = "gray2011_2CaptionsCompleted",
  captionTemplateDir = paste0(getwd(), "/examples/mdp"),
)

timeline 


