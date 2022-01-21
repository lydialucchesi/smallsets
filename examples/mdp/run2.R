library(smallset)

# Make a Smallset timeline for preprocessing in gray2011_2.R

# long timeline

mdpData <- readRDS(file = "examples/mdp/inputs/CM1.rds")

ignore <- colnames(mdpData)
ignore <- ignore[!ignore %in% c("Defective", 
                                "DECISION_DENSITY", 
                                "PATHOLOGICAL_COMPLEXITY", 
                                "GLOBAL_DATA_COMPLEXITY", 
                                "GLOBAL_DATA_DENSITY", 
                                "NUM_OPERATORS",
                                "NUM_OPERANDS", 
                                "NUMBER_OF_LINES", 
                                "LOC_EXECUTABLE", 
                                "LOC_BLANK",
                                "CONDITION_COUNT",
                                "DECISION_COUNT",
                                "HALSTEAD_LENGTH",
                                "CYCLOMATIC_COMPLEXITY",
                                "CALL_PAIRS")]

snapshots <- prepare_smallset(
  data = mdpData,
  code = paste0(getwd(), "/examples/mdp/inputs/gray2011_2.R"),
  rowCount = 10,
  # ran auto = 1 first time to get automated row selection, then added result to rowNums for faster run times
  rowNums = c(10, 141, 233, 235, 271, 290, 300, 388, 412, 443),
  # auto = 1,
  runBig = TRUE,
  ignoreCols = ignore,
  captionTemplateName = "gray2011_2Captions",
  captionTemplateDir = paste0(getwd(), "/examples/mdp/inputs"),
)

timeline <- create_timeline(
  snapshotList = snapshots,
  colScheme = "colScheme1",
  abstract = FALSE,
  ghostData = TRUE,
  highlightNA = TRUE,
  sizing =
    list(
      "columns" = 2,
      "tiles" = .1,
      "captions" = 1.8,
      "data" = 1,
      "legendText" = 8,
      "legendIcons" = 1,
      "title" = 12,
      "subtitle" = 8,
      "footnote" = 7,
      "resume" = .25
    ),
  truncateData = 4,
  rotateHeader = TRUE,
  headerSpace = c(5.5, 3.7),
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 2,
  timelineFont = "sans",
  captionSpace = 9,
  captionTemplateName = "gray2011_2CaptionsCompleted",
  captionTemplateDir = paste0(getwd(), "/examples/mdp/inputs"),
)

timeline 


