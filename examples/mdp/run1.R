library(smallset)

# Make a Smallset timeline for preprocessing in gray2011_1.R

# short timeline

mdpData <- readRDS(file = paste0(getwd(), "/examples/mdp/inputs/CM1.rds"))

ignore <- colnames(mdpData)
ignore <- ignore[!ignore %in% c("Defective", 
                                "DECISION_DENSITY", 
                                "PATHOLOGICAL_COMPLEXITY", 
                                "GLOBAL_DATA_COMPLEXITY", 
                                "GLOBAL_DATA_DENSITY", 
                                "NUM_OPERATORS",
                                "NUM_OPERANDS", 
                                "LOC_BLANK",
                                "LOC_EXECUTABLE", 
                                "NUMBER_OF_LINES")]

snapshots <- prepare_smallset(
  data = mdpData,
  code = paste0(getwd(), "/examples/mdp/inputs/gray2011_1.R"),
  rowCount = 6,
  # ran auto = 1 first time to get automated row selection, then added result to rowNums for faster run times
  rowNums = c(34, 129, 163, 239, 336, 388),
  # auto = 1,
  runBig = TRUE,
  ignoreCols = ignore,
  captionTemplateName = "gray2011_1Captions",
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
      "captions" = 2,
      "data" = 2,
      "legendText" = 8,
      "legendIcons" = 1,
      "title" = 10,
      "subtitle" = 10,
      "footnote" = 9,
      "resume" = .25
    ),
  truncateData = 4,
  rotateHeader = TRUE,
  headerSpace = c(4.3, 2.8),
  accentCols = "darker",
  accentColsDif = 1,
  otherTextCol = 1,
  timelineRows = 1,
  timelineFont = "sans",
  captionSpace = 7,
  captionTemplateName = "gray2011_1CaptionsCompleted",
  captionTemplateDir = paste0(getwd(), "/examples/mdp/inputs"),
)

timeline 


