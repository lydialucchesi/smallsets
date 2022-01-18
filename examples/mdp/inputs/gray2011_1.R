# Translation of NASA MDP data preprocessing strategy proposed in
# "The Misuse of the NASA Metrics Data Program Data Sets for Automated Software Defect Prediction" by
# Gray et al. (2011)

# CM1 dataset retrieved from:
# https://github.com/klainfo/NASADefectDataset/tree/master/OriginalData/MDP
# saved as Rds object (see import below)

mdpData <- readRDS(file = "examples/mdp/inputs/CM1.rds")

# start smallset mdpData

# STEP 1: Remove constant attributes
# Section 3.2, p. 551
mdpData$GLOBAL_DATA_COMPLEXITY <- NULL
mdpData$GLOBAL_DATA_DENSITY <- NULL
mdpData$PATHOLOGICAL_COMPLEXITY <- NULL



# STEP 2: Remove repeated attributes
# Section 3.3, p. 551
mdpData <- mdpData[!duplicated(as.list(mdpData))]



# STEP 3: Replace missing values
# Section 3.4, p. 552
# snap mdpData
mdpData$DECISION_DENSITY[is.na(mdpData$DECISION_DENSITY)] <- 0



# STEP 4: Run integrity checks
# Section 3.5, p. 552
# mdpData <-
#   subset(mdpData,
#          mdpData$HALSTEAD_LENGTH == mdpData$NUM_OPERANDS + mdpData$NUM_OPERATORS)
# mdpData <-
#   subset(mdpData,
#          mdpData$CYCLOMATIC_COMPLEXITY <= mdpData$NUM_OPERATORS + 1)
# mdpData <- subset(mdpData, mdpData$CYCLOMATIC_COMPLEXITY >= 1)
# mdpData <-
#   subset(mdpData, mdpData$CALL_PAIRS <= mdpData$NUM_OPERATORS)



# STEP 5: Remove repeated and inconsistent cases
# snap mdpData
mdpData <- mdpData
dups <- read.table("examples/mdp/inputs/duplicates.txt")$V1
# Remove duplicate cases
mdpData <- mdpData[!rownames(mdpData) %in% dups, ]
mdpData <- mdpData[!rownames(mdpData) %in% c(388, 390), ]

# end smallset mdpData
