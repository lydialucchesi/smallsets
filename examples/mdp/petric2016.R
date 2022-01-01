# Translation of NASA MDP data preprocessing strategy proposed in
# "The Jinx on the NASA Software Defect Data Sets" by
# Petric et al. (2016)

# CM1 dataset retrieved from:
# https://github.com/klainfo/NASADefectDataset/tree/master/OriginalData/MDP
# saved as Rds object (see import below)

mdpData <- readRDS(file = "examples/mdp/CM1.rds")

# start smallset mdpData

# STEP 1: Remove cases with implausible values
# Described in Appendix on p. 1214
# Implausible if LOC_TOTAL = 0
mdpData <- subset(mdpData, mdpData$LOC_TOTAL != 0)

# Implausible if value of any attribute is < 0
for (j in seq(1, ncol(mdpData) - 1, 1)) {
  mdpData <- mdpData[mdpData[, j] >= 0 | is.na(mdpData[, j]),]
}

# Implausible if any count is a noninteger
countCols <- grep("COUNT", colnames(mdpData))
for (j in countCols) {
  mdpData <- mdpData[mdpData[, j] %% 1 == 0,]
}



# STEP 2: Remove cases with conflict feature values (referential integrity checks)
# Described in Appendix on p. 1214)
mdpData <- mdpData
# mdpData <- subset(mdpData, NUMBER_OF_LINES >= LOC_TOTAL)
# mdpData <- subset(mdpData, NUMBER_OF_LINES >= LOC_BLANK)
# mdpData <- subset(mdpData, NUMBER_OF_LINES >= LOC_CODE_AND_COMMENT)
mdpData <- subset(mdpData, NUMBER_OF_LINES >= LOC_COMMENTS)
# mdpData <- subset(mdpData, NUMBER_OF_LINES >= LOC_EXECUTABLE)
# mdpData <- subset(mdpData, LOC_TOTAL >= LOC_EXECUTABLE)
# mdpData <- subset(mdpData, LOC_TOTAL >= LOC_CODE_AND_COMMENT)
# mdpData <- subset(mdpData, NUM_OPERANDS >= NUM_UNIQUE_OPERANDS)
# mdpData <- subset(mdpData, NUM_OPERATORS >= NUM_UNIQUE_OPERATORS)
# mdpData <-
#   subset(mdpData, HALSTEAD_LENGTH == NUM_OPERATORS + NUM_OPERANDS)
# mdpData <-
#   subset(mdpData, CYCLOMATIC_COMPLEXITY <= NUM_OPERATORS + 1)
# mdpData <- subset(mdpData, CALL_PAIRS <= NUM_OPERATORS)

# HALSTEAD_VOLUME = (NUM_OPERATORS + NUM_OPERANDS) * log2(NUM_UNIQUE_OPERATORS +
# NUM_UNIQUE_OPERANDS)
# Round second half of expression to two decimal places
# HALSTEAD_VOLUME <-
#   round((mdpData$NUM_OPERATORS + mdpData$NUM_OPERANDS) * (
#     log2(mdpData$NUM_UNIQUE_OPERATORS + mdpData$NUM_UNIQUE_OPERANDS)
#   ), 2)
# Manual check
# table(mdpData$HALSTEAD_VOLUME - HALSTEAD_VOLUME)
# Three that are .01 off, that is fine. Keep all.

# HALSTEAD_LEVEL = (2 / NUM_UNIQUE_OPERATORS) * (NUM_UNIQUE_OPERANDS /
# NUM_OPERANDS)
# Round second half of expression to two decimal places
# HALSTEAD_LEVEL <-
#   round((2 / mdpData$NUM_UNIQUE_OPERATORS) * (mdpData$NUM_UNIQUE_OPERANDS / mdpData$NUM_OPERANDS),
#         2
#   )
# Manual check
# table(mdpData$HALSTEAD_LEVEL - HALSTEAD_LEVEL)
# Three that are .01 off, that is fine. Keep all.

# HALSTEAD_DIFFICULTY = (NUM_UNIQUE_OPERATORS / 2) * (NUM_OPERANDS /
# NUM_UNIQUE_OPERANDS)
# Round second half of expression to two decimal places
# HALSTEAD_DIFFICULTY <-
#   round((mdpData$NUM_UNIQUE_OPERATORS / 2) * (mdpData$NUM_OPERANDS / mdpData$NUM_UNIQUE_OPERANDS),
#         2
#   )
# Manual check
# table(mdpData$HALSTEAD_DIFFICULTY - HALSTEAD_DIFFICULTY)
# 16 that are .01 off, that is fine. Keep all.

# HALSTEAD_CONTENT = HALSTEAD_VOLUME / HALSTEAD_DIFFICULTY
# Round second half of expression to two decimal places
# HALSTEAD_CONTENT <-
#   round(mdpData$HALSTEAD_VOLUME / mdpData$HALSTEAD_DIFFICULTY, 2)
# Manual check
# sort(mdpData$HALSTEAD_CONTENT - HALSTEAD_CONTENT, na.last = TRUE)
# Four Inf/NaNs are where HALSTEAD_DIFFICULTY (divisor) equals zero. Others are
# within reasonable range. Keep all.

# HALSTEAD_EFFORT = HALSTEAD_VOLUME * HALSTEAD_DIFFICULTY
# Second half of expression
# HALSTEAD_EFFORT  <-
#   mdpData$HALSTEAD_VOLUME * mdpData$HALSTEAD_DIFFICULTY
# Manual check
# sort(round(mdpData$HALSTEAD_EFFORT, 0) - round(HALSTEAD_EFFORT, 0),
#      na.last = TRUE)
# Keep all.

# HALSTEAD_PROG_TIME = HALSTEAD_EFFORT / 18
# Round second half of expression to two decimal places
# HALSTEAD_PROG_TIME <- round((mdpData$HALSTEAD_EFFORT / 18), 2)
# Manual check
# sort(table(mdpData$HALSTEAD_PROG_TIME - HALSTEAD_PROG_TIME))
# Keep all.


# STEP 3: Remove identical cases
# snap mdpData
mdpData <- mdpData
# mdpData <- mdpData[!duplicated(mdpData),]
dups <- read.table("examples/mdp/duplicates.txt")$V1
mdpData <- mdpData[!rownames(mdpData) %in% dups, ]



# STEP 4: Remove inconsistent cases
mdpData <- mdpData
# mdpData[duplicated(mdpData[, 1:ncol(mdpData) - 1]), colnames(mdpData)]
mdpData <- mdpData[!rownames(mdpData) %in% c(388, 390), ]



# STEP 5: Remove cases with missing values
# snap mdpData
mdpData <- mdpData
mdpData <- na.omit(mdpData)



# STEP 6: Remove constant features
# snap mdpData
mdpData <- mdpData
mdpData <- remove_constant(mdpData, quiet = TRUE)



# STEP 7: Run IC1 and IC2 tests
# Check 1 (Rule IC1)
mdpData$IC1 <- mdpData$NUMBER_OF_LINES -
  mdpData$LOC_CODE_AND_COMMENT -
  mdpData$LOC_COMMENTS -
  mdpData$LOC_EXECUTABLE -
  mdpData$LOC_BLANK
mdpData <- subset(mdpData, IC1 == 0 | IC1 == 1)
mdpData$IC1 <- NULL
# Check 2 (Rule IC2)
mdpData$IC2 <-
  (mdpData$LOC_TOTAL) == (mdpData$LOC_CODE_AND_COMMENT + mdpData$LOC_EXECUTABLE)
mdpData <- subset(mdpData, IC2 == TRUE)
mdpData$IC2 <- NULL

# end smallset mdpData
