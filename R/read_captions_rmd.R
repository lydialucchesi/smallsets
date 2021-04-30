#' Read the caption R markdown file
#' @description A function to interpret the caption R markdown file
#' @keywords internal
#' @export

read_captions_rmd <- function(script, pathway) {

  plotInfo <- as.data.frame(readLines(paste0(pathway, "/", script, ".Rmd")))
  colnames(plotInfo) <- c("lines")
  
  changed <- subset(plotInfo, grepl("1. Changed", plotInfo$lines))
  changed <- rbind(empty <- data.frame(lines = c("None")), changed)
  changed$id <- seq(1, nrow(changed), 1)
  row.names(changed) <- seq(1, nrow(changed), 1)
  colnames(changed) <- c("changed", "id")
  changed$changed <- sub("1. Changed.*:", "", changed$changed)
  # colEx <- subset(plotInfo, grepl("1. Changed ", plotInfo$lines))$lines[1]
  # changed$col1 <- gsub("[\\(\\)]", "", regmatches(colEx, gregexpr("\\(.*?\\)", colEx))[[1]])
  # changed$changed <- trimws(changed$changed, which = c("left"))
  
  added <- subset(plotInfo, grepl("2. Added", plotInfo$lines))
  added <- rbind(empty <- data.frame(lines = c("None")), added)
  added$id <- seq(1, nrow(added), 1)
  row.names(added) <- seq(1, nrow(added), 1)
  colnames(added) <- c("added", "id")
  added$added <- sub("2. Added.*:", "", added$added)
  # colEx <- subset(plotInfo, grepl("2. Added", plotInfo$lines))$lines[1]
  # added$col2 <- gsub("[\\(\\)]", "", regmatches(colEx, gregexpr("\\(.*?\\)", colEx))[[1]])
  # added$added <- trimws(added$added, which = c("left"))
  
  deleted <- subset(plotInfo, grepl("3. Deleted", plotInfo$lines))
  deleted <- rbind(empty <- data.frame(lines = c("None")), deleted)
  deleted$id <- seq(1, nrow(deleted), 1)
  row.names(deleted) <- seq(1, nrow(deleted), 1)
  colnames(deleted) <- c("deleted", "id")
  deleted$deleted <- sub("3. Deleted.*:", "", deleted$deleted)
  # colEx <- subset(plotInfo, grepl("3. Deleted", plotInfo$lines))$lines[1]
  # deleted$col3 <- gsub("[\\(\\)]", "", regmatches(colEx, gregexpr("\\(.*?\\)", colEx))[[1]])
  # deleted$deleted <- trimws(deleted$deleted, which = c("left"))
  
  captions <- subset(plotInfo, grepl("Caption:", plotInfo$lines))
  captions$id <- seq(1, nrow(captions), 1)
  row.names(captions) <- seq(1, nrow(captions), 1)
  colnames(captions) <- c("caption", "id")
  captions$caption <- sub("Caption: ", "", captions$caption)
  
  plotInfo <- merge(changed, added)
  plotInfo <- merge(plotInfo, deleted)
  plotInfo <- merge(plotInfo, captions)
  
  return(plotInfo)
  
}
