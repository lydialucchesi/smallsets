read_captions_rmd <- function() {
  plotInfo <- as.data.frame(readLines("captions.Rmd"))
  colnames(plotInfo) <- c("lines")
  
  symbols <- subset(plotInfo, grepl("Symbol:", plotInfo$lines))
  symbols <- rbind(empty <- data.frame(lines = c("None")), symbols)
  symbols$id <- seq(1, nrow(symbols), 1)
  row.names(symbols) <- seq(1, nrow(symbols), 1)
  colnames(symbols) <- c("symbol", "id")
  symbols$symbol <- sub("Symbol: ", "", symbols$symbol)
  
  captions <- subset(plotInfo, grepl("Caption:", plotInfo$lines))
  captions$id <- seq(1, nrow(captions), 1)
  row.names(captions) <- seq(1, nrow(captions), 1)
  colnames(captions) <- c("caption", "id")
  captions$caption <- sub("Caption: ", "", captions$caption)
  
  plotInfo <- merge(symbols, captions)
  
  return(plotInfo)
  
}