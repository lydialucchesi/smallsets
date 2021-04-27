snapshots <- list()
apply_code <- function(bb) {
snapshots[[1]] <- bb[(row.names(bb) %in% c(1, 11, 18, 21, 26, 28)), ]
# snap bb
bb$Day <- gsub(".*-*-", "", bb$Day)
snapshots[[2]] <- bb[(row.names(bb) %in% c(1, 11, 18, 21, 26, 28)), ]
# bb$Day <- seq(1, 31, 1)
bb[, c(2, 3, 8, 9)] <- NULL
bb <- subset(bb, !is.na(Rain))
# snap bb
bb <- transform(bb, Rain = c(NA, Rain[-nrow(bb)]))
snapshots[[3]] <- bb[(row.names(bb) %in% c(1, 11, 18, 21, 26, 28)), ]
bb <- bb[-1, ]
bb$Rained <- ifelse(bb$Rain == 0.0, 0, 1)
# snap bb
snapshots[[4]] <- bb[(row.names(bb) %in% c(1, 11, 18, 21, 26, 28)), ]
return(snapshots)
}
