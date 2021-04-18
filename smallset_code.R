snapshots <- list()
apply_code <- function(bb) {
snapshots[[1]] <- bb
# snap bb
bb$Day <- gsub(".*-*-", "", bb$Day)
snapshots[[2]] <- bb
bb[, c(2, 3, 8, 9)] <- NULL
bb <- subset(bb, !is.na(Rain))
# snap bb
bb <- transform(bb, Rain = c(NA, Rain[-nrow(bb)]))
snapshots[[3]] <- bb
bb <- bb[-1, ]
# resume smallset
bb$Rained <- ifelse(bb$Rain == 0.0, 0, 1)
# snap bb
snapshots[[4]] <- bb
return(snapshots)
}
