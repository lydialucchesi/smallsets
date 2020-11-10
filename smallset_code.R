snapshots <- list()
apply_code <- function(df) {
snapshots[[1]] <- df
df$latitude <- "35.2809"
# bin the defect variable
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
snapshots[[2]] <- df
# snap df
df[is.na(df)] <- 0
snapshots[[3]] <- df
# convert units from minutes to seconds
df <- subset(df, year > 0)
df$time <- df$time * 60
# snap df
snapshots[[4]] <- df
return(snapshots)
}
