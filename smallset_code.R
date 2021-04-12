snapshots <- list()
apply_code <- function(df) {
snapshots[[1]] <- df
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
snapshots[[2]] <- df
df$total <- df$count + df$time
# if I drop incomplete cases here I get an error
df <- df[complete.cases(df),]
# snap df
df$time <- df$time * 60
snapshots[[3]] <- df
df$char <- "testing char"
# snap df
snapshots[[4]] <- df
return(snapshots)
}
