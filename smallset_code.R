snapshots <- list()
apply_code <- function(df) {
snapshots[[1]] <- df
# snap df
df$V4 <- ifelse(df$V4 > 1, 1, 0)
snapshots[[2]] <- df
df$V5 <- df$V2 + df$V3
df <- df[complete.cases(df),]
# snap df
df$V3 <- df$V3 * 60
snapshots[[3]] <- df
# resume smallset
df$V6 <- "A"
# snap df
snapshots[[4]] <- df
return(snapshots)
}
