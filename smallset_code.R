snapshots <- list()
applyCode <- function(df) {
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
snapshots[[1]] <- df
model <- lm(count ~ time, data = df)
# snap df
df$count <- round(ifelse(is.na(df$count), predict(model, df), df$count))
snapshots[[2]] <- df
df$time <- NULL
# snap df
df[is.na(df)] <- 0
snapshots[[3]] <- df
prepped <- subset(df, year > 0)
# snap prepped
snapshots[[4]] <- prepped
return(snapshots)
}
