snapshots <- list()
applyCode <- function(df) {
snapshots[[1]] <- df
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
snapshots[[2]] <- df
model <- lm(count ~ time, data = df)
# snap df
df$count <- round(ifelse(is.na(df$count), predict(model, df), df$count))
snapshots[[3]] <- df
df$time <- NULL
# snap df
df[is.na(df)] <- 0
snapshots[[4]] <- df
prepped <- subset(df, year > 0)
# snap prepped
prepped$one <- 1
snapshots[[5]] <- prepped
newRow <- prepped[1, ]
prepped <- rbind(prepped, newRow)
# snap prepped
snapshots[[6]] <- prepped
return(snapshots)
}
