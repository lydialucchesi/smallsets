snapshots <- list()
apply_code <- function(df1, df2, df3) {
snapshots[['1']] <- df1
df1$latitude <- "35.2809"
snapshots[['1.1']] <- df1
# bin the defect variable
# snap df1
df1$defect <- ifelse(df1$defect > 1, 1, 0)
snapshots[['2']] <- df1
df1[is.na(df1)] <- 0
snapshots[['2.1']] <- df1
# convert units from minutes to seconds
df1 <- subset(df1, year > 0)
snapshots[['2.2']] <- df1
# snap df3
df3 <- left_join(df1, df2)
snapshots[['3']] <- df3
df3$newCar <- "car"
snapshots[['3.1']] <- df3
df3$time <- NULL
snapshots[['3.2']] <- df3
# catch df4
df4 <- left_join(df3, newdata)
snapshots[['3.3']] <- df4
df4$count <- ifelse(df4$count == 7, 100, df4$count)
snapshots[['3.4']] <- df4
df4$defect <- NULL
snapshots[['3.5']] <- df4
df4$defect2 <- 1
snapshots[['3.6']] <- df4
df4$count <- ifelse(df4$count == 10, 100, df4$count)
# snap df4
snapshots[['4']] <- df4
return(snapshots)
}
