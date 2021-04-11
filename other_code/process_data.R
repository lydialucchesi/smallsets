
# start smallset
df$Id <- seq(1, nrow(df), 1)
# bin the defect variable
# snap df
df$Measure <- ifelse(df$Measure >= 50, 1, 0)
# snap df
df[is.na(df)] <- 0
df <- subset(df, Year > 1989)
df$Time <- df$Time * 60
# snap df
# end smallset

