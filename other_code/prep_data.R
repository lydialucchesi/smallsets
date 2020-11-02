
df <- data.frame(
  year = c(2000,-1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  count = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  time = c(20, 11, 9, 18, 4, 15, 20, 21, 12, 13, 19, 6),
  defect = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2),
  true = c(NA, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0)
)

plot(df$count, df$time)

# start smallset
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
model <- lm(count ~ time, data = df)
# snap df
df$count <- round(ifelse(is.na(df$count), predict(model, df), df$count))
df$time <- NULL
# snap df
df[is.na(df)] <- 0
prepped <- subset(df, year > 0)
# snap prepped
prepped$one <- 1
newRow <- prepped[1, ]
prepped <- rbind(prepped, newRow)
# snap prepped
# end smallset


# newRow <- prepped[1, ]
# prepped <- rbind(prepped, newRow)