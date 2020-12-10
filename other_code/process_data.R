
df <- data.frame(
  year = c(2000,-1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  count = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  time = c(20, 11, 9, 18, 4, 15, 20, NA, 12, 13, 19, 6),
  defect = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2)
)

# start smallset
df$latitude <- "35.2809"
# bin the defect variable
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
# snap df
df[is.na(df)] <- 0
# convert units from minutes to seconds
df <- subset(df, year > 0)
df$time <- df$time * 60
# snap df
# end smallset

