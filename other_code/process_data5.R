# df1 <- data.frame(
#   year = c(2000, -1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
#   count = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
#   time = c(20, 11, 9, 18, 4, 15, 20, 21, 12, 13, 19, 6),
#   defect = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2),
#   id = seq(1, 12)
# )
# 
# df2 <- data.frame(
#   id = sample(1:12, 4, replace = FALSE),
#   newVar = c(10, 11, 12, 9)
# )

# start smallset
df1$latitude <- "35.2809"
# bin the defect variable
# snap df1
df1$defect <- ifelse(df1$defect > 1, 1, 0)
df1[is.na(df1)] <- 0
# convert units from minutes to seconds
df1 <- subset(df1, year > 0)
# snap df3
df3 <- left_join(df1, df2)
df3$newCar <- "car"
df3$time <- NULL
# snap df3
# end smallset


