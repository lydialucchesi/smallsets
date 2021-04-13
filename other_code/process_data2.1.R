
# start smallset
# snap df
df$V4 <- ifelse(df$V4 > 1, 1, 0)
df$V5 <- df$V2 + df$V3
df <- df[complete.cases(df),]
# snap df
df$V3 <- df$V3 * 60
# resume smallset
df$V6 <- "A"
# snap df
# end smallset