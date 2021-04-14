# start smallset
df$Time <- ifelse(df$Period == "PM", df$Time + 12, df$Time)
df[df$Time == 12 & df$Period == "AM", "Time"] <- 0
# snap df
df[df$Time == 24 & df$Period == "PM", "Time"] <- 0
df$Period <- NULL
df <- subset(df, (df$Time > 6) & (df$Time < 20))
# snap df
df[is.na(df$Score), "Score"] <- -1.0
df$Pass <- ifelse(df$Score >= 50, TRUE, FALSE)
# snap df
# end smallset
