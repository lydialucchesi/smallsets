df1 <- data.frame(Id = seq(1, 100, 1),
                  Time = sample(1:12, 100, replace = TRUE),
                  Period = sample(c("AM", "PM"), 100, replace = TRUE),
                  Var1 = round(rnorm(100, mean = 0, sd = 1), 3))

df1$Var1 <- ifelse(df1$Var1 < -1.0, NA, df1$Var1)

# start smallset
df1$Time <- ifelse(df1$Period == "PM", df1$Time + 12, df1$Time)
df1[df1$Time == 12 & df1$Period == "AM", "Time"] <- 0
df1[df1$Time == 24 & df1$Period == "PM", "Time"] <- 0
# snap df1
df1$Period <- NULL
df1 <- subset(df1, (df1$Time > 6) & (df1$Time < 20))
# snap df1
df1[is.na(df1$Var1), "Var1"] <- -1.0
df1$Var2 <- ifelse(df1$Var1 >= 0, TRUE, FALSE)
# snap df1
# end smallset



