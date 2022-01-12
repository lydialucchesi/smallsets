

# start smallset df
df <- df[df$C2 == TRUE, ]

meansC6 <- round(aggregate(df$C6, list(df$C1), FUN = mean, na.rm = TRUE), 2)
colnames(meansC6) <- c("C1",  "mean")

meansC8 <- round(aggregate(df$C8, list(df$C1), FUN = mean, na.rm = TRUE), 2)
colnames(meansC8) <- c("C1",  "mean")

for (i in 1:nrow(df)) {
  if (is.na(df$C6[i])) {
    cat <- df$C1[i]
    df$C6[i] <- meansC6[meansC6$C1 == cat, "mean"]
  }
}



for (i in 1:nrow(df)) {
  if (is.na(df$C8[i])) {
    cat <- df$C1[i]
    df$C8[i] <- meansC8[meansC8$C1 == cat, "mean"]
  }
}

# snap df
df <- df
df$C7 <- NULL

# snap df
df$C9 <- df$C3 + df$C4

# resume df
t <- quantile(df$C9, c(0:3 / 3))
df$C10 = with(df,
              cut(
                C9,
                t,
                include.lowest = T,
                labels = c("Low", "Med", "High")
              ))
# end smallset df



