# start smallset df
df <- df[df$C2 == TRUE, ]  

mC6 <- tapply(df$C6, df$C1, function(x) mean(x, na.rm = TRUE))
C6sub <- as.factor(df$C1)
levels(C6sub) <- mC6
df$C6[is.na(df$C6)] <- round(as.numeric(levels(C6sub))[C6sub][is.na(df$C6)], 2)

mC8 <- tapply(df$C8, df$C1, function(x) mean(x, na.rm = TRUE))
C8sub <- as.factor(df$C1)
levels(C8sub) <- mC8
# snap df
df$C8[is.na(df$C8)] <- round(as.numeric(levels(C8sub))[C8sub][is.na(df$C8)], 2)

df$C7 <- NULL

df$C9 <- df$C3 + df$C4
# end smallset df





