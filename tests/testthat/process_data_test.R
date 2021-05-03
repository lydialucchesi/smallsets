
library(magrittr)

df <- data.frame(
  V1 = c(2000,-1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  V2 = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  V3 = c(20, 11, 9, 18, 4, 15, 20, NA, 12, 13, 19, 6),
  V4 = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2)
)

# start smallset
# snap df
df$V4 <- ifelse(df$V4 > 1, 1, 0)
df$V5 <- df$V2 + df$V3
df <- df[complete.cases(df),]
# snap df2
df2 <- df %>% transform(V3 = V3 %>% multiply_by(60))
# resume smallset
df2$V6 <- "A"
# snap df2
# end smallset
