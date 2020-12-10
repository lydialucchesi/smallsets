df1 <- data.frame(
  year = c(2000,-1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  count = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  time = c(20, 11, 9, 18, 4, 15, 20, 21, 12, 13, 19, 6),
  defect = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2),
  id = seq(1, 12)
)

df2 <- data.frame(
  id = sample(1:12, 4, replace = FALSE),
  newVar = c(10, 11, 12, 9)
)

df1 <- df1[-1,]
df3 <- left_join(df1, df2)

lcurrent
lprior

# is there a scenario in which the row name length could be the same but a row
# has been added?

if (length(rownames(lcurrent)) != length(rownames(lprior))) {
  rowsAdd <- setdiff(rownames(lcurrent), rownames(lprior))
  if (length(rowsAdd) > 0) {
    tcurrent <-
      flextable::color(tcurrent, color = added, i = rowsAdd)
  }
}

if ()

df1 <- data.frame(
  year = c(2000,-1999, 2000),
  count = c(10, 5, 5),
  time = c(20, 11, 9)
)

df1 <- df1[-12,]

df2 <- data.frame(
  year = c(2001,3000, 2000),
  count = c(10, 5, 5),
  time = c(20, 11, 9)
)

rbind(df1, df2)
