library(charlatan)

set.seed(7)

testVector <- MissingDataProvider$new()

df <- data.frame(
  C1 = ch_integer(n = 100, min = 1, max = 5),
  C2 = sample(
    c(TRUE, FALSE),
    size = 100,
    replace = TRUE,
    prob = c(.9, .1)
  ),
  C3 = ch_integer(n = 100, min = 20, max = 40),
  C4 = ch_integer(n = 100, min = 100, max = 200),
  C5 = round(ch_norm(n = 100, mean = 5, sd = 1), 2),
  C6 = round(testVector$make_missing(ch_norm(
    n = 100, mean = 10, sd = 3
  )), 2),
  C7 = round(testVector$make_missing(ch_norm(
    n = 100, mean = 0, sd = 1
  )), 2),
  C8 = round(testVector$make_missing(ch_norm(
    n = 100, mean = 0, sd = 1
  )), 2)
)


