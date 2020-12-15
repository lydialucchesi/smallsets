library(smallset)

## ONE DATASET
df <- data.frame(
  year = c(2000,-1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  count = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  time = c(20, 11, 9, 18, 4, 15, 20, NA, 12, 13, 19, 6),
  defect = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2)
)

mylist <- prepare_smallset(
  data = df,
  code = "other_code/process_data.R",
  rowCount = 6,
  rowNums = c(2, 5, 8)
)

fts <- highlight_changes(
  list = mylist,
  captionScript = "mycaptions",
  constant = "cornsilk4",
  changed = "cornflowerblue",
  added = "blueviolet",
  deleted = "darkgoldenrod1",
  author = "Lydia"
)

check <- create_timeline(
  ftsList = fts,
  abstract = FALSE,
  sizing =
    list(
      "columns" = 2,
      "tiles" = .8,
      "captions" = 2,
      "symbols" = 2,
      "circles" = .2,
      "data" = 2,
      "legend" = 6
    ),
  accentCols = "darker",
  accentColsDif = .7,
  stampLoc = 5,
  timelineRows = 1,
  timelineFont = "mono",
  captionSpace = .8
)

check


## TWO DATASETS
df1 <- data.frame(
  year = c(2000, -1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  count = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  time = c(20, 11, 9, 18, 4, 15, 20, 21, 12, 13, 19, 6),
  defect = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2),
  id = seq(1, 12)
)

df2 <- data.frame(
  id = sample(1:12, 4, replace = FALSE),
  newVar = c(10, 11, 12, 9)
)

mylist <- prepare_smallset(
  data = list(df1 = df1, df2 = df2),
  code = "other_code/process_data5.R",
  rowCount = list(6, 2),
)

fts <- highlight_changes(
  list = mylist,
  captionScript = "mycaptions",
  constant = "cornsilk4",
  changed = "cornflowerblue",
  added = "blueviolet",
  deleted = "darkgoldenrod1",
  author = "Lydia"
)

check <- create_timeline(
  ftsList = fts,
  abstract = FALSE,
  sizing =
    list(
      "columns" = 2,
      "tiles" = .8,
      "captions" = 2,
      "symbols" = 2,
      "circles" = .2,
      "data" = 2,
      "legend" = 6
    ),
  accentCols = "darker",
  accentColsDif = .7,
  stampLoc = 5,
  timelineRows = 1,
  timelineFont = "mono",
  captionSpace = .8
)

check


## THREE DATASETS
df1 <- data.frame(
  year = c(2000, -1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  count = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  time = c(20, 11, 9, 18, 4, 15, 20, 21, 12, 13, 19, 6),
  defect = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2),
  id = seq(1, 12)
)

df2 <- data.frame(
  id = sample(1:12, 4, replace = FALSE),
  newVar = c(10, 11, 12, 9)
)

newdata <- data.frame(
  id = sample(1:12, 4, replace = FALSE),
  newVar2 = rep("check", 4)
)


mylist <- prepare_smallset(
  data = list(df1 = df1, df2 = df2, df3 = newdata),
  code = "other_code/process_data6.R",
  rowCount = list(6, 2, 2),
)

fts <- highlight_changes(
  list = mylist,
  captionScript = "mycaptions",
  constant = "cornsilk4",
  changed = "cornflowerblue",
  added = "blueviolet",
  deleted = "darkgoldenrod1",
  author = "Lydia"
)

check <- create_timeline(
  ftsList = fts,
  abstract = FALSE,
  sizing =
    list(
      "columns" = 2,
      "tiles" = .8,
      "captions" = 2,
      "symbols" = 2,
      "circles" = .2,
      "data" = 2,
      "legend" = 6
    ),
  accentCols = "darker",
  accentColsDif = .7,
  stampLoc = 5,
  timelineRows = 1,
  timelineFont = "mono",
  captionSpace = .8
)

check



