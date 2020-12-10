library(smallset)

## ONE DATASET
source("other_code/gen_data.R")

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
source("other_code/process_data5.R")

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
source("other_code/process_data6.R")

mylist <- prepare_smallset(
  data = list(df1 = df1, df2 = df2, df3 = df3),
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



