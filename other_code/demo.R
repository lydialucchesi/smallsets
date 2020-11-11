library(smallset)

source("other_code/gen_data.R")

mylist <- prep_smallset(
  data = df,
  prepCode = "other_code/prep_data.R",
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
  abstract = TRUE,
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

# some issues with the legend that need to be debugged
mylist <-
  prep_smallset(
    data = df,
    prepCode = "other_code/prep_data2.R",
    rowCount = 10
  )

fts <- highlight_changes(list = mylist,
                         captionScript = "mycaptions",
                         constant = "#aab40a",
                         changed = "#3d3a3e",
                         added = "#ff00f4",
                         deleted = "#0a6bb4")

check <- create_timeline(ftsList = fts)
check



