# set.seed(80)

constantC = list("cornsilk4", .4)
changedC = list("cornflowerblue", .4)
addedC = list("blueviolet", .4)
deletedC = list("darkgoldenrod1", .4)

## ONE DATASET
df <- data.frame(
  Year = sample(1989:2010, 100, replace = TRUE),
  Count = sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA), 100, replace = TRUE),
  Time = sample(0:23, 100, replace = TRUE),
  Measure = round(rnorm(100, mean = 50, sd = 10), 1)
)


mylist <- prepare_smallset(
  data = df,
  code = "other_code/process_data.R",
  rowCount = 6,
  rowNums = c(8, 35)
)

fts <- highlight_changes(
  list = mylist,
  captionScript = "mycaptions",
  constant = constantC,
  changed = changedC,
  added = addedC,
  deleted = deletedC,
)

check <- create_timeline(
  ftsList = fts,
  abstract = FALSE,
  sizing =
    list(
      "columns" = 3,
      "tiles" = .8,
      "captions" = 2.75,
      "symbols" = 3,
      "circles" = .15,
      "data" = 2.5,
      "legend" = 8
    ),
  accentCols = "darker",
  accentColsDif = .6,
  stampLoc = 1,
  timelineRows = 1,
  timelineFont = "mono",
  captionSpace = .8
)

check


check <- create_timeline(
  ftsList = fts,
  abstract = TRUE,
  sizing =
    list(
      "columns" = 3,
      "tiles" = .8,
      "captions" = 2.5,
      "symbols" = 3,
      "circles" = .175,
      "data" = 3,
      "legend" = 8
    ),
  accentCols = "darker",
  accentColsDif = .6,
  stampLoc = 5,
  timelineRows = 1,
  timelineFont = "mono",
  captionSpace = .8
)

check
