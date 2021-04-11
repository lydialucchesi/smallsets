df1 <- data.frame(Id = seq(1, 100, 1),
                  Time = sample(1:12, 100, replace = TRUE),
                  Period = sample(c("AM", "PM"), 100, replace = TRUE),
                  Var1 = round(rnorm(100, mean = 0, sd = 1), 3))

df1$Var1 <- ifelse(df1$Var1 < -.5, NA, df1$Var1)

mylist <- prepare_smallset(
  data = df1,
  code = "other_code/paperFig.R",
  rowCount = 6,
)

fts <- highlight_changes(
  list = mylist,
  captionScript = "mycaptions",
  constant = constantC,
  changed = changedC,
  added = addedC,
  deleted = deletedC,
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



