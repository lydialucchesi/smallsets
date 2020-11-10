library(smallset)

source("other_code/gen_data.R")

mylist <- prep_smallset(data = df,
                        prepCode = "other_code/prep_data.R",
                        rowCount = 6,
                        rowNums = c(2, 5, 8))

fts <- highlight_changes(list = mylist,
                         captionScript = "mycaptions",
                         constant = "cornsilk4",
                         changed = "cornflowerblue",
                         added = "blueviolet",
                         deleted = "darkgoldenrod1",
                         author = "Lydia")

check <- create_timeline(ftsList = fts, 
                         abstract = FALSE,
                         sizing =
                             list(
                               "columns" = 2.5,
                               "tiles" = .8,
                               "captions" = 8,
                               "symbols" = 1.5,
                               "circles" = .1,
                               "data" = 2.5
                             ),
                           accentCols = "darker",
                           accentColsDif = .7,
                           stampLoc = 1,
                           timelineRows = 1,
                           timelineFont = "Palatino")
check

# mylist <-
#   prep_smallset(
#     data = df,
#     prepCode = "other_code/prep_data2.R",
#     rowCount = 10
#   )
# 
# fts <- highlight_changes(list = mylist,
#                          captionScript = "mycaptions",
#                          constant = "#aab40a",
#                          changed = "#3d3a3e",
#                          added = "#ff00f4",
#                          deleted = "#0a6bb4")
# 
# check <- create_timeline(ftsList = fts)
# check



