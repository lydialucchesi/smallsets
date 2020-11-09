library(cowplot)
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

# do a create_timeline() function
# that has an abstract argument
# if it is true, then run this function below
check <- abstract_timeline(ftsList = fts, sizing =
                             list(
                               "columns" = 2,
                               "tiles" = 1,
                               "captions" = 6,
                               "symbols" = 2
                             ),
                           stampColsDif = .5,
                           stampLoc = 5,
                           timelineRows = 2)

check

# need to figure out error in highlight_changes
# related to dropping rows and then trying to change colour of rows later on
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

check <- abstract_timeline(ftsList = fts)
check







