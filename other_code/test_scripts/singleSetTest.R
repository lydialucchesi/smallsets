# library(stringr)
# library(gdata)
# library(plyr)
# library(dplyr)
# library(flextable)
# library(reshape2)
# library(ggplot2)
# library(ggforce)
# library(ggfittext)
# library(gplots)
# library(colorspace)
# library(patchwork)
# library(ggtext)

# source('~/Desktop/repos/smallset/R/write_smallset_code_original_1smallset.R')
# source('~/Desktop/repos/smallset/R/write_caption_template.R')
# source('~/Desktop/repos/smallset/R/select_smallset.R')
# source('~/Desktop/repos/smallset/R/read_captions_rmd.R')
# source('~/Desktop/repos/smallset/R/prepare_smallset.R')
# source('~/Desktop/repos/smallset/R/make_timeline_plot.R')
# source('~/Desktop/repos/smallset/R/highlight_changes2.R')
# source('~/Desktop/repos/smallset/R/get_timeline_dimensions.R')
# source('~/Desktop/repos/smallset/R/create_timeline.R')
# source('~/Desktop/repos/smallset/R/add_caption_block.R')

library(smallset)

constantC = list("cornsilk4", .4)
changedC = list("cornflowerblue", .4)
addedC = list("blueviolet", .4)
deletedC = list("darkgoldenrod1", .4)

df <- data.frame(
  V1 = c(2000,-1999, 2000, 1995, 1996, 2001, 2002, 2003, 2001, 1994, 2000, -1999),
  V2 = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  V3 = c(20, 11, 9, 18, 4, 15, 20, NA, 12, 13, 19, 6),
  V4 = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2)
)

# df <- as.data.table(df)

mylist <- prepare_smallset(
  data = df,
  code = "other_code/test_scripts/process_data2.2.R",
  rowCount = 6,
  rowNums = c(2, 5, 8)
)

fts <- highlight_changes(
  smallsetList = mylist,
  captionScript = "mycaptions",
  constant = constantC,
  changed = changedC,
  added = addedC,
  deleted = deletedC,
)

check <- create_timeline(
  snapshotList = fts,
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
  accentCols = "lighter",
  accentColsDif = 1,
  stampLoc = 5,
  timelineRows = 1,
  timelineFont = "mono",
  captionSpace = .8
)

check

