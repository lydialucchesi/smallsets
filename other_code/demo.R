library(ggpubr)
library(cowplot)
library(smallset)

source("other_code/gen_data.R")

mylist <- prep_smallset(data = df, 
                        prepCode = "other_code/prep_data.R", 
                        rowCount = 6, 
                        rowNums = c(1, 2, 5))

fts <- highlight_changes(list = mylist, 
                         captionScript = "mycaptions", 
                         constant = "seashell4", 
                         changed = "darkorchid1", 
                         added = "darkorange", 
                         deleted = "goldenrod1")

check <- abstract_timeline(ftsList = fts, sizing =
                             list(
                               "columns" = 2,
                               "tiles" = 1,
                               "captions" = 10
                             ))

plot_grid(plotlist = check)



# need to figure out error in highlight_changes
# related to dropping rows and then trying to change colour of rows later on
mylist <-
  prep_smallset(
    data = df,
    prepCode = "other_code/prep_data2.R",
    rowCount = 6
  )

fts <- highlight_changes(list = mylist, 
                         captionScript = "mycaptions", 
                         constant = "seashell4", 
                         changed = "darkorchid1", 
                         added = "darkorange", 
                         deleted = "goldenrod1")

check <- abstract_timeline(ftsList = fts)
plot_grid(plotlist = check, nrow = 1)







