library(ggpubr)

source("gen_data.R")
source("apply_smallset_code.R")
source("highlight_changes.R")
source("abstract_it.R")


mylist <-
  prep_smallset(
    data = df,
    prepCode = "prep_data.R",
    rowCount = 6,
    rowNums = c(1, 2, 5)
  )

fts <- highlight_changes(list = mylist, captionScript = "mycaptions")
check <- abstract_it(ftsList = fts)
ggarrange(check[[1]], check[[2]], check[[3]], check[[4]], check[[5]], check[[6]],
          nrow = 1)
