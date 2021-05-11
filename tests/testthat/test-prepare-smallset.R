library(magrittr)

df <- data.frame(
  V1 = c(
    2000,
    -1999,
    2000,
    1995,
    1996,
    2001,
    2002,
    2003,
    2001,
    1994,
    2000,
    -1999
  ),
  V2 = c(10, 5, 5, 9, NA, 8, 10, 10, 6, 7, NA, 3),
  V3 = c(20, 11, 9, 18, 4, 15, 20, NA, 12, 13, 19, 6),
  V4 = c(.6, .5, 1.1, .8, .7, 1.3, .9, 1.1, 1.4, .8, .9, 1.2)
)

codeLoc <- paste0(getwd(), "/process_data_test.R")

mylist <- prepare_smallset(
  data = df,
  code = codeLoc,
  rowCount = 6,
  rowNums = c(2, 5, 8)
)

rowNums <- c(2, 5, 8)
test_that("all specified rows were included",
          {
            expect_equal(c(TRUE, TRUE, TRUE),
                         rowNums %in%
                           row.names(mylist[[1]][[1]]$body$dataset))
          })

test_that("all columns were included",
          {
            expect_equal(colnames(df),
                         colnames(mylist[[1]][[1]]$body$dataset))
          })

test_that("preprocess will not run without needed column ignored by user",
          {
            expect_error(
              prepare_smallset(
                data = df,
                code = codeLoc,
                rowCount = 6,
                rowNums = c(2, 5, 8),
                ignoreCols = c("V4")
              )
            )
          })

mylist <- prepare_smallset(
  data = df,
  code = codeLoc,
  rowCount = 6,
  rowNums = c(2, 5, 8),
  ignoreCols = c("V1")
)

test_that("column was ignored",
          {
            expect_warning(colnames(df) ==
                             colnames(mylist[[1]][[1]]$body$dataset))
          })

mylist <- prepare_smallset(
  data = df,
  code = codeLoc,
  rowCount = 5,
  rowNums = c(1, 2, 3, 11, 12),
  ignoreCols = c("V1")
)

rowNums <- c(1, 2, 3, 11, 12)
test_that("row names match", {
  expect_equal(row.names(mylist[[1]][[1]]$body$dataset),
               as.character(rowNums))
})

test_that("prepare_smallset prints summary info",
          {
            expect_output(
              prepare_smallset(
                data = df,
                code = codeLoc,
                rowCount = 7,
                rowNums = c(4, 8)
              )
            )
          })

mylist <- prepare_smallset(
  data = df,
  code = codeLoc,
  rowCount = 6,
  rowNums = c(1, 2, 5, 8, 11, 12)
)

red <- mylist[[1]][[1]]$body$styles$text$color$data[3:5,]
red <- unique(as.vector(as.matrix(red)))
test_that("three rows are red", 
          {expect_true(red == "#FF0000")
            })

blue <- mylist[[1]][[2]]$body$styles$text$color$data[,c("V5")]
test_that("new column is blue", {
  expect_equal(blue, 
               c("#0000FF", "#0000FF", "#0000FF"))
})

green <- mylist[[1]][[2]]$body$styles$text$color$data[,c("V3")]
test_that("third column is green", {
  expect_equal(green, 
               c("#008000", "#008000", "#008000"))
})

blue <- mylist[[1]][[3]]$body$styles$text$color$data[,c("V6")]
test_that("new column is blue", {
  expect_equal(blue, 
               c("#0000FF", "#0000FF", "#0000FF"))
})

test_that("test that class was set", 
          {expect_s3_class(mylist, "smallsetSnapshots")})
