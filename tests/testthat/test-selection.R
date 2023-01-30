test_that("rowCount less than rowNums returns error",
          {
            expect_error(select_smallset(
              data = s_data,
              rowCount = 5,
              rowNums = c(1, 2, 3, 4, 5, 6)
            ))
          })

smallset <-
  select_smallset(data = s_data,
                  rowCount = 5,
                  rowNums = c(4))
test_that("selection includes rowNum 4",
          {
            expect_true(4 %in% smallset)
          })
test_that("selection does not include rowNums twice",
          {
            expect_length(unique(smallset), 5)
          })

smallset <-
  select_smallset(
    data = s_data,
    rowCount = 5,
    rowNums = c(100, 99, 98, 97, 96)
  )
test_that("selection matches rowNums exactly",
          {
            expect_equal(smallset, c("96", "97", "98", "99", "100"))
          })
