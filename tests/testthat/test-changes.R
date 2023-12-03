smallsetList <-
  list(data.frame(
    C1 = c(1, 2, 3),
    C2 = c(4, 5, 6),
    C3 = c(7, 8, 9)
  ),
  data.frame(
    C1 = c(1, 1),
    C2 = c(4, 5),
    C3 = c(7, 8),
    C4 = c(10, 11)
  ))
dataChanges <-
  find_data_changes(smallsetList,
                    c("#CDAFEE", "#FBE49D",  "#B4D5F5", "#E6E4DF"),
                    FALSE)

test_that("data deletion was found",
          {
            expect_equal(unique(dataChanges[[1]][[1]]$body$styles$text$color$data[3, ]),
                         "#FBE49D")
          })


test_that("data edit was found",
          {
            expect_equal(unique(dataChanges[[1]][[2]]$body$styles$text$color$data[2, "C1"]),
                         "#B4D5F5")
          })


test_that("data addition was found",
          {
            expect_equal(unique(dataChanges[[1]][[2]]$body$styles$text$color$data[, "C4"]),
                         "#CDAFEE")
          })
