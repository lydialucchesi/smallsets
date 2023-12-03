test_that("rowCount less than rowIDs returns error",
          {
            expect_error(select_smallset(
              data = s_data,
              rowCount = 5,
              rowIDs = c("1", "2", "3", "4", "5", "6")
            ))
          })


test_that("selection includes rowID 4",
          {
            expect_true("4" %in% select_smallset(
              data = s_data,
              rowCount = 5,
              rowIDs = c("4"),
              lang = "R"
            ))
          })


test_that("selection does not include rowIDs twice",
          {
            expect_length(unique(
              select_smallset(
                data = s_data,
                rowCount = 5,
                rowIDs = c("4"),
                lang = "R"
              )
            ), 5)
          })


test_that("selection matches rowIDs exactly",
          {
            expect_equal(
              select_smallset(
                data = s_data,
                rowCount = 5,
                rowIDs = c("100", "99", "98", "97", "96"),
                lang = "R"
              ),
              c("96", "97", "98", "99", "100")
            )
          })


# Set up
source(write_smallset_code(
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  smallset = "allROWS",
  lang = "R"
)[[3]],
local = TRUE)
indicator <-
  prepare_score_sheet(
    smallsetList = apply_code(s_data),
    fourCols = unlist(return_scheme(1), use.names = FALSE)
  )
# Test
test_that("coverage indicator matrix is correct",
          {
            expect_setequal(c(sum(indicator$s1), sum(indicator$s2), sum(indicator$s3)), c(10, 90, 90))
          })


# Set up
source(write_smallset_code(
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  smallset = "allROWS",
  lang = "R"
)[[3]],
local = TRUE)
appearance <-
  prepare_colour_sheet(
    smallsetList = apply_code(s_data),
    fourCols = unlist(return_scheme(1), use.names = FALSE),
    ignoreCols = NULL
  )
# Test
test_that("visual appearance matrix is correct",
          {
            expect_setequal(c(length(unique(appearance$C2)),
                              length(unique(appearance$C6)),
                              length(unique(appearance$C8))), c(2, 3, 3))
          })
# Test
test_that("C9 has purple in visual appearance matrix",
          {
            expect_true("#B385E5" %in% appearance$C9)
          })
# Test
test_that("C7 is all yellow in visual appearance matrix",
          {
            expect_true(unique(appearance$C7) == "#F9D76C")
          })
