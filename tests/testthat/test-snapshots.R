test_that("python snapshot-taking function is right length",
          {
            expect_length(readLines(
              write_smallset_code(
                code = system.file("s_data_preprocess.py", package = "smallsets"),
                smallset = c("1", "2", "3", "4", "5"),
                lang = "py"
              )[[3]]
            ), 21)
          })


# Set up
source(write_smallset_code(
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  smallset = c("1", "2", "3", "4", "5"),
  lang = "R"
)[[3]],
local = TRUE)
# Test
test_that("three snapshots taken",
          {
            expect_length(apply_code(s_data), 3)
          })


# Set up
source(write_smallset_code(
  code = system.file("s_data_preprocess_resume.R", package = "smallsets"),
  smallset = c("1", "2", "3", "4", "5"),
  lang = "R"
)[[3]],
local = TRUE)
# Test
test_that("four snapshots taken",
          {
            expect_length(apply_code(s_data), 4)
          })
