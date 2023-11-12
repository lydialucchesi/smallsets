output <-
  write_smallset_code(
    code = system.file("s_data_preprocess.R", package = "smallsets"),
    smallset = c("1", "2", "3", "4", "5"),
    lang = "R"
  )
source(output[[3]], local = TRUE)
smallsetList <- apply_code(s_data)
unlink(output[[3]])
test_that("three snapshots taken",
          {
            expect_length(smallsetList, 3)
          })

output <-
  write_smallset_code(
    code = system.file("s_data_preprocess_resume.R", package = "smallsets"),
    smallset = c("1", "2", "3", "4", "5"),
    lang = "R"
  )
source(output[[3]], local = TRUE)
smallsetList <- apply_code(s_data)
unlink(output[[3]])
test_that("four snapshots taken",
          {
            expect_length(smallsetList, 4)
          })

output <- write_smallset_code(code = system.file("s_data_preprocess.R", package = "smallsets"), 
                              smallset = "allROWS", 
                              lang = "R")
source(output[[3]], local = TRUE)
smallsetList <- apply_code(s_data)
unlink(output[[3]])
fourCols <- unlist(return_scheme(1), use.names = FALSE)
indicator <- prepare_score_sheet(smallsetList = smallsetList, fourCols = fourCols)
sums <- c(sum(indicator$s1), sum(indicator$s2), sum(indicator$s3))
test_that("coverage indicator matrix is correct",
          {
            expect_setequal(sums, c(10, 90, 90))
          })

output <- write_smallset_code(code = system.file("s_data_preprocess.R", package = "smallsets"), 
                              smallset = "allROWS", 
                              lang = "R")
source(output[[3]], local = TRUE)
smallsetList <- apply_code(s_data)
unlink(output[[3]])
fourCols <- unlist(return_scheme(1), use.names = FALSE)
appearance <- prepare_colour_sheet(smallsetList = smallsetList, fourCols = fourCols)
checks <- c(length(unique(appearance$C2)), 
            length(unique(appearance$C6)), 
            length(unique(appearance$C8)))
test_that("visual appearance matrix is correct",
          {
            expect_setequal(checks, c(2, 3, 3))
          })

test_that("C9 has purple in visual appearance matrix",
          {
            expect_true("#B385E5" %in% appearance$C9)
          })

test_that("C7 is all yellow in visual appearance matrix",
          {
            expect_true(unique(appearance$C7) == "#F9D76C")
          })

output <-
  write_smallset_code(
    code = system.file("s_data_preprocess.py", package = "smallsets"),
    smallset = c("1", "2", "3", "4", "5"),
    lang = "py"
  )
py_code <- readLines(output[[3]])
unlink(output[[3]])
test_that("python snapshot-taking function is right length",
          {
            expect_length(py_code, 21)
          })
