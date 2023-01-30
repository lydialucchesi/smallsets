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
