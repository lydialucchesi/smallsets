test_that("alt text prints to console",
          {
            expect_output(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets"),
              altText = TRUE
            ))
          })

test_that("Smallset prints to console",
          {
            expect_output(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets"),
              rowReturn = TRUE
            ))
          })

test_that("example works",
          {
            expect_no_error(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets")
            ))
          })

test_that("ignoring C3 returns error",
          {
            expect_error(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets"),
              ignoreCols = c("C3")
            ))
          })
