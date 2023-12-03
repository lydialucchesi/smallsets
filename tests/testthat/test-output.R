test_that("alt text prints to console",
          {
            expect_output(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets"),
              altText = TRUE
            ))
          })


test_that("alt text prints to console when there is a resume marker",
          {
            expect_output(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess_resume.R", package = "smallsets"),
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


test_that("R example works",
          {
            expect_no_error(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets")
            ))
          })


test_that("R Markdown example works",
          {
            expect_no_error(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.Rmd", package = "smallsets")
            ))
          })


test_that("block comments work",
          {
            expect_length(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess_block.R", package = "smallsets")
            ), 3)
          })


test_that("resume marker works",
          {
            expect_length(nrow(layer_data(
              Smallset_Timeline(
                data = s_data,
                code = system.file("s_data_preprocess_resume.R", package = "smallsets")
              )[[4]]
            )), 1)
          })


test_that("ignoring C5, an unaffected column, works",
          {
            expect_no_error(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets"),
              ignoreCols = c("C5")
            ))
          })


test_that("sizing, spacing, and labelling work",
          {
            expect_no_error(
              Smallset_Timeline(
                data = s_data,
                code = system.file("s_data_preprocess.R", package = "smallsets"),
                sizing = sets_sizing(
                  captions = 5,
                  tiles = .3,
                  legend = 10,
                  icons = 3,
                  columns = .5
                ),
                spacing = sets_spacing(
                  degree = 45,
                  header = 2,
                  right = 2,
                  captions = 10,
                  rows = 3
                ),
                labelling = sets_labelling(labelColDif = 1, labelCol = "lighter")
              )
            )
          })


test_that("4 passed to colours returns error",
          {
            expect_error(return_scheme(4))
          })


test_that("2 passed to colours returns a list",
          {
            expect_type(return_scheme(2), "list")
          })


test_that("vertical alignment works",
          {
            expect_gt(
              ggplot_build(
                Smallset_Timeline(
                  data = s_data,
                  code = system.file("s_data_preprocess.R", package = "smallsets"),
                  align = "vertical"
                )[[1]]
              )$data[[3]]$x,
              ggplot_build(Smallset_Timeline(
                data = s_data,
                code = system.file("s_data_preprocess.R", package = "smallsets")
              )[[1]])$data[[3]]$x
            )
          })
