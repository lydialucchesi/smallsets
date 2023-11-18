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

ST <- Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess_block.R", package = "smallsets")
)
test_that("block comments work",
          {
            expect_length(ST, 3)
          })

ST <- Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess_resume.R", package = "smallsets")
)
r <- nrow(layer_data(ST[[4]]))
test_that("resume marker works",
          {
            expect_length(r, 1)
          })

test_that("ignoring C5, an unaffected column, works",
          {
            expect_no_error(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets"),
              ignoreCols = c("C5")
            ))
          })

ST1 <- Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  rowNums = c(1, 2, 3, 4, 5),
  ghostData = FALSE
)
d1 <- layer_data(ST1[[2]])
test_that("there are no coords where rows were deleted",
          {
            expect_setequal(sum(c(1 %in% d1$y, 2 %in% d1$y)), 0)
          })

ST2 <- Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  rowNums = c(1, 2, 3, 4, 5)
)
d2 <- layer_data(ST2[[2]])
test_that("ghost data plot has more coords",
          {
            expect_gt(nrow(d2), nrow(d1))
          })

test_that("4 passed to colours returns error",
          {
            expect_error(return_scheme(4))
          })

test_that("2 passed to colours returns a list",
          {
            expect_type(return_scheme(2), "list")
          })

ST <- Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  rowNums = c(1, 2, 3, 4, 5),
  printedData = TRUE,
  missingDataTints = TRUE
)
ST_geoms <- ggplot_build(ST)
printed <- ST_geoms$data[[3]]$label[1:3]
test_that("printed data enrichment feature works",
          {
            expect_setequal(printed, c("147", "192", "232"))
          })

labels <- ST_geoms$plot$scales$scales[[1]]$get_labels()
test_that("legend was updated for missing data tints",
          {
            expect_true(labels[2] == "Deleted*  ")
          })

ST_geom1 <- ggplot_build(ST[[1]])[[1]][[1]]
test_that("missing data are highlighted",
          {
            expect_true(length(unique(ST_geom1$fill)) == 4)
          })

test_that("sizing, spacing, and labelling work",
          {
            expect_no_error(Smallset_Timeline(
              data = s_data,
              code = system.file("s_data_preprocess.R", package = "smallsets"),
              sizing = sets_sizing(captions = 5, tiles = .3, legend = 10, icons = 3, columns = .5),
              spacing = sets_spacing(degree = 45, header = 2, right = 2, captions = 10, rows = 3),
              labelling = sets_labelling(labelColDif = 1, labelCol = "lighter")
            ))
          })

STv <- Smallset_Timeline(data = s_data,
                        code = system.file("s_data_preprocess.R", package = "smallsets"),
                        align = "vertical")
v <- ggplot_build(STv[[1]])$data[[3]]$x
STh <- Smallset_Timeline(data = s_data,
                         code = system.file("s_data_preprocess.R", package = "smallsets"))
h <- ggplot_build(STh[[1]])$data[[3]]$x
test_that("vertical alignment works",
          {
            expect_gt(v, h)
          })

