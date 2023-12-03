test_that("ghost data as false works",
          {
            expect_setequal(sum(unique(layer_data(
              Smallset_Timeline(
                data = s_data,
                code = system.file("s_data_preprocess.R", package = "smallsets"),
                rowIDs = c("1", "2", "3", "4", "5"),
                ghostData = FALSE
              )[[2]]
            ))$y >= 3), 24)
          })


test_that("ghost data plot has more coords",
          {
            expect_gt(nrow(layer_data(
              Smallset_Timeline(
                data = s_data,
                code = system.file("s_data_preprocess.R", package = "smallsets"),
                rowIDs = c("1", "2", "3", "4", "5")
              )[[2]]
            )),
            nrow(layer_data(
              Smallset_Timeline(
                data = s_data,
                code = system.file("s_data_preprocess.R", package = "smallsets"),
                rowIDs = c("1", "2", "3", "4", "5"),
                ghostData = FALSE
              )[[2]]
            )))
          })


test_that("printed data enrichment feature works",
          {
            expect_setequal(ggplot_build(
              Smallset_Timeline(
                data = s_data,
                code = system.file("s_data_preprocess.R", package = "smallsets"),
                rowIDs = c("1", "2", "3", "4", "5"),
                printedData = TRUE,
                missingDataTints = TRUE
              )
            )$data[[3]]$label[1:3],
            c("147", "192", "232"))
          })


test_that("legend was updated for missing data tints",
          {
            expect_true(
              ggplot_build(
                Smallset_Timeline(
                  data = s_data,
                  code = system.file("s_data_preprocess.R", package = "smallsets"),
                  rowIDs = c("1", "2", "3", "4", "5"),
                  printedData = TRUE,
                  missingDataTints = TRUE
                )
              )$plot$scales$scales[[1]]$get_labels()[2] == "Deleted*  "
            )
          })


test_that("missing data are highlighted",
          {
            expect_true(length(unique(
              ggplot_build(
                Smallset_Timeline(
                  data = s_data,
                  code = system.file("s_data_preprocess.R", package = "smallsets"),
                  rowIDs = c("1", "2", "3", "4", "5"),
                  printedData = TRUE,
                  missingDataTints = TRUE
                )[[1]]
              )[[1]][[1]]$fill
            )) == 4)
          })
