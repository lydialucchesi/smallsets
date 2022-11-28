#' Generate alt text
#' @description Produces alternative text (alt text) for the Smallset Timeline figure.
#' @keywords internal
#' @import "brew"
#' @importFrom plotrix color.id

generate_alt_text <-
  function(tables,
           fourCols,
           legendDF,
           altTextInfo,
           l,
           printedData,
           ghostData) {
    # Create temporary folder to hold text files
    dir.create("altText")
    
    # Write how many snapshots
    brew(
      file = system.file("altTextTemplates", "snapNum.txt", package = "smallsets"),
      output = "altText/intro_3.txt"
    )
    
    # Describe colour legend
    if (sum(grepl("Data has been edited.", legendDF)) == 1) {
      editColour <- sapply(fourCols[2], color.id)[1]
      brew(
        file = system.file("altTextTemplates", "editColour.txt", package = "smallsets"),
        output = "altText/intro_4.txt"
      )
    }
    if (sum(grepl("Data has been added.", legendDF)) == 1) {
      addColour <- sapply(fourCols[3], color.id)[1]
      brew(
        file = system.file("altTextTemplates", "addColour.txt", package = "smallsets"),
        output = "altText/intro_5.txt"
      )
    }
    if (sum(grepl("Data will be deleted.", legendDF)) == 1) {
      delColour <- sapply(fourCols[4], color.id)[1]
      brew(
        file = system.file("altTextTemplates", "delColour.txt", package = "smallsets"),
        output = "altText/intro_6.txt"
      )
    }
    
    # Compile introduction text
    txtFiles <- grep("intro", list.files("altText"), value = TRUE)
    txtFiles <-
      sort(as.numeric(as.character(gsub(
        "[^0-9-]", "", txtFiles
      ))))
    at <- c()
    for (t in txtFiles) {
      path <- paste0("altText/intro_", as.character(t), ".txt")
      at <- paste(at, utils::read.delim(path, header = FALSE)[1, 1])
      file.remove(path)
    }
    # Write alt text introduction file
    fileConn <-
      file("altText/intro.txt")
    writeLines(at, fileConn)
    close(fileConn)
    
    # Loop through snapshots and describe each one
    for (i in 1:length(tables)) {
      brew(
        file = system.file("altTextTemplates", "snapDim.txt", package = "smallsets"),
        output = "altText/body_1.txt"
      )
      
      if (i == 1) {
        # List out Smallset column names
        brew(
          file = system.file("altTextTemplates", "colNames.txt", package = "smallsets"),
          output = "altText/body_2.txt"
        )
      }
      
      # Describe any row deletions
      if (length(altTextInfo$rowsDrop) != 0) {
        if (length(altTextInfo$rowsDrop) > 1) {
          brew(
            file = system.file("altTextTemplates", "delRows.txt", package = "smallsets"),
            output = "altText/body_3.txt"
          )
          altTextInfo$rowsDrop <- NULL
        } else {
          brew(
            file = system.file("altTextTemplates", "delRow.txt", package = "smallsets"),
            output = "altText/body_4.txt"
          )
          altTextInfo$rowsDrop <- NULL
        }
      } else {
        altTextInfo$rowsDrop <- NULL
      }
      
      # Describe any column deletions
      if (length(altTextInfo$colsDrop) != 0) {
        if (length(altTextInfo$colsDrop) > 1) {
          brew(
            file = system.file("altTextTemplates", "delColumns.txt", package = "smallsets"),
            output = "altText/body_5.txt"
          )
          altTextInfo$colsDrop <- NULL
        } else {
          brew(
            file = system.file("altTextTemplates", "delColumn.txt", package = "smallsets"),
            output = "altText/body_6.txt"
          )
          altTextInfo$colsDrop <- NULL
        }
      } else {
        altTextInfo$colsDrop <- NULL
      }
      
      if (i > 1) {
        # Describe any data edits
        if (nrow(altTextInfo$adjData) > 0) {
          if (nrow(altTextInfo$adjData) > 1) {
            brew(
              file = system.file("altTextTemplates", "editCells.txt", package = "smallsets"),
              output = "altText/body_7.txt"
            )
            altTextInfo$adjData <- NULL
          } else {
            brew(
              file = system.file("altTextTemplates", "editCell.txt", package = "smallsets"),
              output = "altText/body_8.txt"
            )
            altTextInfo$adjData <- NULL
          }
        } else {
          altTextInfo$adjData <- NULL
        }
        
        # Describe any row additions
        if (length(altTextInfo$rowsAdd) != 0) {
          if (length(altTextInfo$rowsAdd) > 1) {
            brew(
              file = system.file("altTextTemplates", "addRows.txt", package = "smallsets"),
              output = "altText/body_9.txt"
            )
            altTextInfo$rowsAdd <- NULL
          } else {
            brew(
              file = system.file("altTextTemplates", "addRow.txt", package = "smallsets"),
              output = "altText/body_10.txt"
            )
            altTextInfo$rowsAdd <- NULL
          }
        } else {
          altTextInfo$rowsAdd <- NULL
        }
        
        # Describe any column additions
        if (length(altTextInfo$colsAdd) != 0) {
          if (length(altTextInfo$colsAdd) > 1) {
            brew(
              file = system.file("altTextTemplates", "addColumns.txt", package = "smallsets"),
              output = "altText/body_11.txt"
            )
            altTextInfo$colsAdd <- NULL
          } else {
            brew(
              file = system.file("altTextTemplates", "addColumn.txt", package = "smallsets"),
              output = "altText/body_12.txt"
            )
            altTextInfo$colsAdd <- NULL
          }
        } else {
          altTextInfo$colsAdd <- NULL
        }
      }
      
      # Write the snapshot caption
      if (isFALSE(printedData) & isTRUE(ghostData)) {
        if (layer_data(l[[i]], 3)$label != "") {
          brew(
            file = system.file("altTextTemplates", "snapCaption3.txt", package = "smallsets"),
            output = "altText/body_13.txt"
          )
        }
      } else if (isTRUE(printedData) & isFALSE(ghostData)) {
        if (layer_data(l[[i]], 5)$label != "") {
          brew(
            file = system.file("altTextTemplates", "snapCaption5.txt", package = "smallsets"),
            output = "altText/body_13.txt"
          )
        }
      } else {
        if (layer_data(l[[i]], 4)$label != "") {
          brew(
            file = system.file("altTextTemplates", "snapCaption4.txt", package = "smallsets"),
            output = "altText/body_13.txt"
          )
        }
      }
      
      # Compile snapshot description text
      txtFiles <- grep("body", list.files("altText"), value = TRUE)
      txtFiles <-
        sort(as.numeric(as.character(gsub(
          "[^0-9-]", "", txtFiles
        ))))
      at <- c()
      for (t in txtFiles) {
        path <- paste0("altText/body_", as.character(t), ".txt")
        at <-
          paste(at, utils::read.delim(path, header = FALSE)[1, 1])
        file.remove(path)
      }
      # Write text file for snapshot
      fileConn <-
        file(paste0("altText/snap", as.character(i), ".txt"))
      writeLines(at, fileConn)
      close(fileConn)
    }
    
    # Compile introduction and snapshot files
    txtFiles <- list.files("altText")
    altText <- c()
    for (i in 1:length(txtFiles)) {
      newPart <-
        utils::read.delim(paste0("altText/", txtFiles[i]), header = FALSE)[1, 1]
      altText <- paste0(altText, newPart)
    }
    altText <- trimws(altText)
    # Write completed alt text to working directory
    fileConn <- file("figureAltText.txt")
    writeLines(altText, fileConn)
    close(fileConn)
    print("Alt text available in figureAltText.txt")
    
    # Delete temporary storage folder
    unlink("altText", recursive = TRUE)
  }
