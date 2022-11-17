#' Generate Alt Text
#' @description The function produces alt text for the Smallset Timeline figure.
#' @keywords internal
#' @import "brew" "knitr" "readr" "english"
#' @importFrom plotrix color.id

generate_alt_text <-
  function(snapshotList,
           altTextInfo,
           l,
           printedData,
           ghostData) {
    suppressWarnings(dir.create("altText"))
  
    suppressWarnings(brew(file = system.file("altTextTemplates", "snapNum.txt", package="smallsets"), output = "altText/intro_3.txt"))
    
    if (("changed1" %in% row.names(snapshotList[[9]])) | (snapshotList[[6]] %in% snapshotList[[9]]$colValue)) {
      editColour <- sapply(snapshotList[[6]], color.id)[1]
      suppressWarnings(brew(file = system.file("altTextTemplates", "editColour.txt", package="smallsets"), output = "altText/intro_4.txt"))
    }
    
    if (("added1" %in% row.names(snapshotList[[9]])) | (snapshotList[[7]] %in% snapshotList[[9]]$colValue)) {
      addColour <- sapply(snapshotList[[7]], color.id)[1]
      suppressWarnings(brew(file = system.file("altTextTemplates", "addColour.txt", package="smallsets"), output = "altText/intro_5.txt"))
    }
    
    if (("deleted1" %in% row.names(snapshotList[[9]])) | (snapshotList[[8]] %in% snapshotList[[9]]$colValue)) {
      delColour <- sapply(snapshotList[[8]], color.id)[1]
      suppressWarnings(brew(file = system.file("altTextTemplates", "delColour.txt", package="smallsets"), output = "altText/intro_6.txt"))
    }
    
    snaps <- grep("intro", list.files("altText"), value = TRUE)
    snaps <-
      sort(as.numeric(as.character(gsub(
        "[^0-9-]", "", snaps
      ))))
    
    at <- c()
    for (s in snaps) {
      atNew <-
        readr::read_file(paste0("altText/intro_", as.character(s), ".txt"))
      at <- paste(at, atNew)
    }
    at <- str_replace_all(at, "[\r\n]" , "")
    
    fileConn <-
      file("altText/intro.txt")
    writeLines(at, fileConn)
    close(fileConn)
    
    for (s in snaps) {
      file.remove(paste0("altText/intro_", as.character(s), ".txt"))
    }
    
    for (i in 1:length(snapshotList[[1]])) {
      
      suppressWarnings(brew(file = system.file("altTextTemplates", "snapDim.txt", package="smallsets"), output = "altText/body_1.txt"))
      
      if (i == 1) {
        suppressWarnings(brew(file = system.file("altTextTemplates", "colNames.txt", package="smallsets"), output = "altText/body_2.txt"))
        
        if (length(altTextInfo$rowsDrop) != 0) {
          if (length(altTextInfo$rowsDrop) > 1) {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "delRows.txt", package="smallsets"), output = "altText/body_3.txt")
            )
            altTextInfo$rowsDrop <- NULL
          } else {
            suppressWarnings(brew(file = system.file("altTextTemplates", "delRow.txt", package="smallsets"), output = "altText/body_4.txt"))
            altTextInfo$rowsDrop <- NULL
          }
        } else {
          altTextInfo$rowsDrop <- NULL
        }
        
        if (length(altTextInfo$colsDrop) != 0) {
          if (length(altTextInfo$colsDrop) > 1) {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "delColumns.txt", package="smallsets"), output = "altText/body_5.txt")
            )
            altTextInfo$colsDrop <- NULL
          } else {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "delColumn.txt", package="smallsets"), output = "altText/body_6.txt")
            )
            altTextInfo$colsDrop <- NULL
          }
        } else {
          altTextInfo$colsDrop <- NULL
        }
        
        if (isFALSE(printedData) & isTRUE(ghostData)) {
          if (layer_data(l[[i]], 3)$label != "") {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "snapCaption3.txt", package="smallsets"), output = "altText/body_13.txt")
            )
          }
        } else if (isTRUE(printedData) & isFALSE(ghostData)) {
          if (layer_data(l[[i]], 5)$label != "") {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "snapCaption5.txt", package="smallsets"), output = "altText/body_13.txt")
            )
          }
        } else {
          if (layer_data(l[[i]], 4)$label != "") {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "snapCaption4.txt", package="smallsets"), output = "altText/body_13.txt")
            )
          }
        }
        
        snaps <- grep("body", list.files("altText"), value = TRUE)
        snaps <-
          sort(as.numeric(as.character(gsub(
            "[^0-9-]", "", snaps
          ))))
        
        at <- c()
        for (s in snaps) {
          atNew <- readr::read_file(paste0("altText/body_", as.character(s), ".txt"))
          at <- paste(at, atNew)
        }
        at <- str_replace_all(at, "[\r\n]" , "")
        
        fileConn <-
          file(paste0("altText/snap", as.character(i), ".txt"))
        writeLines(at, fileConn)
        close(fileConn)
        
        for (s in snaps) {
          file.remove(paste0("altText/body_", as.character(s), ".txt"))
        }
        
      } else {
        if (length(altTextInfo$rowsDrop) != 0) {
          if (length(altTextInfo$rowsDrop) > 1) {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "delRows.txt", package="smallsets"), output = "altText/body_3.txt")
            )
            altTextInfo$rowsDrop <- NULL
          } else {
            suppressWarnings(brew(file = system.file("altTextTemplates", "delRow.txt", package="smallsets"), output = "altText/body_4.txt"))
            altTextInfo$rowsDrop <- NULL
          }
        } else {
          altTextInfo$rowsDrop <- NULL
        }
        
        if (length(altTextInfo$colsDrop) != 0) {
          if (length(altTextInfo$colsDrop) > 1) {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "delColumns.txt", package="smallsets"), output = "altText/body_5.txt")
            )
            altTextInfo$colsDrop <- NULL
          } else {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "delColumn.txt", package="smallsets"), output = "altText/body_6.txt")
            )
            altTextInfo$colsDrop <- NULL
          }
        } else {
          altTextInfo$colsDrop <- NULL
        }
        
        if (nrow(altTextInfo$adjData) > 0) {
          if (nrow(altTextInfo$adjData) > 1) {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "editCells.txt", package="smallsets"), output = "altText/body_7.txt")
            )
            altTextInfo$adjData <- NULL
          } else {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "editCell.txt", package="smallsets"), output = "altText/body_8.txt")
            )
            altTextInfo$adjData <- NULL
          }
        } else {
          altTextInfo$adjData <- NULL
        }
        
        if (length(altTextInfo$rowsAdd) != 0) {
          if (length(altTextInfo$rowsAdd) > 1) {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "addRows.txt", package="smallsets"), output = "altText/body_9.txt")
            )
            altTextInfo$rowsAdd <- NULL
          } else {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "addRow.txt", package="smallsets"), output = "altText/body_10.txt")
            )
            altTextInfo$rowsAdd <- NULL
          }
        } else {
          altTextInfo$rowsAdd <- NULL
        }
        
        if (length(altTextInfo$colsAdd) != 0) {
          if (length(altTextInfo$colsAdd) > 1) {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "addColumns.txt", package="smallsets"), output = "altText/body_11.txt")
            )
            altTextInfo$colsAdd <- NULL
          } else {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "addColumn.txt", package="smallsets"), output = "altText/body_12.txt")
            )
            altTextInfo$colsAdd <- NULL
          }
        } else {
          altTextInfo$colsAdd <- NULL
        }
        
        if (isFALSE(printedData) & isTRUE(ghostData)) {
          if (layer_data(l[[i]], 3)$label != "") {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "snapCaption3.txt", package="smallsets"), output = "altText/body_13.txt")
            )
          }
        } else if (isTRUE(printedData) & isFALSE(ghostData)) {
          if (layer_data(l[[i]], 5)$label != "") {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "snapCaption5.txt", package="smallsets"), output = "altText/body_13.txt")
            )
          }
        } else {
          if (layer_data(l[[i]], 4)$label != "") {
            suppressWarnings(
              brew(file = system.file("altTextTemplates", "snapCaption4.txt", package="smallsets"), output = "altText/body_13.txt")
            )
          }
        }

        
        snaps <- grep("body", list.files("altText"), value = TRUE)
        snaps <-
          sort(as.numeric(as.character(gsub(
            "[^0-9-]", "", snaps
          ))))
        
        at <- c()
        for (s in snaps) {
          atNew <- readr::read_file(paste0("altText/body_", as.character(s), ".txt"))
          at <- paste(at, atNew)
        }
        at <- str_replace_all(at, "[\r\n]" , "")
        
        fileConn <-
          file(paste0("altText/snap", as.character(i), ".txt"))
        writeLines(at, fileConn)
        close(fileConn)
        
        for (s in snaps) {
          file.remove(paste0("altText/body_", as.character(s), ".txt"))
        }
        
      }
      
    }
    
    txtFiles <- list.files("altText")
    
    altText <- c()
    for (i in 1:length(txtFiles)) {
      newPart <- readr::read_file(paste0("altText/", txtFiles[i]))
      altText <- paste0(altText, newPart)
    }
    
    fileConn <- file("figureAltText.txt")
    writeLines(altText, fileConn)
    close(fileConn)
    
    print("Alt text available in figureAltText.txt")
    
    unlink("altText", recursive = TRUE)
    
  }
