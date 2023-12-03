#' Write add colour
#' @description Writes about the add colour.
#' @noRd
write_colourAdded <- function(colourAdded) {
  txt <- paste0("Data additions are ",
                colourAdded,
                ".")
  return(txt)
}

#' Write delete colour
#' @description Writes about the delete colour.
#' @noRd
write_colourDeleted <- function(colourDeleted) {
  txt <- paste0("Data deletions are ",
                colourDeleted,
                ".")
  return(txt)
}

#' Write edit colour
#' @description Writes about the edit colour.
#' @noRd
write_colourEdited <- function(colourEdited) {
  txt <- paste0("Data edits are ",
                colourEdited,
                ".")
  return(txt)
}

#' Write unchanged colour
#' @description Writes about the unchanged colour.
#' @noRd
write_colourUnchanged <- function(colourUnchanged) {
  txt <- paste0("Unchanged data is ",
                colourUnchanged,
                ".")
  return(txt)
}

#' Write column additions
#' @description Writes about column additions.
#' @noRd
write_columnsAdd <- function(columnsAdd, colourAdd) {
  if (length(columnsAdd) == 1) {
    txt <- paste0("Column ",
                  columnsAdd,
                  " is ",
                  colourAdd,
                  ".")
  } else if (length(columnsAdd) == 2) {
    txt <- paste0("Columns ",
                  columnsAdd[1],
                  " and ",
                  columnsAdd[2],
                  " are ",
                  colourAdd,
                  ".")
  } else {
    txt <- paste0(
      "Columns ",
      paste(columnsAdd[1:(length(columnsAdd) - 1)], collapse = ", "),
      ", and ",
      columnsAdd[length(columnsAdd)],
      " are ",
      colourAdd,
      "."
    )
  }
  return(txt)
}

#' Write row additions
#' @description Writes about row additions.
#' @noRd
write_rowsAdd <- function(rowsAdd, colourAdd) {
  if (length(rowsAdd) == 1) {
    txt <- paste0(as.character(length(rowsAdd)),
                  " row is ",
                  colourAdd,
                  ".")
  } else {
    txt <- paste0(as.character(length(rowsAdd)),
                  " rows are ",
                  colourAdd,
                  ".")
  }
  return(txt)
}

#' Write column names
#' @description Writes about the column names.
#' @noRd
write_columnNames <- function(columnNames) {
  if (length(columnNames) == 2) {
    txt <- paste0(
      "The columns, in order from left to right, are ",
      columnNames[1],
      " and ",
      columnNames[2],
      "."
    )
  } else {
    txt <- paste0(
      "The columns, in order from left to right, are ",
      paste(columnNames[1:(length(columnNames) - 1)], collapse = ", "),
      ", and ",
      columnNames[length(columnNames)],
      "."
    )
  }
  return(txt)
}

#' Write column deletions
#' @description Writes about column deletions.
#' @noRd
write_columnsDelete <- function(columnsDelete, colourDelete) {
  if (length(columnsDelete) == 1) {
    txt <- paste0("Column ",
                  columnsDelete,
                  " is ",
                  colourDelete,
                  ".")
  } else if (length(columnsDelete) == 2) {
    txt <- paste0("Columns ",
                  columnsDelete[1],
                  " and ",
                  columnsDelete[2],
                  " are ",
                  colourDelete,
                  ".")
  } else {
    txt <- paste0(
      "Columns ",
      paste(columnsDelete[1:(length(columnsDelete) - 1)], collapse = ", "),
      ", and ",
      columnsDelete[length(columnsDelete)],
      " are ",
      colourDelete,
      "."
    )
  }
  return(txt)
}

#' Write row deletions
#' @description Writes about row deletions.
#' @noRd
write_rowsDelete <- function(rowsDelete, colourDelete) {
  if (length(rowsDelete) == 1) {
    txt <- paste0(as.character(length(rowsDelete)),
                  " row is ",
                  colourDelete,
                  ".")
  } else {
    txt <- paste0(as.character(length(rowsDelete)),
                  " rows are ",
                  colourDelete,
                  ".")
  }
  return(txt)
}

#' Write cell edits
#' @description Writes about cell edits.
#' @noRd
write_cellsEdit <- function(cellsEdit, colourEdit) {
  if (length(cellsEdit) == 1) {
    txt <- paste0(as.character(length(cellsEdit)),
                  " cell is ",
                  colourEdit,
                  ".")
  } else {
    txt <- paste0(as.character(length(cellsEdit)),
                  " cells are ",
                  colourEdit,
                  ".")
  }
  return(txt)
}

#' Write caption
#' @description Writes about the snapshot caption.
#' @noRd
write_caption <- function(caption) {
  txt <- paste0("The caption says ",
                caption,
                "."
                )
  return(txt)
}

#' Write dimensions
#' @description Writes about the snapshot dimensions.
#' @noRd
write_dimensions <- function(i, tables) {
  txt <- paste0("Snapshot ",
                i,
                " is ",
                as.character(nrow(tables[[i]]$body$dataset)),
                " rows by ",
                as.character(ncol(tables[[i]]$body$dataset)),
                " columns.")
  return(txt)
}

#' Write snapshots
#' @description Writes about the number of snapshots.
#' @noRd
write_snapshots <- function(tables) {
  txt <-
    paste0(
      "The Smallset Timeline, explaining how the dataset is preprocessed, consists of ",
      as.character(length(tables)),
      " Smallset snapshots."
    )
  return(txt)
}

#' Writes resume marker
#' @description Writes about the resume marker.
#' @noRd
write_resumeMarker <- function(i, caption) {
  txt <-
    paste0("Between snapshots ",
           i,
           " and ",
           i + 1,
           ", there is a resume marker that says ",
           caption,
           ".")
  return(txt)
}
