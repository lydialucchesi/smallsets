#' Write add colour
#' @description Writes about the add colour.
#' @keywords internal
write_colourAdd <- function(colourAdd) {
  txt <- paste0("Data additions are represented with the colour ",
                colourAdd,
                ".")
  return(txt)
}

#' Write delete colour
#' @description Writes about the delete colour.
#' @keywords internal
write_colourDelete <- function(colourDelete) {
  txt <- paste0("Data deletions are represented with the colour ",
                colourDelete,
                ".")
  return(txt)
}

#' Write edit colour
#' @description Writes about the edit colour.
#' @keywords internal
write_colourEdit <- function(colourEdit) {
  txt <- paste0("Data edits are represented with the colour ",
                colourEdit,
                ".")
  return(txt)
}

#' Write column additions
#' @description Writes about column additions.
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
write_columnNames <- function(columnNames) {
  if (length(columnNames) == 2) {
    txt <- paste0(
      "The columns, in order from left to right, are: ",
      columnNames[1],
      " and ",
      columnNames[2],
      "."
    )
  } else {
    txt <- paste0(
      "The columns, in order from left to right, are: ",
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
write_caption <- function(caption) {
  txt <- paste0("The caption is quote ",
                caption,
                " unquote."
                )
  return(txt)
}

#' Write dimensions
#' @description Writes about the snapshot dimensions.
#' @keywords internal
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
#' @keywords internal
write_snapshots <- function(tables) {
  txt <- paste0("The Smallset Timeline contains ",
                as.character(length(tables)),
                " Smallset snapshots."
                )
  return(txt)
}

