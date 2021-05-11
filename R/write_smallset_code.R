#' Write smallset code
#' @description The function turns the preprocessing code into a function that
#'   takes snapshots.
#' @keywords internal

write_smallset_code <- function(scriptName, dir, runBig, smallset) {
  
  # Import preprocessing code
  processTXT <- as.data.frame(readLines(scriptName, warn = FALSE))
  colnames(processTXT) <- c("command")
  processTXT$command <- as.character(processTXT$command)
  
  rStart <-
    row.names(processTXT)[grepl("# start smallset", processTXT$command)]
  rStartName <- stringr::str_remove(processTXT[rStart, c("command")], "# start smallset ")
  rEnd <-
    row.names(processTXT)[grepl("# end smallset", processTXT$command)]
  rEndName <- stringr::str_remove(processTXT[rEnd, c("command")], "# end smallset ")
  
  smallsetCode <-
    processTXT[(as.numeric(rStart) + 1):(as.numeric(rEnd) - 1),]
  
  smallsetCode <- data.frame(command = smallsetCode)
  smallsetCode$command <- as.character(smallsetCode$command)
  
  # Insert code to take snapshots
  iterLim <-
    nrow(smallsetCode) + nrow(subset(smallsetCode, grepl("# snap ", smallsetCode$command))) - 1
  s = 1
  for (i in 1:iterLim) {
    signal <- "# snap "
    if (grepl(signal, smallsetCode$command[i])) {
      s <- s + 1
      
      if (isTRUE(runBig)) {
        insertSnap <- c(
          paste0(
            "snapshots[[",
            as.character(s),
            "]] <- ",
            as.character(stringr::str_remove(smallsetCode$command[i], signal)),
            as.character("[(row.names("),
            as.character(stringr::str_remove(smallsetCode$command[i], signal)),
            as.character(") %in% c("),
            paste(smallset, collapse = ", "),
            as.character(")), ]")
          )
        )
      } else {
        insertSnap <- c(paste0(
          "snapshots[[",
          as.character(s),
          "]] <- ",
          as.character(stringr::str_remove(smallsetCode$command[i], signal))
        ))
      }
      
      smallsetCode <- c(smallsetCode[1:(i + 1),],
                          insertSnap,
                          smallsetCode[(i + 2):nrow(smallsetCode),])
      }
      
      smallsetCode <- data.frame(command = smallsetCode)
      smallsetCode$command <- as.character(smallsetCode$command)
    }
  
  # Make the preprocessing code a function
  functionStart <-
    paste0("apply_code <- function(", rStartName, ") {")
  
  if (isTRUE(runBig)) {
    smallsetCode <-
      c(
        "snapshots <- list()",
        functionStart,
        paste0(
          "snapshots[[1]] <- ",
          rStartName,
          as.character("[(row.names("),
          rStartName,
          as.character(") %in% c("),
          paste(smallset, collapse = ", "),
          as.character(")), ]")
        ),
        smallsetCode$command,
        paste0(
          "snapshots[[", 
          as.character(s + 1),
          "]] <- ",
          rEndName,
          as.character("[(row.names("),
          rEndName,
          as.character(") %in% c("),
          paste(smallset, collapse = ", "),
          as.character(")), ]")
        ),
        "return(snapshots)",
        "}"
      )
  } else {
    smallsetCode <-
      c(
        "snapshots <- list()",
        functionStart,
        paste0("snapshots[[1]] <- ", rStartName),
        smallsetCode$command,
        paste0("snapshots[[",
               as.character(s + 1),
               "]] <- ", 
               rEndName),
        "return(snapshots)",
        "}"
      )
  }
  
  smallsetCode <- data.frame(command = smallsetCode)
  smallsetCode$command <- as.character(smallsetCode$command)
  
  # Write the updated preprocessing function to directory
  fileConn <- file(paste0(dir, "/smallset_code.R"))
  writeLines(smallsetCode$command, fileConn)
  close(fileConn)
  
  # Determine location of any resume comments
  snapCount <- -1
  resumeLocs <- c()
  for (i in 1:nrow(smallsetCode)) {
    if (grepl("snapshots", smallsetCode$command[i])) {
      snapCount <- snapCount + 1
    }
    if (grepl("# resume ", smallsetCode$command[i])) {
      resumeLocs <- c(resumeLocs, snapCount)
    }
  }
  
  return(resumeLocs)
}