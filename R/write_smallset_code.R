#' Write smallset code
#' @description A function to prepare the smallset preprocessing code
#' @keywords internal
#' @export

write_smallset_code <- function(scriptName, dir) {
  
  processTXT <- as.data.frame(readLines(scriptName, warn = FALSE))
  colnames(processTXT) <- c("command")
  processTXT$command <- as.character(processTXT$command)
  
  rStart <-
    row.names(processTXT)[processTXT$command == "# start smallset"]
  rEnd <-
    row.names(processTXT)[processTXT$command == "# end smallset"]
  
  smallsetCode <-
    processTXT[(as.numeric(rStart) + 1):(as.numeric(rEnd) - 1),]
  
  smallsetCode <- data.frame(command = smallsetCode)
  smallsetCode$command <- as.character(smallsetCode$command)
  
  iterLim <-
    nrow(smallsetCode) + nrow(subset(smallsetCode, grepl("# snap ", smallsetCode$command))) - 1
  s = 1
  for (i in 1:iterLim) {
    signal <- "# snap "
    if (grepl(signal, smallsetCode$command[i])) {
      s <- s + 1
      
      insertSnap <- c(paste0(
        "snapshots[[",
        as.character(s),
        "]] <- ",
        as.character(stringr::str_remove(smallsetCode$command[i], signal))
      ))
      
      if (i != iterLim) {
        smallsetCode <- c(smallsetCode[1:(i + 1),],
                          insertSnap,
                          smallsetCode[(i + 2):nrow(smallsetCode),])
      } else {
        smallsetCode <- c(smallsetCode[1:i,],
                          insertSnap,
                          "return(snapshots)")
      }
      
      smallsetCode <- data.frame(command = smallsetCode)
      smallsetCode$command <- as.character(smallsetCode$command)
    }
  }
  
  initialName <-
    as.character(stringr::str_remove(subset(
      smallsetCode, grepl("# snap ", smallsetCode$command)
    )[1,], "# snap "))
  functionStart <-
    paste0("apply_code <- function(", initialName, ") {")
  smallsetCode <-
    c("snapshots <- list()", 
      functionStart,
      paste0("snapshots[[1]] <- ", initialName),
      smallsetCode$command,
      "}")
  smallsetCode <- data.frame(command = smallsetCode)
  smallsetCode$command <- as.character(smallsetCode$command)
  
  fileConn <- file(paste0(dir, "/smallset_code.R"))
  writeLines(smallsetCode$command, fileConn)
  close(fileConn)
  
}
