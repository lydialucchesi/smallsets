#' Write Smallset code
#' @description Turns the preprocessing code into a function that takes snapshots.
#' @keywords internal

write_smallset_code <-
  function(code,
           dir,
           ignoreCols,
           keepCols,
           smallset,
           lang,
           modelSelection = FALSE) {
    # Import preprocessing code
    if (dir != getwd()) {
      processTXT <-
        as.data.frame(readLines(paste0(dir, code), warn = FALSE))
    } else {
      processTXT <- as.data.frame(readLines(code, warn = FALSE))
    }
    colnames(processTXT) <- c("command")
    processTXT$command <- as.character(processTXT$command)
    
    commentsLines1 <-
      row.names(processTXT)[grepl("# start smallset", processTXT$command)]
    commentsLines2 <-
      row.names(processTXT)[grepl("# snap", processTXT$command)]
    commentsLines3 <-
      row.names(processTXT)[grepl("# end smallset", processTXT$command)]
    commentsLines <-
      c(commentsLines1, commentsLines2, commentsLines3)
    captions <-
      data.frame(
        n = seq(1, length(commentsLines)),
        row = as.numeric(commentsLines),
        caption = NA,
        stop = NA,
        text = NA
      )
    close <-
      as.numeric(row.names(processTXT)[grepl("]caption", processTXT$command)])
    
    for (c in 1:nrow(captions)) {
      if (grepl("caption[", processTXT[captions[c, "row"], "command"], fixed = TRUE)) {
        captions$caption[c] <- TRUE
        
        if (c != nrow(captions)) {
          captions$stop[c] <-
            close[(close >= captions$row[c]) &
                    (close < captions$row[c + 1])]
        } else {
          captions$stop[c] <- close[(close >= as.numeric(captions$row[c]))]
        }
        
        caption <-
          processTXT[captions$row[c]:captions$stop[c], "command"]
        caption[1] <- gsub(".*\\[", "", caption[1])
        caption[length(caption)] <-
          gsub("\\].*", "", caption[length(caption)])
        
        if (captions$row[c] != captions$stop[c]) {
          caption[2:length(caption)] <-
            substring(caption[2:length(caption)], 2)
          captions$text[c] <- paste(caption, collapse = '')
        } else {
          captions$text[c] <- caption
        }
        
      } else {
        captions$caption[c] <- FALSE
      }
      
    }
    
    processTXT <- processTXT$command
    for (c in 1:nrow(captions)) {
      if ((!is.na(captions$stop[c])) &
          (captions$row[c] != captions$stop[c])) {
        span <- captions$stop[c] - captions$row[c]
        processTXT <-
          processTXT[-seq(captions$row[c] + 1, captions$stop[c], 1)]
        processTXT[captions$row[c]] <-
          gsub(" caption\\[.*", "", processTXT[captions$row[c]])
        captions$row <- captions$row - (span)
        captions$stop <- captions$stop - (span)
      } else {
        processTXT[captions$row[c]] <-
          gsub(" caption\\[.*", "", processTXT[captions$row[c]])
      }
    }
    
    processTXT <- data.frame(command = processTXT)
    processTXT$command <- as.character(processTXT$command)
    rStart <-
      row.names(processTXT)[grepl("# start smallset", processTXT$command)]
    rStartName <-
      gsub("# start smallset ", "", processTXT[rStart, c("command")])
    rEnd <-
      row.names(processTXT)[grepl("# end smallset", processTXT$command)]
    rEndName <-
      gsub("# end smallset ", "", processTXT[rEnd, c("command")])
    
    smallsetCode <-
      processTXT[(as.numeric(rStart) + 1):(as.numeric(rEnd) - 1),]
    
    smallsetCode <- data.frame(command = smallsetCode)
    smallsetCode$command <- as.character(smallsetCode$command)
    
    # Prepare Python rows2snap function
    gen_rows2snap <- function(theDatasetName) {
      return(
        paste0(
          ".loc[np.array([",
          paste(smallset, collapse = ", "),
          "])[np.isin(np.array([",
          paste(smallset, collapse = ", "),
          "]), np.array(",
          paste(theDatasetName),
          ".index))]].copy(deep = True))"
        )
      )
    }
    
    # Insert code to take snapshots
    iterLim <-
      nrow(smallsetCode) + nrow(subset(smallsetCode, grepl("# snap ", smallsetCode$command))) - 1
    s = 1
    if (lang == "py") {
      for (i in 1:iterLim) {
        signal <- "# snap "
        if (grepl(signal, smallsetCode$command[i])) {
          s <- s + 1
          
          insertSnap <- c(paste0(
            "snapshots.append(",
            as.character(gsub(
              signal, "", smallsetCode$command[i]
            )),
            gen_rows2snap(as.character(
              gsub(signal, "", smallsetCode$command[i])
            ))
          ))
          
          smallsetCode <- c(smallsetCode[1:(i + 1), ],
                            insertSnap,
                            smallsetCode[(i + 2):nrow(smallsetCode), ])
        }
        
        smallsetCode <- data.frame(command = smallsetCode)
        smallsetCode$command <- as.character(smallsetCode$command)
      }
    } else {
      for (i in 1:iterLim) {
        signal <- "# snap "
        if (grepl(signal, smallsetCode$command[i])) {
          s <- s + 1
          
          insertSnap <- c(
            paste0(
              "snapshots[[",
              as.character(s),
              "]] <- ",
              as.character(gsub(
                signal, "", smallsetCode$command[i]
              )),
              "[(row.names(",
              as.character(gsub(
                signal, "", smallsetCode$command[i]
              )),
              ") %in% c(",
              paste(smallset, collapse = ", "),
              ")), ]"
            )
          )
          
          smallsetCode <- c(smallsetCode[1:(i + 1), ],
                            insertSnap,
                            smallsetCode[(i + 2):nrow(smallsetCode), ])
        }
        
        smallsetCode <- data.frame(command = smallsetCode)
        smallsetCode$command <- as.character(smallsetCode$command)
      }
    }
    
    # Make the preprocessing code a function
    if (lang == "py") {
      functionStart <-
        paste0("def apply_code(", rStartName, "):")
      smallsetCode <-
        c(
          "import numpy as np",
          "snapshots = []",
          functionStart,
          paste0(
            "snapshots.append(",
            rStartName,
            gen_rows2snap(rStartName)
          ),
          smallsetCode$command,
          paste0("snapshots.append(",
                 rEndName,
                 gen_rows2snap(rEndName)),
          "return snapshots"
        )
    } else {
      functionStart <-
        paste0("apply_code <- function(", rStartName, ") {")
      
      smallsetCode <-
        c(
          "snapshots <- list()",
          functionStart,
          paste0(
            "snapshots[[1]] <- ",
            rStartName,
            "[(row.names(",
            rStartName,
            ") %in% c(",
            paste(smallset, collapse = ", "),
            ")), ]"
          ),
          smallsetCode$command,
          paste0(
            "snapshots[[",
            as.character(s + 1),
            "]] <- ",
            rEndName,
            "[(row.names(",
            rEndName,
            ") %in% c(",
            paste(smallset, collapse = ", "),
            ")), ]"
          ),
          "return(snapshots)",
          "}"
        )
      
    }
    
    
    smallsetCode <- data.frame(command = smallsetCode)
    smallsetCode$command <- as.character(smallsetCode$command)
    
    if (lang == "py") {
      for (i in 4:length(smallsetCode$command))
        smallsetCode$command[i] <-
          paste0("    ", smallsetCode$command[i])
    }
    
    # Write the updated preprocessing function to the directory
    if (lang == "py") {
      fileConn <- file(paste0(dir, "/smallset_code.py"))
    } else {
      fileConn <- file(paste0(dir, "/smallset_code.R"))
    }
    writeLines(smallsetCode$command, fileConn)
    close(fileConn)
    
    # Determine location of any resume markers
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
    
    return(list(captions[, c("n", "text")], resumeLocs))
  }
