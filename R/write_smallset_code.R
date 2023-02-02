#' Write Smallset code
#' @description Turns the preprocessing code into a function that takes
#'   snapshots.
#' @keywords internal

write_smallset_code <-
  function(code,
           smallset,
           lang) {
    # Import preprocessing code
    script <- data.frame(command = readLines(code, warn = FALSE))
    
    # Find rows with structured comments
    commentsLines <-
      c(row.names(script)[grepl("# smallsets start", script$command)],
        row.names(script)[grepl("# smallsets snap", script$command)],
        row.names(script)[grepl("# smallsets resume", script$command)],
        row.names(script)[grepl("# smallsets end", script$command)])
    
    # Retrieve captions from script
    captions <-
      data.frame(
        n = seq(1, length(commentsLines)),
        row = as.numeric(commentsLines),
        caption = NA,
        stop = NA,
        text = NA
      )
    close <-
      as.numeric(row.names(script)[grepl("]caption", script$command)])
    
    for (c in 1:nrow(captions)) {
      if (grepl("caption[", script[captions[c, "row"], "command"], fixed = TRUE)) {
        captions$caption[c] <- TRUE
        
        if (c != nrow(captions)) {
          captions$stop[c] <-
            close[(close >= captions$row[c]) &
                    (close < captions$row[c + 1])]
        } else {
          captions$stop[c] <- close[(close >= as.numeric(captions$row[c]))]
        }
        
        caption <-
          script[captions$row[c]:captions$stop[c], "command"]
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
    
    # Remove captions from script
    script <- script$command
    for (c in 1:nrow(captions)) {
      if ((!is.na(captions$stop[c])) &
          (captions$row[c] != captions$stop[c])) {
        span <- captions$stop[c] - captions$row[c]
        script <-
          script[-seq(captions$row[c] + 1, captions$stop[c], 1)]
        script[captions$row[c]] <-
          gsub(" caption\\[.*", "", script[captions$row[c]])
        captions$row <- captions$row - (span)
        captions$stop <- captions$stop - (span)
      } else {
        script[captions$row[c]] <-
          gsub(" caption\\[.*", "", script[captions$row[c]])
      }
    }
    
    # Find structured comments after removing captions
    script <- data.frame(command = script)
    script$command <- as.character(script$command)
    start <-
      row.names(script)[grepl("# smallsets start", script$command)]
    startName <-
      gsub("# smallsets start ", "", script[start, c("command")])
    end <-
      row.names(script)[grepl("# smallsets end", script$command)]
    endName <-
      gsub("# smallsets end ", "", script[end, c("command")])
    
    # Subset to preprocessing code
    script <-
      data.frame(command = script[(as.numeric(start) + 1):(as.numeric(end) - 1),])
    
    # Prepare function needed for python scripts
    # Avoids error if calling a removed row
    if (lang == "py") {
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
    }
    
    # Insert code to take snapshots
    iterLim <-
      nrow(script) + nrow(subset(script, grepl("# smallsets snap ", script$command))) - 1
    s = 1
    if (lang == "py") {
      for (i in 1:iterLim) {
        signal <- "# smallsets snap "
        if (grepl(signal, script$command[i])) {
          s <- s + 1
          
          if ("allROWS" %in% smallset) {
            insertSnap <- c(paste0("snapshots.append(",
                                   as.character(
                                     gsub(signal, "", script$command[i])
                                   ),
                                   "[:])"))
          } else {
            insertSnap <- c(paste0(
              "snapshots.append(",
              as.character(gsub(signal, "", script$command[i])),
              gen_rows2snap(as.character(
                gsub(signal, "", script$command[i])
              ))
            ))
          }
          
          script <- c(script[1:(i + 1), ],
                      insertSnap,
                      script[(i + 2):nrow(script), ])
        }
        
        script <- data.frame(command = script)
        script$command <- as.character(script$command)
      }
    } else {
      for (i in 1:iterLim) {
        signal <- "# smallsets snap "
        if (grepl(signal, script$command[i])) {
          s <- s + 1
          
          if ("allROWS" %in% smallset) {
            insertSnap <- c(paste0(
              "snapshots[[",
              as.character(s),
              "]] <- ",
              as.character(gsub(signal, "", script$command[i])),
              "[,]"
            ))
          } else {
            insertSnap <- c(
              paste0(
                "snapshots[[",
                as.character(s),
                "]] <- ",
                as.character(gsub(signal, "", script$command[i])),
                "[(row.names(",
                as.character(gsub(signal, "", script$command[i])),
                ") %in% c(",
                paste(smallset, collapse = ", "),
                ")), ]"
              )
            )
          }
          
          script <- c(script[1:(i + 1), ],
                      insertSnap,
                      script[(i + 2):nrow(script), ])
        }
        
        script <- data.frame(command = script)
        script$command <- as.character(script$command)
      }
    }
    
    # Turn it into a function
    if (lang == "py") {
      functionStart <-
        paste0("def apply_code(", startName, "):")
      if ("allROWS" %in% smallset) {
        script <-
          c(
            "import numpy as np",
            "snapshots = []",
            functionStart,
            paste0("snapshots.append(",
                   startName,
                   "[:])"),
            script$command,
            paste0("snapshots.append(",
                   endName,
                   "[:])"),
            "return snapshots"
          )
      } else {
        script <-
          c(
            "import numpy as np",
            "snapshots = []",
            functionStart,
            paste0(
              "snapshots.append(",
              startName,
              gen_rows2snap(startName)
            ),
            script$command,
            paste0("snapshots.append(",
                   endName,
                   gen_rows2snap(endName)),
            "return snapshots"
          )
      }
      
    } else {
      functionStart <-
        paste0("apply_code <- function(", startName, ") {")
      
      if ("allROWS" %in% smallset) {
        script <-
          c(
            "snapshots <- list()",
            functionStart,
            paste0("snapshots[[1]] <- ",
                   startName,
                   "[,]"),
            script$command,
            paste0("snapshots[[",
                   as.character(s + 1),
                   "]] <- ",
                   endName,
                   "[,]"),
            "return(snapshots)",
            "}"
          )
      } else {
        script <-
          c(
            "snapshots <- list()",
            functionStart,
            paste0(
              "snapshots[[1]] <- ",
              startName,
              "[(row.names(",
              startName,
              ") %in% c(",
              paste(smallset, collapse = ", "),
              ")), ]"
            ),
            script$command,
            paste0(
              "snapshots[[",
              as.character(s + 1),
              "]] <- ",
              endName,
              "[(row.names(",
              endName,
              ") %in% c(",
              paste(smallset, collapse = ", "),
              ")), ]"
            ),
            "return(snapshots)",
            "}"
          )
      }
    }

    script <- data.frame(command = as.character(script))
    
    # Add python tabs
    if (lang == "py") {
      for (i in 4:length(script$command))
        script$command[i] <-
          paste0("    ", script$command[i])
    }
    
    # Write preprocessing function to temporary directory
    tf <- tempfile(pattern = "smallsets", fileext = paste0(".", lang))
    if (lang == "py") {
      fileConn <- file(tf)
    } else {
      fileConn <- file(tf)
    }
    writeLines(script$command, fileConn)
    close(fileConn)
    
    # Find location of resume markers
    snapCount <- -1
    resumeLocs <- c()
    for (i in 1:nrow(script)) {
      if (grepl("snapshots", script$command[i])) {
        snapCount <- snapCount + 1
      }
      if (grepl("# smallsets resume ", script$command[i])) {
        resumeLocs <- c(resumeLocs, snapCount)
      }
    }
    
    return(list(captions[, c("n", "text")], resumeLocs, tf))
  }
