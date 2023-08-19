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
    lines <-
      c(row.names(script)[grepl("# smallsets start", script$command)],
        row.names(script)[grepl("# smallsets snap", script$command)],
        row.names(script)[grepl("# smallsets resume", script$command)],
        row.names(script)[grepl("# smallsets end", script$command)])
    
    # Find the type of each comment
    type <- NULL
    types <-
      data.frame(command = script[row.names(script) %in% lines, ], type = NA)
    for (i in 1:nrow(types)) {
      types$type[i] <-
        strsplit(types$command[i], " ")[[1]][3]
    }
    
    # Extract captions from script
    captions <-
      data.frame(
        n = seq(1, length(lines)),
        row = as.numeric(lines),
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
    
    # Find start, snap, and end instructions
    captions$type <- types$type
    captions_sub <- subset(captions, type != "resume")
    
    script$row_nums <- row.names(script)
    
    # Get snap args for start instruction
    start_args <-
      gsub("# smallsets start ", "", script[grepl("# smallsets start", script$command), c("command")])
    start_args <- gsub(" caption\\[.*", "", start_args)
    start_args <- strsplit(start_args, " ")
    start_loc <-
      data.frame(
        loc = as.character(rep(NA, length(start_args))),
        name = as.character(rep(NA, length(start_args))),
        row_nums = row.names(script)[grepl("# smallsets start", script$command)],
        stops = captions_sub$stop[1]
      )
    if (length(start_args[[1]]) > 1) {
      start_loc$loc <- start_args[[1]][1]
      start_loc$name <- start_args[[1]][2]
    } else {
      start_loc$loc <- "+1"
      start_loc$name <- start_args[[1]][1]
    }
    start_loc$add <- grepl("+", start_loc$loc, fixed = TRUE)
    if (isTRUE(start_loc$add[1])) {
      start_loc$loc <-
        as.integer(start_loc$stops) + as.integer(start_loc$loc)
    }
    start_loc$type <- "start"
    
    # Get snap args for end instruction
    end_args <-
      gsub("# smallsets end ", "", script[grepl("# smallsets end", script$command), c("command")])
    end_args <- gsub(" caption\\[.*", "", end_args)
    end_args <- strsplit(end_args, " ")
    end_loc <-
      data.frame(
        loc = as.character(rep(NA, length(end_args))),
        name = as.character(rep(NA, length(end_args))),
        row_nums = row.names(script)[grepl("# smallsets end", script$command)],
        stops = captions_sub$stop[nrow(captions)]
      )
    if (length(end_args[[1]]) > 1) {
      end_loc$loc <- end_args[[1]][1]
      end_loc$name <- end_args[[1]][2]
    } else {
      end_loc$loc <- "+1"
      end_loc$name <- end_args[[1]][1]
    }
    end_loc$add <- grepl("+", end_loc$loc, fixed = TRUE)
    if (isTRUE(end_loc$add[1])) {
      end_loc$loc <- as.integer(end_loc$stops) + as.integer(end_loc$loc)
    }
    end_loc$type <- "end"
    
    # Get snap args for intermediate snap instruction
    snap_args <- gsub("# smallsets snap ", "",
                      script[row.names(script)[grepl("# smallsets snap", script$command)],
                             c("command")])
    snap_args <- gsub(" caption\\[.*", "", snap_args)
    snap_args <- strsplit(snap_args, " ")
    if (nrow(captions_sub) > 2) {
      stops <- captions_sub$stop[2:(1 + length(snap_args))]
    } else {
      stops <- as.character(rep(NA, length(snap_args)))
    }
    snap_locs <-
      data.frame(
        loc = as.character(rep(NA, length(snap_args))),
        name = as.character(rep(NA, length(snap_args))),
        row_nums = row.names(script)[grepl("# smallsets snap", script$command)],
        stops = stops
      )
    if (length(snap_args) > 0) {
      for (i in 1:length(snap_args)) {
        if (length(snap_args[[i]]) > 1) {
          snap_locs$loc[i] <- snap_args[[i]][1]
          snap_locs$name[i] <- snap_args[[i]][2]
        } else {
          snap_locs$loc[i] <- "+1"
          snap_locs$name[i] <- snap_args[[i]][1]
        }
      }
    }
    snap_locs$add <- grepl("+", snap_locs$loc, fixed = TRUE)
    for (r in 1:nrow(snap_locs)) {
      if (isTRUE(snap_locs$add[r])) {
        snap_locs$loc[r] <-
          as.integer(snap_locs$stops[r]) + as.integer(snap_locs$loc[r])
      }
    }
    if (length(snap_args) > 0) {
      snap_locs$type <- "snap"
    }
    
    # Merge snap location info for the different instruction types
    locs <-
      as.data.frame(rbind(start_loc, snap_locs, end_loc))[, c("loc", "name", "type")]
    locs$loc <- as.integer(locs$loc)
    
    # Subset to preprocessing code
    script <- script[locs$loc[1]:locs$loc[nrow(locs)], ]
    script$row_nums <- NULL
    first <- as.integer(row.names(script)[1])
    
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
    
    # Insert intermediate snapshots
    if (length(snap_args) > 0) {
      for (s in 2:(nrow(locs) - 1)) {
        
        # For Python script
        if (lang == "py") {
          
          # For Smallset selection
          if ("allROWS" %in% smallset) {
            insertSnap <-
              c(paste0("snapshots.append(", as.character(locs$name[s])), "[:])")
            
          # For building the figure
          } else {
            insertSnap <- c(paste0(
              "snapshots.append(",
              as.character(locs$name[s]),
              gen_rows2snap(as.character(locs$name[s]))
            ))
          }
          
          commands <-
            c(script[row.names(script) %in% (first:locs$loc[s]), "command"],
              insertSnap,
              script[row.names(script) %in% (locs$loc[s] + 1):max(as.integer(row.names(script))), "command"])
          script <- data.frame(command = commands)
          row.names(script) <- first:(first + nrow(script) - 1)
          locs$loc <-
            ifelse(row.names(locs) > s, locs$loc + 1, locs$loc)
          
        # For R script
        } else {
          
          # For Smallset selection
          if ("allROWS" %in% smallset) {
            insertSnap <- c(paste0(
              "snapshots[[",
              as.character(s),
              "]] <- ",
              locs$name[s],
              "[,]"
            ))
            
          # For building the figure
          } else {
            insertSnap <- c(
              paste0(
                "snapshots[[",
                as.character(s),
                "]] <- ",
                locs$name[s],
                "[(row.names(",
                locs$name[s],
                ") %in% c(",
                paste(smallset, collapse = ", "),
                ")), ]"
              )
            )
          }
          
          commands <-
            c(script[row.names(script) %in% (first:locs$loc[s]), "command"],
              insertSnap,
              script[row.names(script) %in% (locs$loc[s] + 1):max(as.integer(row.names(script))), "command"])
          script <- data.frame(command = commands)
          row.names(script) <- first:(first + nrow(script) - 1)
          locs$loc <-
            ifelse(row.names(locs) > s, locs$loc + 1, locs$loc)
        }
      }
    }
    
    # Turn it into a function (insert first and last snapshots)
    
    # For Python script
    if (lang == "py") {
      functionStart <-
        paste0("def apply_code(", locs$name[1], "):")
      
      # For Smallset selection
      if ("allROWS" %in% smallset) {
        script <-
          c(
            "import numpy as np",
            "snapshots = []",
            functionStart,
            paste0("snapshots.append(",
                   locs$name[1],
                   "[:])"),
            script$command,
            paste0("snapshots.append(",
                   locs$name[nrow(locs)],
                   "[:])"),
            "return snapshots"
          )
      
      # For building the figure
      } else {
        script <-
          c(
            "import numpy as np",
            "snapshots = []",
            functionStart,
            paste0(
              "snapshots.append(",
              locs$name[1],
              gen_rows2snap(locs$name[1])
            ),
            script$command,
            paste0(
              "snapshots.append(",
              locs$name[nrow(locs)],
              gen_rows2snap(locs$name[nrow(locs)])
            ),
            "return snapshots"
          )
      }
      
    # For R script
    } else {
      functionStart <-
        paste0("apply_code <- function(", locs$name[1], ") {")
      
      # For Smallset selection
      if ("allROWS" %in% smallset) {
        script <-
          c(
            "snapshots <- list()",
            functionStart,
            paste0("snapshots[[1]] <- ",
                   locs$name[1],
                   "[,]"),
            script$command,
            paste0(
              "snapshots[[",
              as.character(nrow(locs)),
              "]] <- ",
              locs$name[nrow(locs)],
              "[,]"
            ),
            "return(snapshots)",
            "}"
          )
        
      # For building the figure
      } else {
        script <-
          c(
            "snapshots <- list()",
            functionStart,
            paste0(
              "snapshots[[1]] <- ",
              locs$name[1],
              "[(row.names(",
              locs$name[1],
              ") %in% c(",
              paste(smallset, collapse = ", "),
              ")), ]"
            ),
            script$command,
            paste0(
              "snapshots[[",
              nrow(locs),
              "]] <- ",
              locs$name[nrow(locs)],
              "[(row.names(",
              locs$name[nrow(locs)],
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
    tf <-
      tempfile(pattern = "smallsets", fileext = paste0(".", lang))
    if (lang == "py") {
      fileConn <- file(tf)
    } else {
      fileConn <- file(tf)
    }
    writeLines(script$command, fileConn)
    close(fileConn)
    
    # Find location of resume markers
    count <- -1
    resume_locs <- c()
    for (i in 1:nrow(script)) {
      if (grepl("snapshots", script$command[i])) {
        count <- count + 1
      }
      if (grepl("# smallsets resume ", script$command[i])) {
        resume_locs <- c(resume_locs, count)
      }
    }
    
    return(list(captions[, c("n", "text")], resume_locs, tf))
  }
