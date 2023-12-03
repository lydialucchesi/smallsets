#' Write Smallset code
#' @description Turns the preprocessing code into a function that takes
#'   snapshots.
#' @noRd

write_smallset_code <-
  function(code,
           smallset,
           lang) {
    # Import preprocessing code
    script <- data.frame(command = readLines(code, warn = FALSE))
    
    # Find rows with structured comments
    lines <-
      c(row.names(script)[grepl("# smallsets snap", script$command)],
        row.names(script)[grepl("# smallsets resume", script$command)])
    lines <- sort(as.integer(lines))
    
    # Find the type of each comment
    type <- NULL
    types <-
      data.frame(command = script[row.names(script) %in% lines,], type = NA)
    for (i in 1:nrow(types)) {
      types$type[i] <-
        strsplit(trimws(types$command[i]), " ")[[1]][3]
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
        caption <- trimws(caption, which = "left")
        caption[1] <- trimws(gsub(".*\\[", "", caption[1]), which = "right")
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
    captions_sub <- subset(captions, type == "snap")
    
    script$row_nums <- row.names(script)
    
    # Get snap args
    snap_args <- trimws(script[row.names(script)[grepl("# smallsets snap", script$command)],
                               c("command")])
    snap_args <- gsub("# smallsets snap ", "", snap_args)
    snap_args <- gsub(" caption\\[.*", "", snap_args)
    snap_args <- strsplit(snap_args, " ")
    locs <-
      data.frame(
        loc = as.character(rep(NA, length(snap_args))),
        name = as.character(rep(NA, length(snap_args))),
        row_nums = row.names(script)[grepl("# smallsets snap", script$command)],
        stops = captions_sub$stop
      )
    for (i in 1:length(snap_args)) {
      if (length(snap_args[[i]]) > 1) {
        locs$loc[i] <- snap_args[[i]][1]
        locs$name[i] <- snap_args[[i]][2]
      } else {
        locs$loc[i] <- "+0"
        locs$name[i] <- snap_args[[i]][1]
      }
    }
    locs$add <- grepl("+", locs$loc, fixed = TRUE)
    for (r in 1:nrow(locs)) {
      if (isTRUE(locs$add[r])) {
        locs$loc[r] <-
          as.integer(locs$stops[r]) + as.integer(locs$loc[r])
      }
    }
    locs$loc <- as.integer(locs$loc)
    
    # Subset to preprocessing code
    script <- script[locs$loc[1]:locs$loc[nrow(locs)],]
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
    for (s in 1:nrow(locs)) {
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
              "[(rownames(",
              locs$name[s],
              ") %in% c(",
              paste("'", smallset, "'", collapse = ", ", sep = ""),
              ")), ]"
            )
          )
        }
        
        if (locs$loc[s] != max(as.integer(row.names(script)))) {
          commands <-
            c(script[row.names(script) %in% (first:locs$loc[s]), "command"],
              insertSnap,
              script[row.names(script) %in% (locs$loc[s] + 1):max(as.integer(row.names(script))), "command"])
        } else {
          commands <-
            c(script[row.names(script) %in% (first:locs$loc[s]), "command"], insertSnap)
        }
        
        script <- data.frame(command = commands)
        row.names(script) <- first:(first + nrow(script) - 1)
        locs$loc <-
          ifelse(row.names(locs) > s, locs$loc + 1, locs$loc)
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
            script$command,
            "return snapshots"
          )
        
        # For building the figure
      } else {
        script <-
          c(
            "import numpy as np",
            "snapshots = []",
            functionStart,
            script$command,
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
          c("snapshots <- list()",
            functionStart,
            script$command,
            "return(snapshots)",
            "}")
        
        # For building the figure
      } else {
        script <-
          c("snapshots <- list()",
            functionStart,
            script$command,
            "return(snapshots)",
            "}")
      }
    }
    
    script <- data.frame(command = as.character(script))
    
    if (lang == "py") {
      tabs <- data.frame(row = c(), tab = c())
      for (i in 4:(nrow(script) - 1)) {
        if (isFALSE(startsWith(script$command[i], "snapshots.append(")) & (script$command[i] != "")) {
          split <- strsplit(x = script$command[i], split = "")[[1]]
          f <- strsplit(trimws(paste0(split[1:4], collapse = "")), split = "")[[1]][1]
          f <- ifelse(is.na(f), "", f)
          if (f != "#") {
            if (isTRUE(identical(split[1:4], c(" ", " ", " ", " ")))) {
              tab <- T
            } else {
              tab <- F
            }
            result <- data.frame(row = c(i), tab = c(tab))
            tabs <- rbind(tabs, result)
          }
        }
      }
      
      if (sum(tabs$tab) == nrow(tabs)) {
        for(i in 1:nrow(tabs)) {
          script$command[tabs$row[i]] <- sub("....", "", script$command[tabs$row[i]])
        }
      }
    }
    
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
    resume_locs <- as.numeric(rownames(subset(captions, type == "resume"))) - 1
    
    return(list(captions[, c("n", "text")], resume_locs, tf))
  }
