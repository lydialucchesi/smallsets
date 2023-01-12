#' Run advanced gurobi
#' @description Runs the coverage+variety optimisation problem.
#' @keywords internal

run_advanced_gurobi <-
  function(data,
           code,
           rowCount,
           lang,
           fourCols) {
    # Take snapshots of dataset at snapshot points
    output <- write_smallset_code(code, c("allROWS"), lang)
    
    # Run function to take snapshots
    apply_code <- NULL
    if (lang == "py") {
      reticulate::source_python(output[[3]])
    } else {
      source(output[[3]], local = TRUE)
    }
    smallsetList <- apply_code(data)
    unlink(output[[3]])
    
    # Generate coverage indicator matrix
    scores <- prepare_score_sheet(smallsetList, fourCols)
    scores <- scores[, colSums(scores != 0) > 0]
    
    # Generate visual appearance matrix
    colours <- prepare_colour_sheet(smallsetList, fourCols)
    
    # From visual appearance matrix,
    # generate distance matrix with hamming distance
    D <-
      matrix(
        rep(0, nrow(data) * nrow(data)),
        nrow = nrow(data),
        ncol = nrow(data),
        byrow = T
      )
    for (i1 in 1:nrow(data)) {
      for (i2 in 1:nrow(data)) {
        di <- as.vector(colours[i1, ])
        dj <- as.vector(colours[i2, ])
        counter <- 0
        for (j in 1:ncol(colours)) {
          if (isFALSE(di[j] == dj[j])) {
            counter <- counter + 1
          }
        }
        dij <- counter / ncol(colours)
        D[i1, i2] <- dij
      }
    }
    
    # Run coverage+variety optimisation model
    scoresT <- t(as.matrix(scores[,]))
    krow = t(rep(1, nrow(scores)))
    
    model <- list()
    
    model$A     <- rbind(scoresT, krow)
    model$Q     <- D
    model$obj   <- rep(0, nrow(data))
    model$modelsense <- 'max'
    model$rhs   <- c(rep(1, nrow(scoresT)), rowCount)
    model$sense <- c(rep('>', nrow(scoresT)), '=')
    model$vtype <- rep('B', nrow(data))
    
    params <- list(OutputFlag = 0)
    
    result <- gurobi::gurobi(model, params)
    
    # Format model result
    modelSelect <- result$x
    rowNums <- data.frame(rows = rownames(data), modelSelect)
    rowNums <- subset(rowNums, rowNums$modelSelect == 1)$rows
    
  }