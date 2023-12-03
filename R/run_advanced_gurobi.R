#' Run advanced gurobi
#' @description Runs the coverage+variety optimisation problem.
#' @noRd

run_advanced_gurobi <-
  function(data,
           code,
           rowCount,
           lang,
           fourCols,
           ignoreCols) {
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
    colours <- prepare_colour_sheet(smallsetList, fourCols, ignoreCols)
    
    # From visual appearance matrix,
    # generate distance matrix with hamming distance
    tests <- expand.grid(1:nrow(colours), 1:nrow(colours))
    distance_matrix <- function(i1, i2) {
      sum(colours[i1, ] != colours[i2, ])
    }
    hamming_distances <- mapply(distance_matrix, tests$Var1, tests$Var2)
    distance_matrix <- matrix(hamming_distances,
                              nrow = nrow(data),
                              ncol = nrow(data),
                              byrow = T)
    
    # Run coverage+variety optimisation model
    scoresT <- t(as.matrix(scores[,]))
    krow = t(rep(1, nrow(scores)))
    
    model <- list()
    
    model$A     <- rbind(scoresT, krow)
    model$Q     <- distance_matrix
    model$obj   <- rep(0, nrow(data))
    model$modelsense <- 'max'
    model$rhs   <- c(rep(1, nrow(scoresT)), rowCount)
    model$sense <- c(rep('>', nrow(scoresT)), '=')
    model$vtype <- rep('B', nrow(data))
    
    params <- list(OutputFlag = 0)
    
    result <- gurobi::gurobi(model, params)
    
    # Format model result
    modelSelect <- result$x
    rowIDs <- data.frame(rows = rownames(data), modelSelect)
    rowIDs <- subset(rowIDs, rowIDs$modelSelect == 1)$rows

    return(rowIDs)
  }