#' Run simple gurobi
#' @description Runs the coverage optimisation problem.
#' @noRd

run_simple_gurobi <-
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
    
    # Run coverage+variety optimisation model
    scoresT <- t(as.matrix(scores[,]))
    krow = t(rep(1, nrow(scores)))
    
    model <- list()
    
    model$A          <- rbind(scoresT, krow)
    model$modelsense <- 'max'
    model$rhs        <- c(rep(1, nrow(scoresT)), rowCount)
    model$sense      <- c(rep('>', nrow(scoresT)), '=')
    model$vtype      <- 'B'
    
    params <- list(OutputFlag = 0)
    
    result <- gurobi::gurobi(model, params)
    
    # Format model result
    modelSelect <- result$x
    rowIDs <- data.frame(rows = rownames(data), modelSelect)
    rowIDs <- subset(rowIDs, rowIDs$modelSelect == 1)$rows
    
    return(rowIDs)
  }