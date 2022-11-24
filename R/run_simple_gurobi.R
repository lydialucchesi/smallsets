#' Run simple gurobi
#' @description Runs the coverage optimisation problem.
#' @import "reticulate"
#' @keywords internal

run_simple_gurobi <-
  function(data,
           code,
           dir,
           rowCount,
           lang,
           fourCols) {
    # Take snapshots of dataset at snapshot points
    fullCheck <- select_smallset(data = data,
                                 rowCount = nrow(data),
                                 ignoreCols = NULL)
    notNeeded <-
      write_smallset_code(
        code = code,
        dir = dir,
        ignoreCols = NULL,
        smallset = fullCheck,
        lang = lang,
        modelSelection = TRUE
      )
    
    if (lang == "py") {
      source_python(paste0(dir, "/smallset_code.py"))
    } else {
      source(paste0(dir, "/smallset_code.R"))
    }
    smallsetList <- apply_code(data)
    
    # Generate coverage indicator matrix
    scores <-
      prepare_score_sheet(smallsetList = smallsetList, fourCols = fourCols)
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
    rowNums <- data.frame(rows = rownames(data), modelSelect)
    rowNums <- subset(rowNums, rowNums$modelSelect == 1)$rows
    
    return(rowNums)
  }
