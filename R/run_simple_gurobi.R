#' Run simple gurobi
#' @description Runs simple gurobi selection model.
#' @keywords internal

run_simple_gurobi <-
  function(data = data,
           code = code,
           dir = dir,
           rowCount = rowCount,
           lang = lang) {
    fullCheck <- select_smallset(
      data = data,
      rowCount = nrow(data),
      runBig = TRUE,
      ignoreCols = NULL
    )
    
    notNeeded <-
      write_smallset_code(
        scriptName = code,
        dir = dir,
        runBig = TRUE,
        ignoreCols = NULL,
        smallset = fullCheck,
        lang = lang
      )
    
    source(paste0(dir, "/smallset_code.R"))
    smallsetList <- apply_code(data)
    
    scores <-
      prepare_score_sheet(smallsetList = smallsetList, data = data)
    scores <- scores[, colSums(scores != 0) > 0]
    
    myscoresT <- t(as.matrix(scores[, ]))
    krow = t(rep(1, nrow(scores)))
    
    model <- list()
    
    model$A          <- rbind(myscoresT, krow)
    # model$obj        <- obj
    model$modelsense <- 'max'
    model$rhs        <- c(rep(1, nrow(myscoresT)), rowCount)
    model$sense      <- c(rep('>', nrow(myscoresT)), '=')
    model$vtype      <- 'B'
    
    params <- list(OutputFlag = 0)
    
    result <- gurobi::gurobi(model, params)
    modelSelect <- result$x
    rowNums <- data.frame(rows = rownames(data), modelSelect)
    rowNums <- subset(rowNums, rowNums$modelSelect == 1)$rows
    
    return(rowNums)
  }
