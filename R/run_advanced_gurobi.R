#' Run advanced gurobi
#' @description Runs advanced gurobi selection model.
#' @import "reticulate"
#' @keywords internal

run_advanced_gurobi <-
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
        lang = lang,
        modelSelection = TRUE
      )
    
    if (lang == "py") {
      source_python(paste0(dir, "/smallset_code.py"))
    } else {
      source(paste0(dir, "/smallset_code.R"))
    }
    smallsetList <- apply_code(data)
    
    scores <-
      prepare_score_sheet(smallsetList = smallsetList, data = data)
    scores <- scores[, colSums(scores != 0) > 0]
    colours <- prepare_colour_sheet(smallsetList = smallsetList)

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
    
    myscoresT <- t(as.matrix(scores[,]))
    krow = t(rep(1, nrow(scores)))
    
    model <- list()
    
    model$A     <- rbind(myscoresT, krow)
    model$Q     <- D
    model$obj   <- rep(0, nrow(data))
    model$modelsense <- 'max'
    model$rhs   <- c(rep(1, nrow(myscoresT)), rowCount)
    model$sense <- c(rep('>', nrow(myscoresT)), '=')
    model$vtype <- rep('B', nrow(data))
    
    params <- list(OutputFlag = 0)
    
    result <- gurobi::gurobi(model, params)
    modelSelect <- result$x
    rowNums <- data.frame(rows = rownames(data), modelSelect)
    rowNums <- subset(rowNums, rowNums$modelSelect == 1)$rows
    
  }