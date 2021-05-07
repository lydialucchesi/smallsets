#' View smallset colour schemes
#'
#' @description  The function is for viewing built-in smallset colour schemes.
#' Schemes can be viewed together or individually.
#'
#' @param scheme One of "colScheme1", "colScheme2", "colScheme3", or "all".
#' @import "ggplot2"
#' @export

view_smallset_colours <- function(scheme = "all") {
  if (!(scheme %in% c("colScheme1", "colScheme2", "colScheme3", "all"))) {
    stop("Please chooose one of 'colScheme1', 'colScheme2', 'colScheme3', or 'all.")
  }
  
  schemes <- data.frame()
  labels1 <- data.frame()
  labels2 <- data.frame()
  for (i in 1:3) {
    cs <- return_scheme(paste0("colScheme", as.character(i)))
    
    cs <- as.data.frame(rbind(
      unlist(cs$constant),
      unlist(cs$changed),
      unlist(cs$added),
      unlist(cs$deleted)
    ))
    
    cs <- cs[nrow(cs):1, ]
    cs$V2 <- as.numeric(as.character(cs$V2))
    if (i == 1) {
      cs$x <- rep(i, 4)
    } else {
      cs$x <- rep(i + i - 1, 4)
    }
    cs$y <- seq(1, 4, 1)
    
    if (i == 1) {
      numbers <- data.frame(
        x = rep(i - 1, 4),
        y = seq(1, 4, 1),
        n = c("4.", "3.", "2.", "1.")
      )
    } else if (i == 2) {
      numbers <- data.frame(
        x = rep(i, 4),
        y = seq(1, 4, 1),
        n = c("4.", "3.", "2.", "1.")
      )
    } else {
      numbers <- data.frame(
        x = rep(i + 1, 4),
        y = seq(1, 4, 1),
        n = c("4.", "3.", "2.", "1.")
      )
    }
    
    name <-
      data.frame(x = c(unique(cs$x)),
                 y = c(5),
                 label = c(paste0("colScheme", as.character(i))))
    
    schemes <- rbind(schemes, cs)
    labels1 <- rbind(labels1, numbers)
    labels2 <- rbind(labels2, name)
  }
  
  
  if (scheme != "all") {
    j <- as.numeric(gsub("colScheme", "", scheme))
    xSub <- (j + j) - 1
    sub1 <- subset(schemes, x == xSub)
    sub2 <- subset(labels1, x == (unique(sub1$x) - 1))
    
    p <- ggplot() + geom_tile(
      data = sub1,
      aes(
        x = x,
        y = y,
        fill = V1,
        colour = V1,
        alpha = V2
      ),
      height = 1,
      width = 1
    ) +
      scale_fill_identity() +
      scale_colour_identity() +
      scale_alpha_identity() +
      geom_text(data = sub2, aes(x = x, y = y, label = n), family = "sans") +
      geom_text(aes(
        x = xSub,
        y = 5,
        label = paste0("colScheme", j)
      ), family = "sans") +
      coord_equal(ratio = 1) +
      theme_void()
    
  } else {
    p <- ggplot() + geom_tile(
      data = schemes,
      aes(
        x = x,
        y = y,
        fill = V1,
        colour = V1,
        alpha = V2
      ),
      height = 1,
      width = 1
    ) +
      scale_fill_identity() +
      scale_colour_identity() +
      scale_alpha_identity() +
      geom_text(data = labels1, aes(x = x, y = y, label = n), family = "sans") +
      geom_text(data = labels2,
                aes(x = x, y = y, label = label),
                family = "sans") +
      coord_equal(ratio = 1) +
      theme_void()
  }
  
  
  return(p)
  
}
