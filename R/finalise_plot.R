#' Finalise plot
#' @description Adds captions and standard dimensions to the plots.
#' @import ggplot2 ggtext
#' @noRd

finalise_plot <- function(itemNum,
                          plots,
                          output,
                          maxDims,
                          align,
                          font,
                          sizing,
                          spacing) {
  # Retrieve plot caption
  snapshotCaption <- output[[1]]$text[itemNum]
  snapshotCaption[is.na(snapshotCaption)] <- ""
  
  if (align == "vertical") {
    
    tw <- (maxDims[1] + spacing$right + 1.5 + (spacing$captions)) - .5
    w <- tw - (maxDims[1] + spacing$right + 1.5)
    wp <- w / tw
    
    finalPlot <- plots[[itemNum]] +
      geom_textbox(
        aes(
          x = (maxDims[1] + spacing$right + 1.5),
          y = (maxDims[2] + .5),
          label = snapshotCaption
        ),
        height = unit(.80, "npc"),
        width = unit(wp, "npc"),
        box.padding = unit(0, "cm"),
        family = font,
        hjust = 0,
        vjust = 1,
        size = sizing$captions,
        box.colour = NA,
        colour = "black"
      ) +
      xlim(c(.5, maxDims[1] + spacing$right + 1.5 + (spacing$captions))) +
      ylim(c(.5, maxDims[2] + spacing$header))
    
  } else {
    
    th <- maxDims[2] + spacing$header + spacing$captions
    h <- spacing$captions - .25
    hp <- h / th
    
    finalPlot <- plots[[itemNum]] +
      geom_textbox(
        aes(
          x = .5,
          y = -.25,
          label = snapshotCaption
        ),
        width = grid::unit(.9, "npc"),
        height = grid::unit(hp, "npc"),
        box.padding = unit(0, "cm"),
        family = font,
        hjust = c(0),
        vjust = c(1),
        size = sizing$captions,
        box.colour = NA,
        colour = "black"
      ) +
      ylim(c(spacing$captions * (-1), maxDims[2] + spacing$header)) +
      xlim(c(.5, maxDims[1] + spacing$right))
  }
  
  return(finalPlot)
}