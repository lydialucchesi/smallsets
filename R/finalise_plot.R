#' Finalise plot
#' @description Adds captions and standard dimensions to the plots.
#' @import ggplot2 ggtext
#' @keywords internal

finalise_plot <- function(itemNum,
                          plots,
                          output,
                          maxDims,
                          font,
                          sizing,
                          spacing) {
  # Retrieve plot caption
  snapshotCaption <- output[[1]]$text[itemNum]
  snapshotCaption[is.na(snapshotCaption)] <- ""
  
  finalPlot <- plots[[itemNum]] +
    geom_textbox(
      aes(x = .5,
          y = -.25,
          label = snapshotCaption),
      width = grid::unit(.9, "npc"),
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
  
  return(finalPlot)
}