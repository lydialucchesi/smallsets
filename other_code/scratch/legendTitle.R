# if (isTRUE(highlightNA)) {
#   abstractWithCaption <- abstractWithCaption +
#     guides(
#       fill = guide_legend(title = "A lighter colour value signals a missing data value.", 
#                           title.position = "bottom",
#                           title.theme = element_text(size = 5),
#                           family = timelineFont
#                           )
#     )
# }

if (isTRUE(highlightNA)) {
  abstractWithCaption <- abstractWithCaption +
    labs(
      fill = "A lighter colour value signals a missing data value."
    )
}



if (isTRUE(highlightNA)) {
  abstractWithCaption <- abstractWithCaption +
    guides(
      fill = guide_legend(title = "A lighter colour value signals a missing data value.",
                          title.position = "bottom",
                          title.theme = element_text(size = 5),
                          family = timelineFont
      )
    )
}

# if (isTRUE(highlightNA)) {
#   abstractWithCaption <- abstractWithCaption +
#     labs(
#       fill = "A lighter colour value signals a missing data value."
#       )
# }


if (isTRUE(highlightNA)) {
  fontChoice <-
    paste0(
      " & theme(text = element_text(family = '",
      timelineFont,
      "', colour = otherTextColour), 
          legend.position = 'bottom', 
          legend.title = element_text(family = '",
      timelineFont,
      "', colour = otherTextColour), 
          legend.margin = margin(t=0, r=0, b=0, l=0, unit='cm'))"
    )
} else {
  fontChoice <-
    paste0(
      " & theme(text = element_text(family = '",
      timelineFont,
      "', colour = otherTextColour), legend.position = 'bottom', legend.title=element_blank(), legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'))"
    )
}